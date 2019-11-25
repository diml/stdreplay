open Import
open Float.O_dot

let decompose_block s =
  let rec seq acc = function
    | ('[' | 'O' as c) :: l ->
      seq1 (c :: acc) l
    | _ ->
      None
  and seq1 acc = function
    | ('0' .. '9' | ';' | '[' as c) :: l ->
      seq1 (c :: acc) l
    | ('\x00' .. '\x1f' | '\x80' .. '\xff') :: _ | [] ->
      None
    | c :: l ->
      Some (List.rev (c :: acc), l)
  and loop = function
    | '\x1b' as c :: ('\x1b' :: _ as l) -> begin
        match loop l with
        | [] -> [[c]]
        | x :: l -> (c :: x) :: l
      end
    | '\x1b' as c :: l -> begin
        match seq [c] l with
        | Some (seq, l) ->
          seq :: loop l
        | None ->
          match uchar l with
          | None -> [[c]]
          | Some (u, l) ->
            (c :: u) :: loop l
      end
    | l ->
      match uchar l with
      | None -> []
      | Some (u, l) -> u :: loop l
  and uchar = function
    | '\x00'..'\x7f' as c :: l -> Some ([c], l)
    | c :: l -> Some (trailing [c] l)
    | [] -> None
  and trailing acc = function
    | c :: l when Char.to_int c land 0b1100_0000 = 0b1000_0000 ->
      trailing (c :: acc) l
    | l -> (List.rev acc, l)
  in
  loop (String.to_list s)
  |> List.map ~f:String.of_char_list

let explode = function
  | "" -> []
  | s ->
    Caml.Scanf.unescaped s
    |> decompose_block

type command =
  | Input of string list
  | Fast
  | Slow

let load fn =
  let invalid () =
    Printf.ksprintf failwith "%s: invalid format" fn
  in
  match In_channel.read_lines fn with
  | [] -> invalid ()
  | spec :: lines ->
    let prog, args =
      match String.split spec ~on:'\000' with
      | [] -> invalid ()
      | prog :: args -> prog, args
    in
    let blocks =
      List.mapi lines ~f:(fun i line ->
        if String.is_prefix line ~prefix:"## " then
          Input []
        else match line with
          | "##fast" -> Fast
          | "##slow" -> Slow
          | _ ->
            try
              Input (explode line)
            with exn ->
              Caml.Printf.eprintf "%s:%d: %S\n" fn i line;
              Caml.Printf.eprintf "%s:%d: %s\n" fn i (Exn.to_string exn);
              Caml.exit 2)
      |> List.filter ~f:(function
        | Input [] -> false
        | _ -> true)
    in
    (prog, args, blocks)

let setup_term fd =
  let attr = Unix.tcgetattr fd in
  Caml.at_exit (fun () -> try Unix.tcsetattr fd Unix.TCSAFLUSH attr with _ -> ());
  Unix.tcsetattr fd Unix.TCSAFLUSH
    { attr with
      Unix.c_echo   = false
    ; Unix.c_icanon = false
    ; Unix.c_isig   = false
    }

type timings =
  | Save of string
  | Use  of float list ref

let wait fd1 fd2 ~start ~timings =
  let buf = Bytes.create 1 in
  let timeout =
    match timings with
    | Save _ -> None
    | Use r ->
      match !r with
      | []     -> None
      | x :: l ->
        r := l;
        Some (start +. x)
  in
  let rec loop timeout =
    let timed_out =
      match timeout with
      | None -> false
      | Some t ->
        let d = t -. (Unix.gettimeofday ()) in
        if Float.(<=) d 0. then
          true
        else
          match Unix.select [fd1] [] [] d with
          | [], [], [] -> true
          | _ -> false
    in
    if timed_out then
      ()
    else if Unix.read fd1 buf 0 1 = 1 then
      match Bytes.get buf 0 with
      | '\024' -> Unix.kill (Unix.getpid ()) Caml.Sys.sigint
      | ' ' -> ()
      | _ -> assert (Unix.write fd2 buf 0 1 = 1); loop timeout
    else
      ()
  in
  loop timeout;
  let t = Unix.gettimeofday () in
  (match timings with
   | Use _ -> ()
   | Save fn ->
     let oc = Out_channel.create ~append:true fn in
     Out_channel.fprintf oc "%f\n" (t -. start);
     Out_channel.close oc);
  t



let delay_fast = None
let delay_slow = Some 0.05

let rec find_free_timings_file n =
  let fn = Printf.sprintf "timings%d" n in
  if Caml.Sys.file_exists fn then
    find_free_timings_file (n + 1)
  else
    fn

let replay ~log_file:log_fn ~auto =
  let prog, args, blocks = load log_fn in
  (*  {[
       if false then
         List.iter blocks ~f:(fun keys ->
           List.iter keys ~f:(Caml.Printf.printf " %S");
           Caml.Printf.printf "\n%!");
     ]} *)
  let timings =
    match auto with
    | Some fn ->
      Use (ref
             (In_channel.read_lines fn
              |> List.map ~f:Float.of_string))
    | None ->
      let fn = find_free_timings_file 1 in
      Caml.close_out (Caml.open_out fn);
      Save fn
  in
  Spawn.spawn ~prog ~args ~mode:"replaying" ~f:(fun pty ->
    setup_term Unix.stdin;
    Forward.spawn pty Unix.stdin;
    let delay = ref delay_slow in
    let t = ref (Unix.gettimeofday ()) in
    List.iter blocks ~f:(fun cmd ->
      match cmd with
      | Fast -> delay := delay_fast
      | Slow -> delay := delay_slow
      | Input keys ->
        t := wait Unix.stdin pty ~timings ~start:!t;
        List.iter keys ~f:(fun s ->
          let len = String.length s in
          assert (Unix.write_substring pty s 0 len = len);
          Option.iter !delay ~f:Unix.sleepf)))
