open Import

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

let wait fd1 fd2 =
  let buf = String.create 1 in
  let rec loop () =
    if Unix.read fd1 buf 0 1 = 1 then
      match buf.[0] with
      | '\024' -> Unix.close fd2; Caml.exit 0
      | ' ' -> ()
      | c -> assert (Unix.write fd2 buf 0 1 = 1); loop ()
  in
  loop ()


let delay_fast = None
let delay_slow = Some 0.05

let replay log_fn =
  let prog, args, blocks = load log_fn in
  (*  {[
       if false then
         List.iter blocks ~f:(fun keys ->
           List.iter keys ~f:(Caml.Printf.printf " %S");
           Caml.Printf.printf "\n%!");
     ]} *)
  Spawn.spawn ~prog ~args ~mode:"replaying" ~f:(fun pty ->
    setup_term Unix.stdin;
    Forward.spawn pty Unix.stdin;
    let delay = ref delay_slow in
    List.iter blocks ~f:(fun cmd ->
      match cmd with
      | Fast -> delay := delay_fast
      | Slow -> delay := delay_slow
      | Input keys ->
        wait Unix.stdin pty;
        List.iter keys ~f:(fun s ->
          let len = String.length s in
          assert (Unix.write pty s 0 len = len);
          Option.iter !delay ~f:Unix.sleepf)))
