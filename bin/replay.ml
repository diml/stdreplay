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

let load fn =
  let lines = In_channel.read_lines fn in
  List.map lines ~f:explode
  |> List.filter ~f:(function
    | [] -> false
    | _ -> true)

let setup_term fd =
  let attr = Unix.tcgetattr fd in
  Caml.at_exit (fun () -> try Unix.tcsetattr fd Unix.TCSAFLUSH attr with _ -> ());
  Unix.tcsetattr fd Unix.TCSAFLUSH
    { attr with
      Unix.c_echo   = false
    ; Unix.c_icanon = false
    ; Unix.c_isig   = false
    }

let replay log_fn prog args =
  let blocks = load log_fn in
  if false then
    List.iter blocks ~f:(fun keys ->
      List.iter keys ~f:(Caml.Printf.printf " %S");
      Caml.Printf.printf "\n%!");
  let buf = String.create 256 in
  Spawn.spawn ~prog ~args ~mode:"replaying" ~f:(fun pty ->
    setup_term Unix.stdin;
    Forward.spawn pty Unix.stdin;
    List.iter blocks ~f:(fun keys ->
      ignore (Unix.read Unix.stdin buf 0 (String.length buf) : int);
      List.iter keys ~f:(fun s ->
        let len = String.length s in
        assert (Unix.write pty s 0 len = len);
        Unix.sleepf 0.05)))
