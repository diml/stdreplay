let add_to_log fn c =
  let fd = Unix.openfile fn [O_WRONLY; O_APPEND; O_CREAT] 0o666 in
  let s =
    if c = '\n' then
      "\n\\n\n"
    else
      Char.escaped c
  in
  let len = String.length s in
  assert (Unix.write fd s 0 len = len);
  Unix.close fd

let forward (fd1, fd2, log) =
  let buf = Bytes.create 1 in
  let rec loop () =
    if Unix.read fd1 buf 0 1 = 1 then begin
      (match log with
       | None -> ()
       | Some fn -> add_to_log fn buf.[0]);
      if Unix.write fd2 buf 0 1 = 1 then
        loop ()
    end
  in
  loop ()

let spawn ?log_file fd1 fd2 =
  ignore
    (Thread.create forward (fd1, fd2, log_file)
     : Thread.t)
