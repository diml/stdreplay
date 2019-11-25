let add_to_log fn buf n =
  let fd = Unix.openfile fn [O_WRONLY; O_APPEND; O_CREAT] 0o666 in
  for i = 0 to n - 1 do
    let c = Bytes.get buf i in
    let s =
      if c = '\n' then
        "\n\\n\n"
      else
        Char.escaped c
    in
    let len = String.length s in
    assert (Unix.write_substring fd s 0 len = len);
  done;
  Unix.close fd

let forward (fd1, fd2, log) =
  let buf = Bytes.create 1024 in
  let rec loop () =
    let n = Unix.read fd1 buf 0 (Bytes.length buf) in
    if n > 0 then begin
      (match log with
       | None -> ()
       | Some fn -> add_to_log fn buf n);
      let p = ref 0 in
      while n - !p > 0 do
        p := !p + Unix.write fd2 buf !p (n - !p)
      done;
      loop ()
    end
  in
  loop ()

let spawn ?log_file fd1 fd2 =
  ignore
    (Thread.create forward (fd1, fd2, log_file)
     : Thread.t)
