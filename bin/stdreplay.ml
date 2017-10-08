external sys_exit : int -> _ = "caml_sys_exit"

let in_the_child () =
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  try
    Unix.execvp argv.(0) argv
  with _ ->
    sys_exit 127

let forward (fd1, fd2) =
  let buf = Bytes.create 1 in
  let rec loop () =
    if Unix.read fd1 buf 0 1 = 1 then begin
      if Unix.write fd1 buf 0 1 = 1 then
        loop ()
    end
  in
  loop ()

let main () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "Usage: stdreplay <prog> <args>...";
    exit 2
  end;
  match Forkpty.forkpty () with
  | In_the_child -> in_the_child ()
  | In_the_parent child ->
    let _th1 : Thread.t = Thread.create forward (Unix.stdin, child.pty) in
    let _th2 : Thread.t = Thread.create forward (child.pty, Unix.stdin) in
    match snd (Unix.waitpid [] child.pid) with
    | WEXITED n -> exit n
    | WSIGNALED n -> exit 255
    | WSTOPPED _ -> assert false
