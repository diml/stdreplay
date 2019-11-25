external sys_exit : int -> _ = "caml_sys_exit"

let in_the_child ~prog ~args ~mode =
  let argv = Array.of_list (prog :: args) in
  Unix.putenv "STDREPLAY" mode;
  try
    Unix.execvp prog argv
  with _ ->
    sys_exit 127

let spawn ~prog ~args ~mode ~f =
  match Forkpty.forkpty () with
  | In_the_child -> in_the_child ~prog ~args ~mode
  | In_the_parent { pid; pty } ->
    f pty;
    match snd (Unix.waitpid [] pid) with
    | WEXITED   n -> exit n
    | WSIGNALED _ -> exit 255
    | WSTOPPED  _ -> assert false

