let setup_term fd =
  let attr = Unix.tcgetattr fd in
  at_exit (fun () -> try Unix.tcsetattr fd Unix.TCSAFLUSH attr with _ -> ());
  Unix.tcsetattr fd Unix.TCSAFLUSH
    { attr with
      Unix.c_echo   = false
    ; Unix.c_icanon = false
    ; Unix.c_isig   = false
    }

let record ~log_file:log_fn ~prog ~args =
  Spawn.spawn ~prog ~args ~mode:"recording" ~f:(fun pty ->
    setup_term Unix.stdin;
    Stdio.Out_channel.write_lines log_fn
      [String.escaped
         (String.concat "\000" (prog :: args))];
    Forward.spawn Unix.stdin pty ~log_file:log_fn;
    Forward.spawn pty Unix.stdin;
    ())
