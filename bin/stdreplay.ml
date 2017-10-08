module Cli = struct
  open Cmdliner

  let record =
    let run log_file prog args =
      let prog =
        match prog with
        | Some x -> x
        | None -> (Unix.getpwuid (Unix.getuid ())).pw_shell
      in
      Record.record ~log_file ~prog ~args
    in
    ( Term.(const run
            $ Arg.(value
                   & opt string "log"
                   & info ["o"; "output"]
                     ~docv:"LOG-FILE"
                     ~doc:"Output file where to write data captured from stdin.")
            $ Arg.(value
                   & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
            $ Arg.(value
                   & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")))
    , Term.info "record"
        ~doc:"Run a command recording everything it receives on stdin"
        ~man:[ `S "DESCRIPTION"
             ; `P {|$(b,stdreplay record) $(b,command) runs $(b,command) and
                    record everything that comes from stdin to a log file.|}
             ]
    )

  let replay =
    ( Term.(const Replay.replay
            $ Arg.(value
                   & pos 0 string "log" (Arg.info [] ~docv:"FILE")))
    , Term.info "replay"
        ~doc:"Run a command and replay previously captured stdin data"
        ~man:[ `S "DESCRIPTION"
             ; `P {|$(b,stdreplay record) $(b,command) runs $(b,command) and
                    send on its stdin everything that was previously captured..|}
             ]
    )

  let all =
    [ record
    ; replay
    ]

  let default =
    ( Term.(ret (const (`Help (`Pager, None))))
    , Term.info "stdreplay"
        ~doc:"stdin record & replay"
        ~version:"%%VERSION%%"
        ~man:
          [ `S "DESCRIPTION"
          ; `P {|$(b,stdreplay) is a tool for recording stdin and replaying it
                 step by step. It's purpose is to create replayable sessions for
                 creating video turorials.|}
          ]
    )

  let run () = ignore (Term.eval_choice default all : _ Term.result)
end

let () = Cli.run ()
