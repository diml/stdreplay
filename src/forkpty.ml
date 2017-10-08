type result =
  | In_the_child
  | In_the_parent of { pid : int; pty : Unix.file_descr }

external forkpty : unit -> result = "stdreplay_forkpty"
