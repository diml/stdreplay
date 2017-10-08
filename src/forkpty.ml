type child =
  { pid : int
  ; pty : Unix.file_descr
  }

type result =
  | In_the_child
  | In_the_parent of child

external forkpty : unit -> result = "stdreplay_forkpty"
