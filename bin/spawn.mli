val spawn
  :  prog:string
  -> args:string list
  -> mode:string
  -> f:(Unix.file_descr -> unit) -> unit
