let () =
  let cfg =
    let ic = open_in Sys.argv.(1) in
    let rec loop acc =
      match input_line ic with
      | exception End_of_file -> close_in ic; acc
      | x -> Scanf.sscanf x "%[^:]: %s" (fun k v -> loop ((k, v) :: acc))
    in
    loop []
  in
  print_endline
    (match List.assoc "system" cfg with
     | "linux" -> "(-lutil)"
     | _ -> "()")

