let p b f =
  if b
  then ignore
  else
    fun s ->
    Printf.printf f s;
    Stdio.print_string "\n"
;;
