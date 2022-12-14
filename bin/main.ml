
(*
find . -name "*.ml" | entr -rc bash -c 'dune exec --display=quiet bin/main.exe'
*)
let () =
  Dream.run (fun _ ->
    Dream.html "Good morning, world3!")