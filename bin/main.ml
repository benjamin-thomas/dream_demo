(*
`entr` doesn't play nice with dune here.
./manage/dev/run
*)
let () =
  Dream.run @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Hello!");
         Dream.get "/echo/:word" (fun request ->
             Dream.html (Dream.param request "word"));
         Dream_livereload.route ();
       ]
