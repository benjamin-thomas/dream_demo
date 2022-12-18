(*
`entr` doesn't play nice with dune here.
./manage/dev/run
*)

open Dream

let login request =
  match Dream.session_field request "user" with
  | None ->
      let%lwt () = Dream.invalidate_session request in
      let%lwt () = Dream.set_session_field request "user" "alice" in
      Dream.html "You weren't logged in; but now you are!"
  | Some username ->
      Printf.ksprintf Dream.html "Welcome back, %s!"
        (Dream.html_escape username)

let set_lang req =
  match cookie req "ui.language" with
  | Some value ->
      Printf.ksprintf html "Your preferred language is %s!" (html_escape value)
  | None ->
      let resp = response "Set language preference; come again!" in
      add_header resp "Content-Type" text_html;
      set_cookie resp req "ui.language" "fr-FR";
      Lwt.return resp

let get_echo req = html (param req "word")
let get_echo2 req = param req "word" |> Template.render |> html

let get_root _ =
  html
    "<h1>Hello!</h1><p>Go to the main (static) CSS file <a \
     href=\"/static/css/main.css\">HERE</a></p>"

let show_form action req = html (Template.show_form action req)

let post_form action req =
  match%lwt form req with
  | `Ok [ ("message", message) ] ->
      (* I should rather do a redirect here *)
      html (Template.show_form ~message action req)
  | _ -> empty `Bad_Request

type message_object = { message : string } [@@deriving yojson]

(* echo '{"message": "hello"}' | http POST :8080/json Origin:http://localhost:8080 Host:localhost:8080 *)
let post_json request =
  let%lwt body = body request in

  let message_object =
    body |> Yojson.Safe.from_string |> message_object_of_yojson
  in

  `String message_object.message |> Yojson.Safe.to_string |> json

let show_upload req = html (Template.show_upload req)

let post_upload req =
  match%lwt multipart req with
  | `Ok [ ("files", files) ] -> html (Template.got_upload files)
  | _ -> empty `Bad_Request

let () =
  run ~error_handler:debug_error_handler
  @@ logger @@ memory_sessions
  @@ Dream_livereload.inject_script ()
  @@ router
       [
         get "/" get_root;
         get "/echo/:word" get_echo;
         get "/echo2/:word" get_echo2;
         get "/login" login;
         get "/set-lang" set_lang;
         get "/form" (show_form "/form");
         post "/form" (post_form "/form");
         post "/json" post_json;
         get "show-upload" show_upload;
         post "post-upload" post_upload;
         get "/static/**" (Dream.static "static");
         (* Error handler examples *)
         get "/bad" (fun _ -> empty `Bad_Request);
         get "/fail" (fun _ -> raise (Failure "The Web app failed!"));
         Dream_livereload.route ();
       ]
