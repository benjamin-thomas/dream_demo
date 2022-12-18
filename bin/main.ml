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
    {|
<h1>Hello!</h1>

<h2>Using session cookies here</h2>

<ul>
  <li><a href="/echo/John">Echo normal (John)</a></li>
  <li><a href="/echo2/Bob">Echo via template (Bob)</a></li>
  <li><a href="/static/css/main.css">A static file (main.css)</a></li>
  <li><a href="/comments">List and post comments</a></li>
  <li><a href="/login">Login (sets a fake session user)</a></li>
  <li><a href="/set-lang">Set language (sets a cookie value)</a></li>
  <li><a href="/show-upload">Upload files (will print their file size)</a></li>
  <li><a href="/bad">Return a bad request (HTTP 400 Bad Request)</a></li>
  <li><a href="/fail">Raise an exception (HTTP 500 Internal Server Error)</a></li>
</ul>

<hr>

<p>
A JSON POST endpoint can be reached that way:

<pre>
echo '{"message": "hello"}' | http POST :8080/json Origin:http://localhost:8080 Host:localhost:8080
</pre>
</p>
|}

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

module type DB = Caqti_lwt.CONNECTION

module R = Caqti_request
module T = Caqti_type
open Caqti_request.Infix

let list_comments =
  let query =
    ( ->* ) T.unit T.(tup2 int string) "SELECT id, text FROM comment"
  in
  fun (module Db : DB) ->
    let%lwt comments_or_error = Db.collect_list query () in
    Caqti_lwt.or_fail comments_or_error

let add_comment =
  let query =
    (T.string ->. T.unit) @@ "INSERT INTO comment (text) VALUES ($1)"
  in
  fun text (module Db : DB) ->
    let%lwt unit_or_error = Db.exec query text in
    Caqti_lwt.or_fail unit_or_error

let get_comments request =
  let%lwt comments = Dream.sql request list_comments in
  Dream.html (Comments.render comments request)

let post_comment request =
  match%lwt Dream.form request with
  | `Ok [ ("text", text) ] ->
      let%lwt () = Dream.sql request (add_comment text) in
      Dream.redirect request "/comments"
  | _ -> Dream.empty `Bad_Request

let () =
  run ~error_handler:debug_error_handler
  @@ logger
  (* @@ memory_sessions *)
  @@ sql_pool "sqlite3:db.sqlite"
  (* @@ sql_sessions *)
  (* DELETE FROM dream_session; *)
  @@ cookie_sessions
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
         (* Comments *)
         get "/comments" get_comments;
         post "/post-comment" post_comment;
         (* Error handler examples *)
         get "/bad" (fun _ -> empty `Bad_Request);
         get "/fail" (fun _ -> raise (Failure "The Web app failed!"));
         (* Live reload *)
         Dream_livereload.route ();
       ]
