let render param =
  <html>
  <body>
    <h1>The URL parameter was <%s param %>!</h1>
  </body>
  </html>

let show_form ?message action request =
  <html>
  <body>

% (*let action = "/post-here" in*)

%   begin match message with
%   | None -> ()
%   | Some message ->
      <p>You entered: <b><%s message %>!</b></p>
%   end;

    <h1>CSRF protected form!</h1>
    <h2>Will send POST request to: <%s action %></h2>

    <form method="POST" action=<%s action %> autocomplete="off">
      <%s! Dream.csrf_tag request %>
      <input name="message" autofocus>
    </form>

  </body>
  </html>

let show_upload request =
  <html>
  <body>
    <form method="POST" action="/post-upload" enctype="multipart/form-data">
      <%s! Dream.csrf_tag request %>
      <input name="files" type="file" multiple>
      <button>Submit!</button>
    </form>
  </body>
  </html>

let got_upload files =
  <html>
  <body>
%   files |> List.iter begin fun (name, content) ->
%     let name =
%       match name with
%       | None -> "None"
%       | Some name -> name
%     in
      <p><%s name %>, <%i String.length content %> bytes</p>
%   end;
  </body>
  </html>
