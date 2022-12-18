let render comments request =
  <html>
  <body>

    <p>
    Back to: <a href="/">the front page</a>
    </p>

    <h1>Post a comment!</h1>

%   comments |> List.iter (fun (_id, comment) ->
      <p><%s comment %></p><% ); %>

    <form method="POST" action="/post-comment" autocomplete="off">
      <%s! Dream.csrf_tag request %>
      <input name="text" autofocus>
    </form>

  </body>
  </html>