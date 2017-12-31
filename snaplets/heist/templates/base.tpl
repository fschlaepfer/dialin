<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0">

    <script src="/app.js"></script>
    <link rel="stylesheet" type="text/css" href="/app.css">

    <title>Dial-In</title>
  </head>
  <body>
    <div class="ui text container">
        <ifLoggedIn>
            <div class="ui four item menu">
                <a class="item" href="/new">
                    Pull shot
                </a>
                <a class="item" href="/recent">
                    My shots
                </a>
                <a class="item" href="/new_bean">
                    Add coffee
                </a>
                <a class="item" href="/logout">
                    <i>Logout</i>
                </a>
            </div> 
        </ifLoggedIn>

        <apply-content/>
    </div>
  </body>
</html>
