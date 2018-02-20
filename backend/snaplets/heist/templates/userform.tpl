<form class="ui large form" method="POST" action="${postAction}">
  <div class="ui stacked segment">
    <div class="field">
      <div class="ui left icon input">
        <i class="user icon"></i>
        <input type="text" name="login" placeholder="Username" required autofocus>
      </div>
    </div>
    <div class="field">
      <div class="ui left icon input">
        <i class="lock icon"></i>
        <input type="password" name="password" placeholder="Password" required>
      </div>
    </div>
    <button class="ui fluid large submit button" type="submit"><submitText/></button>
  </div>
</form>
