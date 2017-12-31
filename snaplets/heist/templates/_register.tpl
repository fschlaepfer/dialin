<style type="text/css">
  body {
    background-color: #DADADA;
  }
  body > .grid {
    height: 100%;
  }
  .image {
    /*margin-top: -100px;*/
  }
  .column {
    max-width: 450px;
  }
</style>

<div class="ui middle aligned center aligned grid">
  <div class="column">
    <h2 class="ui teal image header">
      <div class="content">
        Sign up
      </div>
    </h2>

    <bind tag="postAction">/register</bind>
    <bind tag="submitText">Sign up</bind>
    <apply template="userform"/>
  </div>
</div>
