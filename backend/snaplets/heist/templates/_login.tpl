<style type="text/css">
  body {
    background-color: #DADADA;
  }
  body > .grid {
    height: 100%;
  }
  .image {
    margin-top: -100px;
  }
  .column {
    max-width: 450px;
  }
</style>
  
<div class="ui middle aligned center aligned grid">
  <div class="column">
     <p>
        <loginError />
     </p>

    <bind tag="postAction">/login</bind>
    <bind tag="submitText">Login</bind>
    <apply template="userform"/>

    <div class="ui message">
      Don't have a login yet?  <a href="/register">Sign Up</a>
    </div>
  </div>
</div>
