<apply template="base">

  <ifLoggedIn>
    <!--<div id="dialin-app" class="full-height">
    </div>-->

    <h2>Welcome, <i><loggedInUser/></i>!</h2>

    <!--<ignore>
    <apply template="mainContentTemplate"/>
    </ignore>-->
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
