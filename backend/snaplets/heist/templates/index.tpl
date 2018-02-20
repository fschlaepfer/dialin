<apply template="base">

  <ifLoggedIn>
    <div id="dialin-app" class="full-height">
    </div>

    <!--<script src="/rts.js"></script>
    <script src="/lib.js"></script>
    <script src="/out.js"></script>-->
    <!--<script src="/all.min.js" defer></script>-->
    <script src="/all.min.js"></script>
    <!--<script src="/runmain.js" defer></script>-->
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
