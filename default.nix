with import ./dialin.nix;
let
  static = ./backend/static;
  snaplets = ./backend/snaplets;
  ccjs = reflex.nixpkgs.closurecompiler;
in
reflex.nixpkgs.runCommand "dialin-driver" {} ''
    mkdir -p $out

    # TODO: get hold of pkgs.closurecompiler and pkgs.zopfli to minify and gzip resulting js file.

    ln -s ${ghcjs.frontend} $out/frontend
    ln -s ${ghc.backend} $out/backend

    # copy app resources
    cp -r --no-preserve=mode ${static}   "$out/static"
    cp -r --no-preserve=mode ${snaplets} "$out/snaplets"
    ln -s ${static} "$out/static"

    # ghcjs artifacts
    #ln -s "${ghcjs.frontend}/bin/frontend.jsexe/rts.js" "$out/static"
    #ln -s "${ghcjs.frontend}/bin/frontend.jsexe/lib.js" "$out/static"
    #ln -s "${ghcjs.frontend}/bin/frontend.jsexe/out.js" "$out/static"
    ### #cat "${ghcjs.frontend}/bin/frontend.jsexe/out.js" "${ghcjs.frontend}/bin/frontend.jsexe/runmain.js" > "$out/static/dialin.js"

    #           cat "${ghcjs.frontend}/bin/frontend.jsexe/rts.js" \
    #               "${ghcjs.frontend}/bin/frontend.jsexe/lib.js" \
    #               "${ghcjs.frontend}/bin/frontend.jsexe/out.js" \
    #               "${ghcjs.frontend}/bin/frontend.jsexe/runmain.js" \
    #               > "$out/static/all.max.js"

    #${ccjs}/bin/closure-compiler -W QUIET --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=node --externs="${ghcjs.frontend}/bin/frontend.jsexe/all.js.externs" "$out/static/all.max.js" > "$out/static/all.min.js"

    ${ccjs}/bin/closure-compiler \
                  "${ghcjs.frontend}/bin/frontend.jsexe/all.js" \
        --externs="${ghcjs.frontend}/bin/frontend.jsexe/all.js.externs" \
        --compilation_level=SIMPLE_OPTIMIZATIONS \
        -W QUIET \
      > "$out/static/all.min.js"

    #           ${ccjs}/bin/closure-compiler \
    #               -W QUIET -O ADVANCED --externs="${ghcjs.frontend}/bin/frontend.jsexe/all.js.externs" \
    #               "$out/static/all.max.js" \
    #               > "$out/static/all.min.js"

    #ln -s "${ghcjs.frontend}/bin/frontend.jsexe/runmain.js" "$out/static"
    ##${ccjs}/bin/closure-compiler \
    ##    -W QUIET -O ADVANCED --externs="${ghcjs.frontend}/bin/frontend.jsexe/all.js.externs" \
    ##    "${ghcjs.frontend}/bin/frontend.jsexe/runmain.js" \
    ##    > "$out/static/runmain.js"

    # backend server
    mkdir -p "$out/bin"
    ln -s "${ghc.backend}/bin/backend" "$out/bin"
''
