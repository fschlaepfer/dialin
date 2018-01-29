with import ./dialin.nix;
let
  static = ./backend/static;
  snaplets = ./backend/snaplets;
in
reflex.nixpkgs.runCommand "dialin-driver" {} ''
    mkdir -p $out

    ln -s ${ghcjs.frontend} $out/frontend
    ln -s ${ghc.backend} $out/backend

    # copy app resources
    # TODO: cp *and* ln static?
    cp -r --no-preserve=mode ${static}   "$out/static"
    cp -r --no-preserve=mode ${snaplets} "$out/snaplets"
    ln -s ${static} "$out/static"

    # ghcjs artifacts
    ln -s "${ghcjs.frontend}/bin/frontend.jsexe/rts.js" "$out/static"
    ln -s "${ghcjs.frontend}/bin/frontend.jsexe/lib.js" "$out/static"
    #cat "${ghcjs.frontend}/bin/frontend.jsexe/out.js" "${ghcjs.frontend}/bin/frontend.jsexe/runmain.js" > "$out/static/dialin.js"
    ln -s "${ghcjs.frontend}/bin/frontend.jsexe/out.js" "$out/static"
    ln -s "${ghcjs.frontend}/bin/frontend.jsexe/runmain.js" "$out/static"

    # backend server
    mkdir -p "$out/bin"
    ln -s "${ghc.backend}/bin/backend" "$out/bin"
''
