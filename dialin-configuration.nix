{
  network.description = "dialin";

  dialin =
    { config, pkgs, ... }: let
      dialin = import ./default.nix;
    in
    { networking.hostName = "dialin";

      nix.trustedBinaryCaches = [ "https://ryantrinkle.com:5443" ];

      networking.firewall.allowedTCPPorts = [ 22 80 3911 ];
      environment.systemPackages = with pkgs; [
        dialin
        rxvt_unicode.terminfo
        git
        tmux
      ];

      services.postgresql = {
          enable = true;
          package = pkgs.postgresql94;
          initialScript = builtins.toFile "init.sql" ''
              create user dialin with password 'dialin';
              create database dialin owner dialin;
              \connect dialin
              alter schema public owner to dialin
          '';
      };

      systemd.services.dialin = {
          description = "dialin web app";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          restartIfChanged = true;
          script = ''
            set -x
            export HOME="/var/lib/dialin"
            cd "$HOME"
            ln -sft . "${dialin}"/*
            mkdir -p log
            ./bin/backend -p 80 >> log/stdout.log 2>>log/stderr.log
          '';
          #serviceConfig = {
          #  #ExecStart = "{dialin}/ghc/backend/bin/backend";
          #  ExecStart = ''
          #      echo "in ExecStart"
          #      mkdir -p log
          #      echo "starting backend"
          #      ./bin/backend -p 80 >> log/stdout.log 2>>log/stderr.log
          #  '';
          #};
      };

      users.extraUsers.dialin = {
        group = "dialin";
        description = "dialin app user";
        home = "/var/lib/dialin";
        createHome = true;
        isSystemUser = true;
      };

      users.extraGroups.dialin = {
      };
    };
}
