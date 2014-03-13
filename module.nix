{ config, pkgs, ... }:
let
  inherit (pkgs.lib) mkOption mkIf types;

  cfg = config.services.redsift;
in {
  options = {
    services.redsift = {
      enable = mkOption {
        default = false;

        type = types.bool;

        description = "Enable redsift";
      };

      package = mkOption {
        type = types.package;

        description = "The built redsift package";
      };
    };
  };

  config = mkIf cfg.enable {
    users.extraUsers.redsift = {
      description = "redsift daemon user";
      uid = 1010;
      home = "/home/redsift";
      createHome = true;
    };

    systemd.services.redsift = {
      description = "redsift web application";

      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "idle";
        PIDFile = "/run/redsift.pid";
        ExecStart = "${cfg.package}/bin/redsift";
        Restart = "on-failure";
        User = "redsift";
        WorkingDirectory = "${config.users.extraUsers.redsift.home}";
      };
    };
  };
}
