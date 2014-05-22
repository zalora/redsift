{ config, pkgs, lib, ... }:

let

  inherit (lib) mkOption mkIf types;

  cfg = config.services.redsift;

  configFile = pkgs.writeText "redsift.conf" ''
    app {
      port = ${toString cfg.port}
      rowLimit = ${toString cfg.rowLimit}
    }

    db {
      import "${cfg.redcatCredentialsFile}"
    }

    s3 {
      bucket = "${cfg.s3.bucket}"
      expiry = ${toString cfg.s3.expiry}
      import "${cfg.s3.credentialsFile}"
    }

    email {
      sender = "${cfg.emailSenderAddress}"
    }
  '';

in {
  imports = [
    <zalora-nix-lib/daemon-users-module.nix>
    <zalora-nix-lib/mail-relay.nix>
  ];

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

      port = mkOption {
        type = types.int;
        default = 9000;
        description = "The port to listen on";
      };

      rowLimit = mkOption {
        type = types.int;
        default = 100;
        description = "Row limit setting";
      };

      redcatCredentialsFile = mkOption {
        type = types.string;
        description = ''
          The path to a file containing connection details to the
          Redcat DB.
        '';
      };

      emailSenderAddress = mkOption {
        type = types.string;
        description = ''
          The sender address of the emails sent from redsift.
        '';
      };

      s3 = {
        bucket = mkOption {
          type = types.str;
          description = "The S3 bucket that redsift should use";
        };

        expiry = mkOption {
          type = types.int;
          description = "S3 expiry";
        };

        credentialsFile = mkOption {
          type = types.str;
          description = ''
            The path to a file containing S3 credentials.
          '';
        };
      };
    };
  };

  config = mkIf cfg.enable {
    users.extraUsers.redsift = {
      description = "redsift daemon user";
      uid = config.zalora.daemonUids.redsift;
    };

    zalora.smtpRelay.allowedSenders = [ cfg.emailSenderAddress ];

    systemd.services.redsift = {
      description = "redsift web application";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/redsift --config=${configFile}";
        Restart = "on-failure";
        User = "redsift";
      };
    };
  };
}
