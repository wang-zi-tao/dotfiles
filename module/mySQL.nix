{ pkgs, config, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.MySQL.enable {
    containers = {
      mysql = {
        autoStart = true;
        forwardPorts = [{
          containerPort = 3306;
          hostPort = 3306;
          protocal = "tcp";
        }];
        bindMounts = {
          "/var/lib/mysql" = {
            hostPath = "/srv/mysql";
            isReadOnly = false;
          };
          "/var/backup/mysql" = {
            hostPath = "/srv/backup-mysql";
            isReadOnly = false;
          };
          "/run/mysqld" = {
            hostPath = "/srv/mysql";
            isReadOnly = false;
          };
        };
        config = { config, pkgs, ... }: {
          boot.isContainer = true;
          services.mysql = {
            enable = true;
            # Okq4ikoZVsxif1Q55GZauhASJQtEA1mS
            package = pkgs.mysql57;
            settings = {
              client = { default-character-set = "utf8mb4"; };
              mysql = { default-character-set = "utf8mb4"; };
              mysqld = {
                character-set-client-handshake = "FALSE";
                character-set-server = "utf8mb4";
                collation-server = "utf8mb4_unicode_ci";
                init_connect = "'SET NAMES utf8mb4'";
              };
            };
            ensureDatabases = [ "nextcloud" "mydb" "onedev" ];
            ensureUsers = [
              {
                name = "nextcloud"; # jn4TjfrGOyKmjyWn
                ensurePermissions = { "nextcloud.*" = "ALL PRIVILEGES"; };
              }
              {
                name = "onedev"; # d1Ycb85VU6Q1iT41
                ensurePermissions = { "onedev.*" = "ALL PRIVILEGES"; };
              }
              {
                name = "backup";
                ensurePermissions = { "*.*" = "SELECT, LOCK TABLES"; };
              }
            ];
          };
          services.mysqlBackup = {
            enable = true;
            databases = [ "nextcloud" "mydb" "onedev" ];
          };
        };
      };
    };
  };
}
