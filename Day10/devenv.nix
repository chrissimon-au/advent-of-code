{ pkgs, lib, config, inputs, ... }:

let
  db_name = "day10";
in
{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
  ];

  # https://devenv.sh/processes/
  processes.code.exec = "code .";

  # https://devenv.sh/services/
  services.postgres = {
    enable = true;
    initialDatabases = [{ name = db_name; }];
    extensions = extensions: [
      extensions.pgtap      
    ];    
  };

  processes.setup-pgtap = {
    exec = "psql -c 'CREATE EXTENSION IF NOT EXISTS pgtap;' -d ${db_name}";
    process-compose.depends_on.postgres.condition = "process_healthy";
  };

  processes.watch-tests = {
    exec = "watchexec -F ${db_name}.sql --timings 'psql -f ${db_name}.sql -d ${db_name}'";
    process-compose.depends_on.postgres.condition = "process_healthy";
    process-compose.depends_on.setup-pgtap.condition = "process_completed_successfully";
  };

  cachix.enable = false;
}
