{ pkgs, lib, config, inputs, extensions, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/processes/
  processes.code.exec = "code .";

  # https://devenv.sh/services/
  services.postgres.enable = true;
  services.postgres.extensions = extensions: [
    extensions.pgtap
  ];
  services.postgres.initialDatabases = [
    { name = "day10"; }
  ];
  services.postgres.initialScript = ''
    CREATE EXTENSION IF NOT EXISTS pgtap;
  '';
  cachix.enable = false;
}
