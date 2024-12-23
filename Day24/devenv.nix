{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    # pkgs.watchexec
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.gleam.enable = true;  

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  # process.managers.process-compose.settings = {
  #   processes = {
  #     test-wait = {
  #       command = "watchexec -e txt './command.sh'";
  #       is_tty = true;
  #     };
  #   };
  # };

  cachix.enable = false;
}
