{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.inotify-tools
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.gleam.enable = true;  

  # https://devenv.sh/processes/
  processes.code.exec = "code .";

  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "gleam test -- --glacier";
        is_tty = true;
        working_dir = "day24";
      };
    };
  };

  cachix.enable = false;
}
