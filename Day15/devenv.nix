{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.ruby.enable = true;

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "watchexec -e rb,txt 'cd src; ruby day15.rb'";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
