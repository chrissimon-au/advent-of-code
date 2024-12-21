{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.pascal.enable = true;

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "watchexec -e txt,pas --timings 'rm day21 ; fpc day21.pas -Fu../../fptest/src -Fu../../epiktimer/; ./day21'";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
