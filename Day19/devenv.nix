{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.shunit2
    pkgs.picat
  ];

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "watchexec -e pi,txt './day19.test.sh'";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
