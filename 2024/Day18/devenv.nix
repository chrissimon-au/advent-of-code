{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.rustup
  ];

  # https://devenv.sh/languages/
  languages.rust.enable = true;  


  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "watchexec -e rs,txt 'cd day18; cargo test'";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
