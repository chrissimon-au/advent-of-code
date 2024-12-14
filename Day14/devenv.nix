{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.catch2_3
  ];

  # https://devenv.sh/languages/
  languages.cplusplus.enable = true;  

  # https://devenv.sh/processes/
  processes.code.exec = "code .;";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "watchexec -e cxx --timings 'cd build; ./build.sh'";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
