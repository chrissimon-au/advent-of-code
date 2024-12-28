{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.picat
  ];

  languages.dotnet.enable=true;

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "cd csharp; dotnet watch test";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
