{ pkgs, lib, config, inputs, ... }:

{
  env.LANG="en_US.UTF-8";

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.perl.enable = true;  
  languages.perl.packages = [
    "Test::Simple"
    "JSON"
    "File::Slurp"
  ];

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  process.managers.process-compose.settings = {
    processes = {
      test-wait = {
        command = "watchexec -e pl,txt --timings 'perl day20.pl'";
        is_tty = true;
      };
    };
  };

  cachix.enable = false;
}
