{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
  ];

  # https://devenv.sh/languages/
  languages.c.enable = true; 
  languages.ruby.enable = true;

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  processes.ceedling-install.exec = "gem install ceedling;";
  processes.ceedling-fix = {
    exec = "./fix_ceedling.sh";
    process-compose.depends_on.ceedling-install.condition = "process_completed_successfully";
  };
  processes.watch-tests = {
    exec = "cd AoC-Day13; watchexec -w src -w test -w project.yml --timings 'ceedling test:all'";
    process-compose.depends_on.ceedling-fix.condition = "process_completed_successfully";
  };

  cachix.enable = false;
}
