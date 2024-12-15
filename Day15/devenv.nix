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
      test-wait= {
        command = "watchexec -e rb,txt 'ruby day15_part1_tests.rb && ruby day15_part2_tests.rb'";
        is_tty = true;
        working_dir = "src";
      };
    };
  };

  cachix.enable = false;
}
