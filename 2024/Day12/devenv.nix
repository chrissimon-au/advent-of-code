{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    # pkgs.watchexec
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.clojure.enable = true;  

  # https://devenv.sh/processes/
  processes.code.exec = "code ./aoc-day12";
  processes.test-wait.exec = "cd aoc-day12; clj -X:test:watch";

  cachix.enable = false;
}
