{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.shunit2
    (pkgs.callPackage ./rockstar/rockstar.nix {})
  ];

  # https://devenv.sh/processes/
  processes.shunit2-watch.exec = "watchexec -e rock,sh,txt --timings ./aoc-day3.test.sh";
  processes.code.exec = "code .";
  
  cachix.enable = false;
}
