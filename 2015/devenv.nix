{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.racket.enable = true;

  enterShell = "code .; watchexec -e data,rkt --timings 'raco test day$DAY/day$DAY.rkt'; exit";

  cachix.enable = false;
}
