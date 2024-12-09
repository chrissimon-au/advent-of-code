{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/languages/
  languages.kotlin.enable = true;

  processes.code.exec = "idea AoC-Day07";
  
  cachix.enable = false;
}
