{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/languages/
  languages.java.enable = true;
  languages.java.gradle.enable = true;
  languages.java.jdk.package = pkgs.jdk23;

  # https://devenv.sh/processes/
  processes.idea.exec = "idea AoC-Day06";
  
  cachix.enable = false;
}
