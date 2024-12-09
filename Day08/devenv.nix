{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/languages/
  languages.scala.enable = true;
  languages.scala.sbt.enable = true;

  # https://devenv.sh/processes/
  processes.sbt-wait.exec = "cd aoc-day08; sbt ~test";
  processes.code.exec = "code aoc-day08";
  
  cachix.enable = false;
}
