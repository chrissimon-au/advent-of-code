{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ 
    pkgs.git
    pkgs.poetry
  ];

  # https://devenv.sh/languages/
  languages.python.enable = true;
  languages.python.directory = "aoc-day04";

  enterShell = '' 
    cd aoc-day04 
    poetry install
  '';

  # https://devenv.sh/processes/
  processes.poetry-watch.exec = "cd aoc-day04; poetry run ptw";
  processes.code.exec = "code .";
  
  cachix.enable = false;
}
