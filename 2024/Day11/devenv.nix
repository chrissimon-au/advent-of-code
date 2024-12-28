{ pkgs, lib, config, inputs, ... }:

{
  env.LANG="en-AU.UTF-8";
  
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
  ];

  # https://devenv.sh/languages/
  languages.haskell.enable = true;  

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  processes.test-wait.exec = "cd AoC-Day11; watchexec -e hs --timings 'stack test'";

  cachix.enable = false;
}
