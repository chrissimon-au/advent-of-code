{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/languages/
  languages.javascript.enable = true;

  enterShell = ''
    npm i
  '';

  # https://devenv.sh/processes/
  processes.jest-watch.exec = "npm run watch";
  processes.code.exec = "code .";
  
  cachix.enable = false;
}
