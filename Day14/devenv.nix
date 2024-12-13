{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.watchexec
    pkgs.catch2
    # pkgs.shunit2
  ];

  # https://devenv.sh/languages/
  languages.cplusplus.enable = true;  

  # https://devenv.sh/processes/
  processes.code.exec = "code .";
  # processes.test-wait.exec = "waitexec -e txt ''";

  cachix.enable = false;
}
