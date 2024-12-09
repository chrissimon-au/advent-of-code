{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/languages/
  languages.dotnet.enable = true;
  languages.dotnet.package = pkgs.dotnetCorePackages.sdk_9_0;

  # https://devenv.sh/processes/
  # https://devenv.sh/processes/
  processes.code.exec = "rider AoC.Day1.sln";
  
  cachix.enable = false;
}
