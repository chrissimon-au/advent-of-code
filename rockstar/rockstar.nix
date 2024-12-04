# rockstar.nix
{
  stdenv, lib,
  fetchzip,
  pkgs
}:

stdenv.mkDerivation rec {
  pname = "rockstar";
  version = "2.0.2";
  platform = if builtins.match ".*darwin.*" pkgs.system != null then
              "macos"
             else
              "linux";

  src = fetchzip {
    url = "https://github.com/RockstarLang/rockstar/releases/download/v${version}/rockstar-v${version}-${platform}-x64.tar.gz";
    sha256 = if platform == "macos" then
              "Kc9dDJsqWFDunb5jTmvtGOLRPX9HIdU0vizESYpddz8="
             else
              "GCPam6lxcYhZfT/QQwyz/2MJbhaActgUrvYrMl6KdrY=";
  };

  dontBuild = true;
  dontConfigure = true;

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp -R source/* $out/bin
    runHook postInstall
  '';

  meta = with lib; {
    homepage = "";
    description = "Rockstar interpreter";
    platforms = [ "x86_64-linux" "aarch64-darwin" ];
  };
}
