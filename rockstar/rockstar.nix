# rockstar.nix
{
  stdenv, lib,
  fetchzip,
}:

stdenv.mkDerivation {
  pname = "rockstar";
  version = "2.0.0";

  src = fetchzip {
    url = "https://github.com/RockstarLang/rockstar/releases/download/v2.0.0/rockstar-2.0.0-linux-x64.tar.gz";
    sha256 = "GCPam6lxcYhZfT/QQwyz/2MJbhaActgUrvYrMl6KdrY=";
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
    platforms = platforms.linux;
  };
}
