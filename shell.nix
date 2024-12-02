let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.11";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShellNoCC {
  packages = with pkgs; [
    dotnetCorePackages.sdk_9_0
    nodejs_23
    watchexec
    shunit2
  ];
  buildInputs = with pkgs; [
    (callPackage ./rockstar/rockstar.nix {})
  ];
}
