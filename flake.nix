{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      ghcWithPkgs = pkgs.haskellPackages.ghcWithPackages
        (pkgs': with pkgs'; [ brick random-shuffle tf-random timers ]);
    in {
      defaultPackage.${system} = pkgs.mkShell {
        name = "nixshell";
        buildInputs = [ ghcWithPkgs ];
      };
    };
}
