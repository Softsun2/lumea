{
  description = "CmaeBrea Static Org-file Blog";

  inputs = { nixpkgs.url = github:nixos/nixpkgs/nixos-23.05; };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      cabalProjectName = "lumea";
    in {
      packages.aarch64-darwin.default = pkgs.haskellPackages.developPackage {
        root = ./.;
      };
      devShells.aarch64-darwin.default = pkgs.mkShell {
        inputsFrom = [ self.packages.aarch64-darwin.default ];
        packages = with pkgs; [ 
          haskell-language-server
          ghcid
        ];
      };
    };
}
