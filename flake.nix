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
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
            cabal-install
            ghcid
          ]);
      };
      devShells.aarch64-darwin.default = pkgs.mkShell {
        inputsFrom = [ self.packages.aarch64-darwin.default.env ];
        packages = with pkgs; [ 
          haskell-language-server
        ];
      };
    };
}
