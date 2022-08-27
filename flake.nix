{
  description = "htrace";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix/4b61024e55a1f9b0a26b2baf714002c09ebbfb28";
    haskell-language-server.url = "github:haskell/haskell-language-server";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    fourmolu = {
      url = "github:fourmolu/fourmolu?ref=v0.7.0.1";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ inputs.haskell-nix.overlay ];
        inherit (inputs.haskell-nix) config;
      };

      fourmoluFor = system: (((nixpkgsFor system).haskell-nix.cabalProject' {
        compiler-nix-name = "ghc921";
        src = "${inputs.fourmolu}";
      }).getPackage "fourmolu").components.exes.fourmolu;

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.haskell-nix.cabalProject' {
          name = "htrace";
          compiler-nix-name = "ghc923";
          src = ./.;
          modules = [{
            reinstallableLibGhc = true;
          }];

          shell = {
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              # Tooling
              pkgs.cabal-install
              pkgs.postgresql

              # Formatting & linting
              pkgs.nixpkgs-fmt
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.apply-refact
              (fourmoluFor system)
              pkgs.haskellPackages.hlint
              pkgs.nodejs
              pkgs.nodePackages.npm
              pkgs.nodePackages.typescript-language-server
              pkgs.nodePackages.vscode-css-languageserver-bin
              pkgs.nodePackages.vscode-html-languageserver-bin
              # pkgs.nodePackages.vue-language-server
              # haskell-language-server.packages.${system}.haskell-language-server-922
            ];
          };
        };
    in
    {
      inherit nixpkgsFor;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      hydraJobs.x86_64-linux = self.checks.x86_64-linux;
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system};
          } "touch $out"
      );
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
