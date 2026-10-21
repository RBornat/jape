{
  description = "Jape: A configurable proof calculator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      packages = forAllSystems (system: {
        default = nixpkgsFor.${system}.callPackage ./package.nix { };
        jape = self.packages.${system}.default;
      });

      apps = forAllSystems (system: rec {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/jape";
          meta.description = "Jape proof calculator";
        };
        jape = default;
      });

      overlays.default = final: prev: {
        jape = final.callPackage ./package.nix { };
      };

      nixosModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.programs.jape;
        in
        {
          options.programs.jape = {
            enable = lib.mkEnableOption "Jape proof calculator";
            package = lib.mkPackageOption self.packages.${pkgs.system} "jape" {
              default = [ "default" ];
            };
          };

          config = lib.mkIf cfg.enable {
            environment.systemPackages = [ cfg.package ];
          };
        };

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              ocaml-ng.ocamlPackages_4_14.ocaml
              ocaml-ng.ocamlPackages_4_14.ocamlbuild
              openjdk17
              ant
              pandoc
              dos2unix
            ];
          };
        }
      );
    };
}
