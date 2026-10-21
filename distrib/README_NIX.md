# Building and Running Jape with Nix

This directory contains Nix packaging for **Jape** (`flake.nix`, `package.nix`, and `default.nix`).

## Building with Flakes

From the repository root:
```bash
nix build .?dir=distrib
```

Or from the `distrib/` directory:
```bash
nix build
```

The resulting package will be in `./result` with:
- `result/bin/jape`: The graphical Jape application.
- `result/bin/jape-engine`: The standalone OCaml proof engine.
- Desktop entries and application icons for desktop environments.

## Running directly

```bash
nix run .?dir=distrib
```

Or from `distrib/`:
```bash
nix run
```

## Development Shell

To enter a development shell containing all necessary build tools (OCaml 4.14, ocamlbuild, OpenJDK, Ant, Pandoc, Dos2unix):
```bash
nix develop .?dir=distrib
```

## Installing on NixOS

In your system's `flake.nix`:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    jape.url = "github:RBornat/jape?dir=distrib";
  };

  outputs = { self, nixpkgs, jape, ... }: {
    nixosConfigurations.yourhostname = nixpkgs.lib.nixosSystem {
      modules = [
        # Method 1: Direct package
        {
          environment.systemPackages = [
            jape.packages.x86_64-linux.default
          ];
        }

        # Method 2: NixOS module
        # jape.nixosModules.default
        # { programs.jape.enable = true; }

        # Method 3: Overlay
        # {
        #   nixpkgs.overlays = [ jape.overlays.default ];
        #   environment.systemPackages = [ pkgs.jape ];
        # }
      ];
    };
  };
}
```

Then rebuild your system:
```bash
sudo nixos-rebuild switch
```
