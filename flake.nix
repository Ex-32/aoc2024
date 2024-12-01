{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    default-systems.url = "github:nix-systems/default";
  };

  outputs = inputs @ {self, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import inputs.default-systems;
      perSystem = {pkgs, ...}: {
        devShells.default = pkgs.mkShell {
          name = "aoc2024";
          hardeningDisable = ["all"];
          packages = with pkgs; [
            erlang_27
          ];
        };
      };
    };
}
