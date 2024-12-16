{
    inputs = {
        # Get nixpkgs. Check https://status.nixos.org for newer versions.
        nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

        # Get nixpkgs-unstable
        nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

        # Flake-Utils
        flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils, nixpkgs-unstable }:
        flake-utils.lib.eachDefaultSystem (system:
            let
                # Define an overlay for the unstable nixpkgs channel.
                nixpkgs-unstable-overlay = final: prev: {
                    unstable = import nixpkgs-unstable {
                        inherit system;
                    };
                };

                # Define pkgs as nixpkgs with some additional overlays
                pkgs = import nixpkgs {
                    inherit system;

                    # Define overlays
                    overlays = [
                        # Nixpkgs unstable overlay
                        nixpkgs-unstable-overlay
                    ];
                };

                ################################################################
                # Build dependencies
                nativeBuildInputs = with pkgs; [
                    # R Language
                    R
                    rPackages.languageserver

                    # R Packages
                    rPackages.ggplot2
                    rPackages.dplyr
                    rPackages.tidyr
                    rPackages.svglite
                    rPackages.data_table
                ];

                ################################################################
                # Runtime dependencies
                buildInputs = # with pkgs;
                [];

                ################################################################
            in
                with pkgs;
            {
                devShells.default = mkShell {
                    inherit nativeBuildInputs buildInputs;
                };
            }
        );
}
