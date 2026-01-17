{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
    buildInputs = [
        erlang
        (lib.optional stdenv.isLinux inotify-tools)
    ];
}
