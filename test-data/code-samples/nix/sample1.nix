#!/usr/bin/env nix-shell
#! nix-shell -i bash

# This is
# header

{ pkgs ? import <nixpkgs> {} }:
/* This is not the header. */
pkgs.mkShell {
  # A regular comment.
}
