# Haskell SSB Client

This is an implementation of the SSB protocol documented at: https://ssbc.github.io/scuttlebutt-protocol-guide/

## Status

This is still a work in progress, a lot of things are broken

## Building

To build just run `stack build --copy-bins` and it will copy the ssb client executable to stack's bin directory (default should be something like `~/.local/bin`)

## Usage

Just run `ssb-hs` and it will open the client and start a CLI. (Work in progress)

The program will create a new directory (`~/.ssb-hs`) where all data will be stored. If there is no identity (`~/.ssb-hs/secret`) then a new one will be created. (This is not a secure identity afawk and should only be used for testing)

Try opening clients in other computers (since each client will use the same port, some virtualization might be needed to test in a single machine) and following those identities over LAN.
