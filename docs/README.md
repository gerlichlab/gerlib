# Documentation

## Building the library
This project provides a [Nix shell](../shell.nix) which provides everything you should need to build this library. 
From the repository root, ensure you have whatever branch or tag checked out that you want to build, then start the Nix shell with `nix-shell`. 

The Nix shell provides `sbt`, which this project uses as its build tool. 
Once the Nix shell is active, use `sbt assembly` to build a full JAR with all dependencies bundled inside, and use `sbt publishLocal` to publish this library to your local machine's Ivy repository (useful for then building any project which depends on this one).