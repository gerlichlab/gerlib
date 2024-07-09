# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v0.1.0] - 2024-07-09

### Added
* Core domain-specific types like `FieldOfView` and `NuclearDesignation`
* Code formatting with `scalafmt`
* `sbt-buildinfo` plugin
* Simple typeclass instances for many core types

### Changed
* Project structure switches to modules / subprojects, much like `refined` in terms of folder structure and the `build.sbt`.
* Rather than custom `opaque type`s, start using `iron` to represent refined domains of numeric types.
* Use `cats`-style source file layout for syntax enrichment files and typeclass instances files.

## [v0.0.1] - 2024-06-24
* This is the first release.
