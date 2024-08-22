# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Changed
* Scala is now 3.5.0.

### Added
* `Area` and `MeanIntensity` data types, in a new subpackage--`measurement`--of the `roi` module.

## [v0.1.0] - 2024-07-10

### Added
* Core cell biology domain-specific types like `NuclearDesignation`, in `cell` module
* Core geometric types like `Point3D`, in `geometry` module
* Core imaging domain-specific types like `FieldOfView`, in `imaging` module
* Input-/output-related functionality in `io`
* ROI-related functionality in `roi` module
* Simple typeclass instances for many core types
* Code formatting with `scalafmt`
* `sbt-buildinfo` plugin
* `scalafix` plugin

### Changed
* Project structure switches to modules / subprojects, much like `refined` in terms of folder structure and the `build.sbt`.
* Rather than custom `opaque type`s, start using `iron` to represent refined domains of numeric types.
* Use `cats`-style source file layout for syntax enrichment files and typeclass instances files.

## [v0.0.1] - 2024-06-24
* This is the first release.
