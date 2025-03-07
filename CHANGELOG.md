# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v0.4.0] - 2025-03-07

### Changed
* Generalised `EuclideanDistance.between` to accommodate points with any raw coordinate type `C` for which there's a `Numeric` instance available, rather than restricting to `C =:= Double`.

## [v0.3.2] - 2024-12-12

### Added
* `flipBy` (aliased also as `swapBy`) as syntax on `Tuple2[A, A]`, similar to `sortBy` on a sequence-like collection

## [v0.3.1] - 2024-12-04

### Added
* `collections.lookupBySubset`
* Documentation for how to build this library

## [v0.3.0] - 2024-11-21

### Changed
* Scala is now version 3.5.2.
* `sbt` is now version 1.10.5.
* License is now Apache 2.0, due to our use of `iron`.

### Added
* `graph` module for graph-related functionality (using `scala-graph`)
* `remove` method as syntax extension on `AtLeast2[Set, X]`, yielding a `NonEmptySet[X]`
* `IntraCellDelimiter[A]` abstraction for reading and writing a CSV cell which contains a collection of individual elements
* The `AtLeast2[C[*], A]` abstraction for a collection/container `C[A]` which must contain at least two elements
* `fs2.data.csv.CellEncoder[AtLeast2[C[*], A]]` instance derivation for when the element type `A` has a `fs2.data.csv.CellEncoder` instance available, and there's an given `IntraCellDelimiter[A]` available

## [v0.2.0] - 2024-10-22

### Changed
* Scala is now 3.5.1.

### Added
* _Many_ new data types and typeclass instances.

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
