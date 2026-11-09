# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
* CI tests on Java 21 only, on Linux and macOS, and no longer on Java 17: looptrace builds and runs this library with Java 21.
* CI installs the sbt launcher of the version `project/build.properties` names, 1.13.1, where setup-sbt installed the newest sbt, 2.0.12. On 9 November GitHub served that version's zip only as an error page, which failed every job.

## [v0.7.0] - 2026-11-08

### Changed
* **Scala 3.6.4 -> 3.9.0,** the long-term support line, where 3.6.4 is no longer patched. A library compiled with 3.9 can't be read by an older Scala 3 compiler, so a project depending on this release needs Scala 3.9 too. Under `-source:future`, 3.9 no longer accepts `implicit val`, and wants implicit arguments passed with `using`: each test suite's `PropertyCheckConfiguration` is now a `given` of the suite, which is preferred over the inherited one. Its stricter unused-symbol checks, errors under `-Werror`, removed unused imports and parameters, among them the `A: Order` bound of `max` on coordinates, which orders with `Order[C]`, so it asks for less.
* sbt 1.12.13 -> 1.13.1, the newest release of sbt 1.
* **The libraries at their newest release within their major version:** iron 3.0.0 -> 3.4.1, upickle and ujson 4.1.0 -> 4.4.3, pureconfig 0.17.10 -> 0.17.11, mouse 1.3.2 -> 1.4.0, scala-graph 2.0.2 -> 2.0.3, fs2-data-csv 1.11.2 -> 1.14.1, fs2-io 3.11.0 -> 3.14.0 and os-lib 0.11.4 -> 0.11.10; for the tests, scalatest and scalactic 3.2.19 -> 3.2.20, and scalacheck 1.18.1 -> 1.20.0 with scalatestplus' `scalacheck-1-20`. cats, kittens, squants and discipline-scalatest are at their newest already.
* **`NonnegativeInt`, `NonnegativeReal`, `PositiveInt`, `PositiveReal` and `Distance` extend iron's `RefinedSubtype`,** where they extended `RefinedType`. Since iron 3.1.0, a `RefinedType` is a type of its own, no longer a subtype of the type it refines; `RefinedSubtype`, from iron 3.2.0, keeps them as they were, so a `PositiveInt` is still an `Int`.
* **upickle traces `read` by default since 4.4.0:** an error while reading comes wrapped in `upickle.core.TraceVisitor.TraceException`, whose message is the JSON path, with the reader's own error as its cause, unless `read` is given `trace = false`. The codec of `DistanceLike` reads its value and metric type untraced, so its message still gives the value's own error, where it would have given only `$`; a test pins it.
* **The sbt plugins at their newest releases:** sbt-assembly 2.3.1 -> 2.5.0, sbt-github-actions 0.31.0 -> 0.32.2, which moves the workflow to `actions/setup-java` v6, sbt-scalafmt 2.6.1 -> 2.6.2, sbt-buildinfo 0.13.1 -> 0.13.2 and sbt-scalafix 0.14.7 -> 0.14.9; and typelevel's scalafix rules 0.2.0 -> 0.6.0, which the code passes as it is.
* scalafmt 3.9.3 -> 3.11.5, which aligns six `case` arrows anew.

### Removed
* **Breaking: the `zarr` module, and `readRoiData` and its helper `getOriginZYX` from `roi`** ([#30](https://github.com/gerlichlab/gerlib/issues/30)). The module read format-2 Zarr stores with jzarr 0.4.2, which brings Unidata's netCDF library (`edu.ucar:cdm-core`) and jblosc. cdm-core is not on Maven Central, so every build had to name Unidata's repository, and in November 2026 Java on GitHub's macOS runner rejected that repository's certificate, which failed looptrace's CI while it published this library. `roi` depended on `zarr` only for `readRoiData`, so `roi` and `io`, and whatever depends on them, brought all three. Nothing else in this library used them, no test covered them, and jzarr can't read format-3 stores. A project that reads ROI data with them can stay on v0.6.0. `roi` now names `imaging`, which it used and had through `zarr`.

### Fixed
* **CI runs the scalafmt and scalafix checks,** on Linux with Java 21. Their condition was never true, so no job ran them: it compared the runner's OS with the label `ubuntu-latest`, where the runner names its OS `Linux`, and tested `runner.java-version`, which does not exist. It now tests the matrix's Java entry, as looptrace's build does.

## [v0.6.0] - 2026-07-07

### Added
* `EuclideanDistance.unsafe` method, wrapping a `squants.space.Length` value as a `EuclideanDistance`, 
performing the necessary validation as nonnegative and erroring otherwise.
* `configuration` module, with support for parsing definition of pixel lengths for `scopt` or `pureconfig`.
* Instances of `Show` and `SimpleShow` for `geometry.Distance`, with an implementation which can be guaranteed to roundtrip through `Distance.parse`
* Tests for `PixelDefinition` and `Pixels3D`

### Changed
* Changed the error message for refinement of `squants.space.Length` as a `Distance` value from "Allegedly nonnegative length must actually be nonnegative." to "Allegedly nonnegative length is actually negative."
* Updated `sbt` to version 1.12.13
* Restricted the CI Java version matrix to the LTS releases Temurin 17 and 21 (dropping 11 and 19).

## [v0.5.2] - 2026-03-18

### Fixed
* Updated Unidata repository URL for `cdm-core` dependency, as the upstream structure changed.

## [v0.5.1] - 2025-09-09

### Added
* `cats.Eq` instance for `imaging.FieldOfViewLike`
* Introduced more basic helper functions for reading from ZARR

## [v0.5.0] - 2025-06-17

### Added
* Introduced a transparent `Distance` type alias for a `squants.space.Length` value refined to be nonnegative
* Introduced `Pixels3D` and `PixelDefinition` types

### Changed
* `iron` is now v3.0.0.
* Removed the `ProximityComparable` trait
* Simplified the distance types considerably, removing the `DistanceThreshold` and its subtypes
* Simplification of the numeric refinement types, relying more directly on reference to the underlying `iron` type names
* More general and typesafe way to define a pixel as a unit of length
* Better definition of distance-related types

## [v0.4.1] - 2025-03-19

### Added
* `getRawValue` member to the `FieldOfView` type, such that client code can access the raw nonnegative integer value, but in a clear and intentional way.

### Changed
* More updates regarding Scala 3.6's revision of syntax for `given` instances and for bounds, per SIP-64.
* Apply `-new-syntax` + `-rewrite` to scalac settings.

## [v0.4.0] - 2025-03-12

### Added
* `.jvmopts` to provide more memory for the environment, mainly related to use of Semantic DB with `scalafix`

### Changed
* Generalise the wrapped raw coordinate value, of distance computations to accommodate points with any raw coordinate type `C` for which there's a `Numeric` instance available, rather than restricting to `C =:= Double`.
* Bump Scala from 3.5.2 to 3.6.4.
* Bump up all project plugins and dependencies to latest versions (as of 2025-03-07).
* Update to Scala 3.6's revision of syntax for `given` instances and for bounds, per SIP-64.
* Back off of a couple of the more aggressive applications of `fewerBraces`, as this was not playing nicely between `scalafmt`, `scalafix`, general compilation, and IDE plugin (Metals). 
This seemed to pertain mainly to replacement of braces by colons and indentation in `Try` blocks (i.e., `scala.util.Try`, not `try...catch`).
* Condensed / centralised implementation of `PiecewiseDistance.between`.

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
