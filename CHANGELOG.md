# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).  In the spirit of Rich Hickey's "Spec-ulation" keynote address at the 2016 Clojure/conj, no backwards-incompatible changes will be made to any public var after version 1.0.0.  As a result, semantic versioning is overkill (there will be no breaking changes after 1.0.0)

Versioning is managed using git tags as implemented in the Makefile.  The pom file is generated using [garamond](https://github.com/workframers/garamond) and it defers to the same git tags.

## [Unreleased](https://github.com/cch1/zipii/compare/v0.2.0...HEAD)
## [0.2.0](https://github.com/cch1/zipii/compare/v0.1.0...v0.2.0)
### Update
- Refactoring to (partially) satisfy linter.
- Documentation updates.
- Flesh out build tooling (`Makefile`, `deps.edn` aliases, etc).
### Fixed
- Fixed bug in scar Loc support for pivot/`down-to`.
## [0.1.0](https://github.com/cch1/zipii)
### Added
- Initial functionality for navigating and manipulating trees using Zippers, both with and without scars.
