# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2026-07-22

### Added
- `make_filter_density()`: calculates burrow density by dividing apparent activity burrows by quadrant area, returning season and density columns

### Fixed
- Handle NA values in quantile calculations by adding `na.rm = TRUE` to `make_cuantiles()` function


[unreleased]: https://github.com/IslasGECI/clean_dcco_data/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/IslasGECI/clean_dcco_data/compare/v0.0.0...v0.1.0
