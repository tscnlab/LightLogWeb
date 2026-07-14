# MeLiDos test fixtures

This directory contains minimal, deidentified extracts from four public MeLiDos site repositories. The files cover eye-, chest-, and wrist-level one-minute light data, European DST time zones, an equatorial site, a negative UTC offset, high-latitude photoperiod, and representative wear/sleep intervals.

All source datasets are licensed CC BY 4.0. `manifest.csv` and `manifest.json` record each repository, release, commit, DOI, selected row count, transformation, and file checksum. The extraction is reproducible with `data-raw/build-melidos-fixtures.R`. Participant codes are replaced with fixture-only identifiers and only two local dates plus selected measurement columns are retained.

Ordinary package checks use only these offline fixtures. Live downloads through `melidosData` are opt-in and run separately in CI.
