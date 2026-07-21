# Milestone 3 development fixtures

These fixtures exercise the raw-import boundary without introducing network
access into routine tests. The complete `dev/` directory is excluded from R
package builds.

## IZTECH light-glasses snapshot

`melidos-iztech-light-glasses-1minute.rds` is a development snapshot of the
MeLiDos IZTECH `light_glasses_1minute` dataset. It contains 151,200 rows, 37
columns, and 17 anonymous participant IDs. Timestamps use `Europe/Istanbul` and
span 2024-12-24 through 2025-06-01. The uncompressed R object is approximately
42.7 MiB; the xz-compressed fixture is approximately 2.9 MiB.

- Dataset: Didikoglu, A., Akgun, S. G., Aydin, S. N., Kayar, Z., Zauner, J.,
  & Spitschan, M. (2025). *Personal light exposure dataset for Izmir, Türkiye*
  (Version 1.0.1). <https://doi.org/10.5281/zenodo.16568109>
- Repository: <https://github.com/MeLiDosProject/DidikogluEtAl_Dataset_2025>
- Source file commit: `1f83aa1563603610a4f43afe2a0fbe30f805191c`
- Source Git blob: `8a6aef8c7c0e842eaa167d22fef634848b57508a`
- Retrieval date: 2026-07-20
- Loader: `melidosData::load_data("light_glasses_1minute", site = "IZTECH")`
  from melidosData 1.0.6
- Snapshot transform: no row or column changes; serialized as RDS version 3
  with xz compression
- Snapshot SHA-256:
  `939bea8c78578db3b9840e20a505b0366e3c3ee347c1d838f2c6781844a65ab8`
- License: [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)

Regenerate from the repository root with:

```sh
LIGHTLOGWEB_REFRESH_M3_FIXTURE=1 Rscript dev/generate-milestone-3-fixtures.R
```

The generator deliberately checks the pinned dimensions, names, participant
count, and source time zone. If upstream data changes, review and update this
provenance before accepting a refreshed snapshot.

## Synthetic raw files

Files under `raw/` are small, fictional ActLumus exports for manual browser
acceptance. They contain no participant data. Routine tests generate equivalent
files in temporary directories so test results do not depend on repository
paths.
