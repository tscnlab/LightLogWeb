# Milestone 7 derived GLC fixtures

These are small, deterministic, offline test fixtures derived from three
different published public GLC packages. They are not complete published
datasets and are not current or live measurement evidence.

Each fixture represents five participants and retains the first 32 rows from
each exact selected public source file. Exact bounded public values are kept
under the owner's explicit no-filtering instruction. TUM contains five files,
IZTECH contains six files because IZTECH_S004 has separate AL03 and AL04
device files, and UCR contains five files. Metadata is limited to the selected
study, participants, files, devices, datasheets, and imported variables.

The exact sources, revisions, licenses, selections, compatibility and unit
identities, resource availability, hashes, generation tools, and explicit
no-filtering policy are frozen in `manifest.json`.

- `tum-medi-only.rds` requests only the exact `MEDI` source variable.
- `iztech-light-subset.rds` requests the scientifically useful `LIGHT` and
  `MEDI` pair. It remains explicitly partial because two declared metadata
  resources returned HTTP 400 through exported `glc_metadata()` at generation.
- `ucr-all-light-variables.rds` requests every variable from the compatible
  UCR S001 through S005 wrist files.

Routine tests read these files directly and must not reopen a public package.
Regeneration is an owner-gated development action:

```sh
LLW_M7_GENERATED_AT='YYYY-MM-DDTHH:MM:SSZ' \
  RENV_CONFIG_SANDBOX_ENABLED=FALSE \
  Rscript dev/generate-milestone-7-public-fixtures.R
```

The generator requires the pinned glcdp revision and writes through a staging
directory before replacing the four generated files.
