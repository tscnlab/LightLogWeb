# LightLogWeb visual-direction workshop

Owner-only Milestone 2 review site for comparing Circadian Field, Spectral
Console, Daylight Almanac, and Clear Measure. The site is content-led and keeps
review state in browser local storage only; it has no database, uploads, runtime
secrets, or external analytics.

## Local use

```sh
pnpm install
pnpm run verify
pnpm run dev
```

The site source is intentionally kept under `dev/`, which is excluded from the
R package build. The LightLogR logo is shown only as an MIT-licensed heritage
reference. Institutional and funder identities remain attribution marks rather
than product-brand inputs.

The generated atmosphere prompts and file mappings are recorded in
`moodboard-prompts.md`.
