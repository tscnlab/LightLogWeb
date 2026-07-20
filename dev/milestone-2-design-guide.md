# Milestone 2 — Design guide and LightLogWeb identity

| Field | Result |
| --- | --- |
| Implementation status | Complete on 2026-07-20 |
| Acceptance status | Accepted by the owner on 2026-07-20 |
| Selected direction | Circadian Field / Measured Day Arc |
| Decision source | [`dev/visual-direction-brief.md`](visual-direction-brief.md) |
| Gallery | [`dev/component-gallery-app.R`](component-gallery-app.R) |
| Editable logo master | [`dev/brand/lightlogweb-logo-master.svg`](brand/lightlogweb-logo-master.svg) |
| Asset provenance | [`dev/brand/brand-provenance.md`](brand/brand-provenance.md) |

## Outcome

Milestone 2 now has one reusable design system shared by the component gallery
and the production app. It defines semantic light/dark color tokens, Source
Sans 3 typography, spacing and radii, responsive behavior, navigation,
containers, forms, action hierarchy, tables, plots, focus treatment, and every
Milestone 1 task state. It uses native Shiny, bslib, Bootstrap, DT, and gt
behavior with scoped external Sass and no bespoke JavaScript.

The gallery remains a design-system test surface. Its sample navigation and
workflow fragments do not approve the final information architecture.

## Design-system implementation

| Concern | Source of truth | Implemented behavior |
| --- | --- | --- |
| Cross-output brand declaration | [`_brand.yml`](../_brand.yml) | Source Sans 3 and the light Circadian Field palette for future compatible outputs; build-excluded because runtime styling is explicit. |
| Runtime theme and helpers | [`R/design-system.R`](../R/design-system.R) | Internal Bootstrap 5 theme factory, semantic token access, asset dependency, app scope, skip link, wordmark, view header, task statuses, plot theme, and contrast calculation. No public API was added. |
| Component styling | [`inst/app/scss/lightlogweb.scss`](../inst/app/scss/lightlogweb.scss) | Light/dark tokens, Source Sans 3 hierarchy, states, focus, controls, cards, tables, plots, overlays, gallery specimens, reduced motion, print, and responsive rules. Compiles with the locked LibSass toolchain. |
| Fonts | [`inst/app/www/fonts/`](../inst/app/www/fonts/) | Pinned `@fontsource/source-sans-3` 5.2.9 Latin and Latin-ext WOFF2 subsets at weights 400, 500, and 600; `font-display: swap`; exact SIL OFL 1.1 included; no runtime CDN. |
| Font loading | [`inst/app/www/css/lightlogweb-fonts.css`](../inst/app/www/css/lightlogweb-fonts.css) | One visible family for headings, prose, controls, tables, code-like values, plots, and diagnostics. |
| Production integration | [`R/LightLogWeb-app.R`](../R/LightLogWeb-app.R) and modules | Live accessible wordmark, favicon assets, skip link, one view `h1`, dark-mode control, responsive shell, themed dataset manager/dashboard/import/core showcase, status callouts, and plot theme. |

The app root is a real `.llw-app` DOM wrapper rather than an attribute added to
a Shiny tag list. This matters because tag-list attributes are not rendered as
an ancestor; the wrapper ensures that every scoped component and focus rule is
actually active in the browser.

### Component behavior

- Navigation uses weight and a structural marker as well as color. The
  wordmark is never the view heading, and an early skip link reaches main
  content.
- Cards and grouped settings use borders and spacing before shadow. Shadow is
  reserved for overlays.
- Form labels persist above controls. Validation is local and explains
  recovery. Each local action group has at most one primary action.
- `bslib` grid items use `min-width: 0`, preventing file inputs, Selectize
  controls, and long labels from escaping narrow columns.
- DT provides the compact sortable specimen. The comfortable gt summary is
  static because it does not need reactivity; this avoids a shared input/output
  ID warning in current Shiny/gt while preserving semantic table markup.
- Tables that can be wide sit in labelled, keyboard-focusable component-level
  scroll regions. The document does not scroll horizontally.
- Plot distinctions combine color with line type, point shape, direct labels,
  a written threshold, day/night bands, missing-data patterning, and an
  accessible summary.
- The gallery demonstrates a candidate expandable plot card using bslib's
  built-in full-screen control and a fill-aware plot region. Expansion changes
  presentation only; data, transformations, and export resolution stay
  unchanged. The affordance remains visible instead of appearing only on hover
  or focus.
- Idle, queued, running, finalizing, complete, warning, error, cancelled, and
  stale states all include an icon, label, and plain-language consequence.
  Dynamic status uses a polite live region.
- Reduced-motion preferences remove nonessential transition and animation
  duration without removing phase text.
- Owner-review refinements keep expanded accordion paint within rounded
  boundaries, align wrapped checkbox labels with their text rather than their
  controls, keep both halves of Shiny file inputs equal in height, and anchor
  plot annotations to the threshold and missing interval they describe.

## Component gallery

Run from the package root:

```sh
Rscript --vanilla -e 'source("dev/component-gallery-app.R"); shiny::runApp(component_gallery_app())'
```

The gallery includes the full identity matrix; type, token, and spacing
foundations; navigation and containers; action and form states; DT and gt
tables; an accessible expandable personal-light-exposure plot; dense/no-data
and linear/symlog cases; all task/system states; modal, popover, notification,
and live-state interactions; a contrast matrix; focus tour; reduced-motion
note; and narrow-layout specimens.

## Identity and asset pipeline

The Measured Day Arc retains LightLogR kinship through a point-up hexagonal
window and the light-exposure subject. It is deliberately separate in
construction: flat geometry, a day/night field, one measured arc, six
perpendicular interval ticks, and one solar marker; no room perspective,
barcode, extrusion, or inherited institutional color.

One editable master generates all shipping derivatives:

| Target | Derived assets |
| --- | --- |
| App header and reusable UI | Adaptive, light, dark, monochrome, and reversed mark and horizontal/stacked wordmark SVGs under `inst/app/www/brand/` |
| Browser and install surfaces | Adaptive `favicon.svg`; 16, 32, and 48 px PNG reductions; 180 px touch icon; 192 and 512 px install icons |
| Pkgdown/repository | `man/figures/logo.svg` |
| High-resolution export | Transparent light and approved dark-background 1024 and 2048 px PNGs |

Regenerate with:

```sh
Rscript --vanilla dev/brand/generate-brand-assets.R
```

The generator prefers the R `magick` and `rsvg` packages and has a documented
Node/sharp fallback. It rewrites the derivatives, provenance record, and
SHA-256 manifest. Automated tests verify the required files, PNG dimensions,
SVG construction classes, tick count, font count/license, and dependency
URLs. The three legacy raster identities are retained only in
`dev/brand/legacy/`, which is excluded from package builds.

## Deliberate adjustments to the visual brief

These changes preserve the selected direction while improving runtime and
small-size integrity:

1. The app ships static 400/500/600 Latin and Latin-ext WOFF2 subsets instead
   of a full variable font. The interface uses only those weights and no
   italics, so the subsets are smaller, explicit, and sufficient. The family,
   source version, fallback stack, license, and `font-display` behavior remain
   as selected.
2. Arc ticks are optically biased outward from the trajectory rather than
   centered across its 5-unit stroke. Centered short ticks disappeared at
   favicon sizes; the one-sided rhythm stays perpendicular to the local arc
   and reads as measurement without becoming a ruler.
3. The app header composes the mark asset with a live Source Sans 3 text node.
   This keeps the accessible name, font rendering, and responsive spacing
   reliable. Generated SVG lockups remain available for pkgdown and document
   contexts.
4. The gallery uses a static gt summary table and an interactive DT table. A
   reactive gt output added no useful behavior and produced a current-version
   shared-ID warning.

## Accessibility and browser verification

### Contrast

Automated contrast calculations use WCAG relative luminance. Text pairs use a
4.5:1 threshold and graphical/focus/state pairs use 3:1.

| Pair | Light | Dark | Threshold |
| --- | ---: | ---: | ---: |
| Primary text / page | 14.03:1 | 16.34:1 | 4.5:1 |
| Muted text / surface | 6.03:1 | 8.77:1 | 4.5:1 |
| Action link / surface | 6.62:1 | 8.81:1 | 4.5:1 |
| Button content / action | 6.62:1 | 10.48:1 | 4.5:1 |
| Control border / surface | 4.80:1 | 4.67:1 | 3:1 |
| Focus / page | 5.67:1 | 13.21:1 | 3:1 |
| Success / surface | 6.13:1 | 8.83:1 | 3:1 |
| Warning / surface | 6.79:1 | 9.84:1 | 3:1 |
| Danger / surface | 6.47:1 | 8.48:1 | 3:1 |

### Real-browser results

The in-app Chromium browser rendered the gallery and production app against
live local Shiny servers.

| Surface | Width | Document width | Result |
| --- | ---: | ---: | --- |
| Gallery | 320 px | 310 px | No document overflow; one `h1`; no broken images |
| Gallery | 768 px | 753 px | No document overflow; one `h1`; no broken images |
| Gallery | 1440 px | 1425 px | No document overflow; one `h1`; no broken images |
| Production import view | 320 px | 305 px | No document overflow; file control contained within its grid/card; no off-screen visible buttons |
| Production import view | 1440 px | 1425 px | Light and dark modes render without overflow or broken assets |

Additional observations:

- An owner-review regression pass at 1321 × 1324 px and 320 px confirmed
  clipped accordion paint with an inset visible focus ring, hanging indents for
  both checkbox labels, fully contained threshold and missing-interval labels,
  and zero document-level horizontal overflow.
- A follow-up pass at 1322 × 1324 px and 320 px measured both halves of the
  Shiny file input at 47 px with aligned top and bottom edges.
- The gallery-only plot card expands and closes at 1322 × 1324 px and 320 px;
  its visible controls are named `Expand card` and `Close card`, plot labels
  remain contained, and neither state introduces document overflow.
- In bslib 0.11.0, closing full-screen mode leaves focus on the document body
  instead of returning it to the expand control. The gallery pattern is
  accepted; production adoption remains deferred until an upstream fix or an
  approved focus-restoration enhancement resolves that behavior.
- Source Sans 3 weights 400 and 600 loaded from the local dependency.
- Light/dark surface and text tokens switched correctly.
- The shadow-DOM bslib color-mode control receives the same solid 3 px focus
  ring and 3 px offset as ordinary controls.
- The modal has a titled dialog and scoped actions; the provenance popover has
  a title and close control; notification text is exposed; and advancing the
  live task state changed Ready to Queued without stealing focus.
- The accessible summary table, plot alternative, labelled data regions,
  skip target, and single view heading are present in the rendered tree.
- Browser logs contained no errors and no LightLogWeb-owned warnings. Shiny's
  bundled bootstrap-datepicker locale files emit five upstream deprecation
  warnings; they are unrelated to the design code.

The browser-control surface cannot change the browser's native zoom level.
Reflow was therefore exercised at 720 CSS px, the layout width equivalent of a
1440 px viewport at 200% zoom, as well as the stricter exact 320 px case. The
owner accepted the milestone on the available evidence; a native 200% zoom
pass remains a recommended release check rather than an unrecorded claim.

## Preliminary name and visual-collision scan

The 2026-07-20 scan found no exact `LightLogWeb` result through general web
search or exact-name searches targeted at WIPO, EUIPO, and DPMA pages. The
broader `LightLog` root is already used by unrelated film-photography and
software-logging products and in light-logger research, so it must not be
treated as exclusive. Hexagon-and-sunrise imagery is also common in stock and
sustainability marks. The measured tick arc, sparse flat construction, and full
`LightLogWeb` wordmark are therefore essential differentiators.

This is not legal clearance. WIPO states that its database covers multiple
collections but recommends consulting relevant national/regional registers,
and its FAQ warns that absence from a search does not establish availability:

- [WIPO Global Brand Database](https://www.wipo.int/en/web/global-brand-database)
- [WIPO Global Brand Database FAQ](https://www.wipo.int/en/web/global-brand-database/faqs_branddb)
- [EUIPO search services and TMview](https://www.euipo.europa.eu/en/search-ip)
- [DPMAregister](https://www.deutsches-patentamt.de/service/leichte_sprache/dienste/dpmaregister/index.html)

Obtain a professional search before filing a mark or making a consequential
public launch.

## Technical verification

- `devtools::load_all()` and app construction pass.
- Focused design-system and module-contract tests pass.
- The complete test suite passes; the mirai-daemon test is intentionally
  skipped when daemons are unavailable, and its existing contained-failure
  warnings remain expected test behavior.
- Sass compiles from the packaged external source with bslib 0.11.0 and sass
  0.4.10.
- `dev/`, `_brand.yml`, `project-plan.md`, and other design sources remain
  build-excluded. Only the runtime Sass, fonts, font license, brand
  derivatives, and dependency stylesheet ship.
- The inspected source archive contains 88 entries, including 41 intentional
  runtime design assets, no build-excluded design source, and none of the three
  legacy runtime logos.
- No exported function or documented public R API changed.

Final source build and `R CMD check` closeout on R 4.5.0 completed with:

```text
0 errors | 0 warnings | 0 notes
```

The check built the source archive, installed it, loaded/unloaded the namespace,
ran examples, and reran the installed-package tests. External CRAN and
Bioconductor indexes were unavailable in the restricted check environment; all
declared dependencies were already present locally and the dependency check
completed successfully.

## Owner acceptance

The owner accepted Milestone 2 on 2026-07-20 after reviewing the design guide,
component gallery, Measured Day Arc identity, responsive expandable-plot
pattern, and requested clipping, alignment, and control refinements. The
preliminary collision scan remains an explicitly non-legal boundary, and the
native 200% browser-zoom pass remains recommended before release because it
could not be recorded through the browser-control surface.
