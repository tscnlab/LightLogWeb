# LightLogWeb selected visual-direction brief

| Field | Decision |
| --- | --- |
| Status | Owner-selected direction; ready for Milestone 2 implementation |
| Decision date | 2026-07-20 |
| Base direction | **Circadian Field** |
| Logo territory | **Measured Day Arc**: Daylight Almanac's hex sun-path window with Spectral Console measurement ticks |
| Typography | **Source Sans 3 throughout** |
| Governing milestone | [Milestone 2 — Design guide and LightLogWeb identity](../project-plan.md#milestone-2--design-guide-and-lightlogweb-identity) |

This document is the implementation source of truth for the selected visual
direction. It records the owner's review decision and turns it into buildable
requirements. It does not approve final logo artwork, `_brand.yml`, Sass, or
the component gallery; those remain the next reviewed implementation slice.

## 1. Decision

LightLogWeb will use **Circadian Field** as its visual system: calm scientific
clarity, deep blue-green and daylight-neutral surfaces, cyan action and data
signals, restrained amber highlights, generous space, and subtle time-based
structure.

Three reviewed ideas are combined deliberately:

- **From Circadian Field:** the palette, quiet data-first surfaces, visual
  rhythm, and balance between scientific credibility and beginner access.
- **From Daylight Almanac:** the logo's flat hexagonal window, curved
  day/night boundary, and simplified sun-path rather than an abstract aperture.
- **From Spectral Console:** short ruler marks that sit perpendicular to and
  visibly measure the sun-path arc. The marks must not turn the identity into a
  dense instrument panel.
- **From Clear Measure:** one-family typographic discipline. Source Sans 3 is
  used for headings, prose, controls, tables, values, plots, and diagnostics.

The resulting visual idea is **measured daylight**: LightLogWeb helps people
understand light exposure as something observed across time, while making the
analysis choices and system state equally legible.

### Review rationale

| Direction | Review outcome |
| --- | --- |
| Circadian Field | Selected because it best balances scientific credibility, beginner approachability, LightLogR kinship, data-visualization fit, accessibility, and maintainable bslib implementation. |
| Spectral Console | Its measurement discipline is useful for the arc, but the full direction is too dense and expert-coded for the default experience. |
| Daylight Almanac | Its sun-path logo is more human and immediately legible, but the warm editorial surfaces and serif voice would compete with dense analytical work. |
| Clear Measure | Its single-family typography is retained, but the wider cobalt-and-neutral system is less distinctive and weakens the light/time character. |

### Locked decisions

- Circadian Field is the base direction.
- Source Sans 3 is the only visible interface typeface.
- The logo uses a flat point-up hexagonal window, a day/night boundary, a
  sun-path arc, a single amber solar marker, and sparse ticks measuring the
  arc.
- The identity remains flat, calm, spacious, and data-led in light and dark
  modes.
- LightLogR kinship comes from the hexagon and light/time subject, not from its
  three-dimensional room, barcode, perspective, or construction style.
- TUM, TSCN, MeLiDos, and funder marks are attribution only. They do not alter
  the product palette or enter the LightLogWeb mark.
- Meaning is never carried by color or motion alone.

### Open to implementation testing

- Optical adjustment of the final SVG geometry and stroke widths.
- The exact number and spacing of minor arc ticks within the rules below.
- Responsive density within the defined spacing and type scales.
- The precise bslib component composition, provided the gallery does not lock
  in the final application information architecture.

## 2. Approved design narrative

**Make light exposure legible across time. Reveal consequential choices. Calm
complexity. Communicate every state without relying on color alone.**

LightLogWeb is a scientific workbench, not a clinical device dashboard or a
consumer wellness product. It should feel trustworthy before it feels
technical. The interface exposes provenance, settings, revisions, warnings,
and task progress in plain language, then gives plots and tables enough quiet
space to carry the evidence.

The design uses the passage from daylight to night as an organizing metaphor.
It may appear as a horizon, a measured arc, or a restrained time band when
that helps comprehension. It is not a decorative theme to repeat on every
surface.

### Experience principles

1. **Evidence first.** Plots, tables, selected settings, and provenance have
   stronger visual priority than decoration.
2. **Consequences stay visible.** Actions that change prepared data or
   interpretation name their effect before application.
3. **Calm is not ambiguity.** Quiet surfaces still use explicit hierarchy,
   boundaries, labels, and states.
4. **Beginner-readable, expert-efficient.** Explanations use plain language;
   repeated workflows retain compact, predictable placement.
5. **Time has structure.** Timestamps, revisions, task phases, and day/night
   context are aligned and comparable.
6. **Accessibility is structural.** Semantics, focus, labels, icons, patterns,
   and responsive behavior are part of the component definition.

### Visual guardrails

Use flat geometry, clear alignment, soft daylight neutrals, one dominant blue-
green action channel, and amber only for measured highlights or annotations.
Use borders more often than shadows and whitespace more often than dividers.

Do not use 3D rooms, extruded hexagons, glass effects, neon glow, HUD grids,
generic wellness imagery, decorative spectral rainbows, or institutional
colors as product-brand shortcuts. Do not place a horizon or arc behind dense
data if it reduces legibility.

## 3. Semantic color tokens

Components must consume semantic custom properties rather than raw palette
values. The values below are the approved targets from the validated
Circadian Field workshop.

| Semantic token | Light | Dark | Intended use |
| --- | --- | --- | --- |
| `--llw-bg-page` | `#F6F8F4` | `#081A20` | Page and quiet canvas |
| `--llw-bg-surface` | `#FFFFFF` | `#10282F` | Cards, menus, dialogs, controls |
| `--llw-bg-subtle` | `#E6EFEB` | `#16343A` | Selected rows, bands, quiet fills |
| `--llw-text` | `#102A33` | `#EEF7F5` | Primary text and axes |
| `--llw-text-muted` | `#52666D` | `#B7C7C5` | Secondary text and metadata |
| `--llw-border` | `#B8C7C9` | `#46636A` | Structural and decorative borders |
| `--llw-border-control` | `#60767B` | `#74939A` | Input and interactive boundaries |
| `--llw-action` | `#0B6675` | `#76D2DD` | Primary action, link, running state |
| `--llw-on-action` | `#FFFFFF` | `#07171C` | Content on the action color |
| `--llw-accent` | `#C36A1D` | `#F2B66D` | Sun marker, selected plot point, annotation |
| `--llw-focus` | `#006D7A` | `#9CEBF2` | Keyboard focus ring |
| `--llw-success` | `#1E6F4A` | `#83D5AA` | Successful completion |
| `--llw-warning` | `#8A4B09` | `#FFC47D` | Recoverable warning or stale result |
| `--llw-danger` | `#A33B3B` | `#FFAAAA` | Error and destructive action |
| `--llw-grid` | `#B9C9CA` | `#46636A` | Nonessential plot grid and guides |

The workshop contrast check passes the selected reference pairs. In light/dark
mode respectively, primary text measures `14.03:1` / `16.34:1`, muted text
`5.65:1` / `10.17:1`, primary-button content `6.62:1` / `10.48:1`, control
boundaries `4.80:1` / `4.67:1`, focus rings `5.67:1` / `13.21:1`, and amber
chart marks `3.89:1` / `8.55:1`. Derived and interactive states still require
re-testing in their actual contexts.

### Color-use rules

- `--llw-text` is the default for prose and essential data. Use
  `--llw-text-muted` only for genuinely secondary content.
- Amber is not normal body text. On light surfaces it is a non-text mark,
  selected point, or sufficiently large graphical annotation. Warning text
  uses `--llw-warning`, not `--llw-accent`.
- Status colors always appear with a plain-language label and an icon. Plot
  categories also use line style, point shape, direct labels, or patterns.
- Interactive boundaries use `--llw-border-control`; the lighter structural
  border is insufficient for an otherwise unbounded control.
- The focus ring is a solid 3 px ring with a 3 px offset. It must remain visible
  against both page and surface backgrounds and must not be removed on mouse
  focus if doing so would make the control state ambiguous.
- Hover and active colors are derived in Sass from their semantic parent and
  re-tested. Do not lower opacity to manufacture a disabled or hover state.
- Disabled controls retain readable labels, expose their disabled state
  semantically, and do not look like active controls.
- Any later token adjustment must rerun the contrast matrix for text, controls,
  focus, chart marks, and every task/system state in both modes.

## 4. Typography

Use **Source Sans 3 throughout the visible product**. Do not introduce Source
Serif 4, IBM Plex Mono, Atkinson Hyperlegible Next, or a decorative display
face. Hierarchy comes from weight, size, spacing, alignment, and tabular
figures.

### Font assets

- Self-host a pinned Source Sans 3 WOFF2 variable font for normal text and an
  italic WOFF2 only if italic content is actually used.
- Retain the font's SIL Open Font License alongside the vendored files.
- Use `font-display: swap`; do not request fonts from Google Fonts or another
  runtime CDN.
- The fallback stack is `"Source Sans 3", system-ui, -apple-system,
  BlinkMacSystemFont, "Segoe UI", sans-serif`.
- Use the same family for headings, interface labels, tables, timestamps,
  diagnostic values, code-like values, and plot text. Apply
  `font-variant-numeric: tabular-nums` to aligned numbers, times, dates,
  durations, versions, and units.

### Type scale

| Role | Size | Weight | Line height | Notes |
| --- | --- | --- | --- | --- |
| Display | `clamp(2.5rem, 4vw + 1rem, 4rem)` | 600 | 1.05 | Landing/gallery use only |
| Page heading | `clamp(2.25rem, 3vw + 1rem, 3.5rem)` | 600 | 1.08 | One semantic `h1` per view |
| Section heading | `clamp(1.75rem, 2vw + 1rem, 2.5rem)` | 600 | 1.12 | `h2` |
| Subsection heading | `clamp(1.25rem, 1vw + 1rem, 1.5rem)` | 600 | 1.2 | `h3` |
| Body and controls | `1rem` | 400 or 500 | 1.5–1.6 | Default reading size |
| Label | `0.875rem` | 600 | 1.3 | Sentence case |
| Secondary text | `0.875rem` | 400 | 1.45 | Never essential by color alone |
| Compact data | `0.875rem` | 400 or 500 | 1.35 | Tabular numerals |

Headings use modest negative tracking only at 2rem and above, no tighter than
`-0.025em`. Eyebrows may use uppercase at no smaller than `0.75rem`, with
`0.08em` letter spacing, and only for short orientation labels. Body copy stays
within about 70 characters per line.

## 5. Spacing, layout, and surfaces

### Spacing scale

Use a 4 px base and the following named scale. Components may combine tokens;
they may not introduce arbitrary one-off spacing without a documented reason.

| Token | Value | Typical use |
| --- | --- | --- |
| `--llw-space-1` | `0.25rem` / 4 px | Icon-to-label optical adjustment |
| `--llw-space-2` | `0.5rem` / 8 px | Tight inline groups |
| `--llw-space-3` | `0.75rem` / 12 px | Compact controls and table cells |
| `--llw-space-4` | `1rem` / 16 px | Default component gap |
| `--llw-space-6` | `1.5rem` / 24 px | Card padding and form groups |
| `--llw-space-8` | `2rem` / 32 px | Section subdivisions |
| `--llw-space-12` | `3rem` / 48 px | Major section separation |
| `--llw-space-16` | `4rem` / 64 px | Wide-view breathing room |

### Layout behavior

- Cap the primary application canvas at `77.5rem` / 1240 px. Long-form copy
  remains narrower.
- Use Bootstrap 5 breakpoints as implementation primitives; verify behavior at
  the required 320, 768, and 1440 px widths rather than designing for device
  names.
- Below 576 px, use one column, stack actions, keep the primary action first,
  and place wide tables in labelled component-level scroll regions. The
  document itself must not scroll horizontally.
- From 576 through 991 px, keep the main workflow linear. Two-column forms are
  allowed only for short, strongly related fields.
- At 992 px and above, controls may sit beside plots or tables when the reading
  order remains logical in the DOM.
- At 1200 px and above, increase whitespace rather than stretching prose or
  controls across the entire canvas.
- At 200% zoom, preserve the same semantic order, wrapping labels and actions
  before truncating them.

### Surface rules

- Controls use a 6 px radius, ordinary cards a 12 px radius, and large shells
  or dialogs an 18 px radius. Pills are reserved for short statuses, tags, and
  segmented choices.
- Ordinary cards use a 1 px structural border on the surface color. Do not add
  a shadow merely to separate every panel.
- Shadows are reserved for overlays such as menus, popovers, and modals. They
  remain neutral and low-opacity; no colored glow.
- A card needs a clear semantic grouping, not just visual framing. Avoid cards
  nested more than one level deep.
- Use a subtle horizon or time band only when it orients a plot, period, or
  task phase. It should never sit behind body text.
- Interactive targets should be at least 44 × 44 CSS px where space allows and
  never below the WCAG 2.2 minimum target requirement without a documented
  exception.

## 6. Component language

### Navigation

- Keep navigation flat, predictable, and quiet. The active destination uses
  text, weight, and a structural marker—not color alone.
- The product wordmark is not the page `h1`; each view owns one descriptive
  `h1`.
- Preserve logical keyboard order and an early skip link to main content.
- Institutional/funder attribution belongs in an about or footer region, not
  beside the primary product identity.

### Cards and callouts

- Card headers describe the decision or evidence inside, not a generic type
  such as “Panel”.
- Default cards are white/deep-blue-green surfaces with borders. Use subtle
  fills for grouped settings, selected states, or explanatory callouts.
- Success, warning, and error callouts include an icon, heading, concise
  explanation, and recovery action when one exists.

### Forms and actions

- Put persistent labels above controls; placeholders are examples, never
  labels.
- Put units in the label or a semantic suffix. Pair validation with the
  relevant field and describe how to fix it.
- Use at most one visually primary action in a local decision group. Secondary
  actions are outline or quiet; destructive actions use danger semantics and
  confirmation proportional to consequence.
- Draft, preview, apply, reset, and undo remain visibly distinct. “Apply” names
  what will change when the consequence is not obvious.
- Focus, hover, active, disabled, valid, and invalid states are required
  specimens, not implementation details to defer.

### Tables

- Left-align labels and prose; right-align numeric measures and use tabular
  numerals. Units belong in column headings where possible.
- Favor row rules and space over full cell boxing. Use zebra fills only when
  they materially improve long-row tracking.
- Sticky headers must remain semantically associated with cells. Sorting uses
  text available to assistive technology as well as a visible icon.
- On narrow screens, preserve key identifiers and values. A labelled internal
  scroll region is acceptable; clipping, tiny type, and document-level
  horizontal overflow are not.

### Motion

- Use motion only to explain state change or preserve spatial context. Normal
  transitions target 120–180 ms.
- A running indicator always includes phase text. Animation is optional and
  pauses or disappears under `prefers-reduced-motion: reduce`.
- Do not animate decorative arcs, gradients, or plots continuously.

## 7. Task and system states

These labels align with the Milestone 1 task contract. Each rendered state
must include its exact text (or a context-specific plain-language equivalent),
an icon, and the semantic color. Dynamic updates use a polite live region and
do not steal focus.

| State | Meaning | Visual treatment | Required communication |
| --- | --- | --- | --- |
| Idle | No task is active; the action is available | Muted `circle` icon | “Ready” or an explicit available action |
| Queued | Work was accepted and is waiting to start | Action-color `hourglass-split` | “Queued” plus what is waiting |
| Running | Work is executing | Action-color `arrow-repeat`; motion optional | Current phase and progress if knowable |
| Finalizing | Computation ended but results are being assembled or committed | Action-color `three-dots` | “Finalizing”; never imply completion |
| Complete | The result committed to the current dataset revision | Success `check-circle` | What completed and the resulting revision |
| Warning | A usable result exists but needs attention | Warning `exclamation-triangle` | Consequence and recommended action |
| Error | The scoped operation failed; the app remains usable | Danger `x-octagon` | Safe summary, recovery action, private diagnostic reference |
| Cancelled | The user stopped the operation before commit | Muted `slash-circle` | “Cancelled; no changes applied” when true |
| Stale | A result belongs to an older revision and was rejected | Warning `clock-history` | “Stale result not applied” plus current revision |

Unknown progress must not be presented as a percentage. Cancellation remains
available only while it can still prevent commit. A stale result is a safe
rejection, not a generic error, and must never silently replace newer state.

## 8. Plot and data-visualization specification

Plots inherit the same surface, text, and grid tokens as the interface.
Day/night context is a subtle band, not a decorative gradient.

| Role | Light | Dark | Additional encoding |
| --- | --- | --- | --- |
| Primary measured series | `#0B6675` | `#76D2DD` | Solid line; circle points when needed |
| Comparison or selected series | `#C36A1D` | `#F2B66D` | Dashed line; square points |
| Reference/threshold | `#60767B` | `#B7C7C5` | Long-dash line plus direct label |
| Axes and essential labels | `#102A33` | `#EEF7F5` | Text and position |
| Grid/nonessential guide | `#B9C9CA` | `#46636A` | Thin line; never the sole boundary |
| Day/night or interval band | `#E6EFEB` | `#16343A` | Named legend/direct annotation |

- Use the primary line for the user's measured light data. Amber is a
  comparison, selected datum, threshold crossing, or brief annotation—not a
  second default brand background.
- Confidence intervals use a low-opacity fill plus an opaque outline. Missing,
  excluded, imputed, or outside-window data use gaps, hatch, shape, or labels;
  opacity alone is insufficient.
- For more than two groups, introduce line type, point shape, small multiples,
  or direct labels before adding hues. Any categorical palette expansion needs
  a separate contrast and color-vision review.
- Do not use success, warning, or danger colors as ordinary data categories.
- Preserve units, timezone, date window, grouping, transformation, and scale
  information next to the plot. Linear versus symlog changes presentation,
  not the underlying data.
- Keep axes and tooltip text in Source Sans 3 with tabular numerals. Interactive
  plots require keyboard-accessible equivalents or an adjacent accessible
  summary/table.

## 9. Logo construction brief

### Concept: Measured Day Arc

The mark is a calm scientific window onto a day: a point-up hexagon holds a
curved day/night boundary, a sun-path arc, and one warm solar point. Sparse
ticks are set perpendicular to the arc so they visibly measure the path rather
than forming a separate ruler or laboratory beam.

The mark should read in this order:

1. LightLogR family resemblance through the hexagon.
2. Light across time through the day/night field and arc.
3. Measurement through the restrained tick sequence.

### Construction target

- Use a `160 × 176` SVG viewBox and a point-up hexagon with initial vertices
  `80,6 150,47 150,129 80,170 10,129 10,47`.
- Use a 5-unit outer stroke with rounded joins. Keep the mark flat; no depth,
  perspective, gradients, shadows, bevels, or enclosing badge.
- Clip the inner field to the hexagon. Start the day/night boundary around
  `M4 104 Q76 88 156 103`; optical adjustment is allowed.
- Start the measured arc around `M24 106 Q78 23 136 106`, with a 5-unit rounded
  stroke. The arc enters and exits close to the horizon without touching the
  outer hexagon.
- Place the amber solar marker near the arc apex, initially at `80,51` with an
  8-unit radius. It replaces the center ruler tick.
- Sample six approximately equal arc-length stations around the center point,
  three on each side. Draw ticks perpendicular to the local arc tangent. Use
  two 6-unit major ticks and four 4-unit minor ticks with a 2-unit rounded
  stroke. Keep the sequence symmetric unless optical balance requires a small
  correction.
- The tick rhythm signifies measured intervals, not calibrated lux values or
  literal clock hours. Do not add numerals, an edge ruler, or a prism beam.
- Keep at least one outer-stroke width of clear space between all inner marks
  and the hexagon boundary.

### Color and reduction

| Element | Light | Dark | Monochrome |
| --- | --- | --- | --- |
| Hex outline and measured arc | `#0B6675` | `#76D2DD` | `currentColor` |
| Day/night lower field | `#E6EFEB` | `#16343A` | `currentColor` at low tint or open field |
| Solar marker | `#C36A1D` | `#F2B66D` | `currentColor` |
| Interior surface | `#FFFFFF` or transparent on page | `#10282F` or transparent on surface | Transparent |

- At 48 px and above, show the full measured arc and all six ticks.
- From 24–47 px, retain the sun, two major ticks, horizon, and arc.
- Below 24 px, use the hex, simplified arc, and sun only. Test the favicon at
  16, 32, and 48 px rather than mechanically scaling detail.
- The one-color and reversed marks must remain recognizable without the amber
  cue. The silhouette and arc geometry therefore carry the identity.
- Pair the mark with a Source Sans 3 wordmark set as `LightLogWeb`, weight 600,
  with optical rather than default tracking. Do not place text inside the
  hexagon.
- Minimum clear space around the standalone mark is one quarter of the mark's
  height. Do not rotate, stretch, recolor, or combine it with attribution
  logos.
- Before finalization, perform a similarity/trademark check and review the mark
  in the app header, pkgdown, monochrome print, and small-icon contexts.

## 10. bslib and Sass mapping

The implementation targets the package's locked **bslib 0.11.0** and
**Bootstrap 5**. Prefer native bslib components and Bootstrap utilities before
scoped Sass. No bespoke JavaScript is required for this design system.

### Theme mapping

| Design decision | bslib/Bootstrap target |
| --- | --- |
| Light page and text | `bs_theme(bg = ..., fg = ..., version = 5)` / `$body-bg`, `$body-color` |
| Primary action | `primary` / `$primary`; derive accessible hover and active states |
| Muted/secondary action | `secondary` / `$secondary`; do not reuse it blindly for small text |
| Statuses | `$success`, `$warning`, `$danger`; map informational/running treatment to action color |
| Surface | Custom `--llw-bg-surface`; map `$card-bg`, dropdown, modal, and popover surfaces |
| Structural boundary | `$border-color` from `--llw-border` |
| Control boundary | `$input-border-color` and equivalent control variables from `--llw-border-control` |
| Focus | `$focus-ring-width: 0.1875rem`, full-opacity focus token, `0.1875rem` visual offset in scoped rules |
| Typography | `base_font`, `heading_font`, and `code_font` all register the same local Source Sans 3 family |
| Radius | `$border-radius: 0.375rem`, `$border-radius-lg: 0.75rem`, custom shell radius `1.125rem` |
| Shadows | Disable routine card shadow; retain one neutral overlay shadow token |
| Dark mode | Theme-scoped semantic tokens under bslib/Bootstrap's dark-mode attribute; do not duplicate component rules |

### Implementation rules

- Make a future `_brand.yml` the cross-output brand declaration where its
  schema covers the decision. Keep LightLogWeb-only behavior in external Sass.
- Define all `--llw-*` properties once per color mode. Map Bootstrap variables
  into those values and style custom components from the semantic properties.
- Create an internal, unexported theme factory, suggested as
  `lightlogweb_theme()`, so no public R API changes.
- Store packaged font and shipping brand assets under `inst/app/www/` and
  attach them through the package's resource or `htmlDependency()` pattern.
  Do not depend on remote font or asset hosts.
- Keep nontrivial Sass external and compile it through bslib/R sass. Use syntax
  compatible with LibSass; do not use Dart Sass module syntax.
- Scope custom selectors to `.llw-*` component classes. Avoid deep selectors,
  IDs, `!important`, and undocumented bslib/widget DOM internals.
- Use bslib page, navigation, sidebar, card, accordion, toolbar, modal, toast,
  value-box, layout, and task-button primitives before creating wrappers.
- Pin the Bootstrap major version and verify DT, gt, Shiny inputs, plot output,
  task buttons, notifications, and modals in both modes.
- Treat `_brand.yml`, Sass, fonts, theme helpers, and component-gallery code as
  implementation outputs of the next approved slice, not outputs of this
  brief.

## 11. Component-gallery inventory

The future gallery is a build-ignored development app, suggested as
`dev/component-gallery-app.R`. It demonstrates visual and interaction rules
with representative personal-light-exposure data; it does not establish the
final navigation, screen sequence, or information architecture.

Each specimen is shown in light and dark modes and includes default, hover,
focus, active, disabled, validation, long-text, and narrow-width cases where
relevant.

### Required specimens

- Brand mark: full-color, dark, reversed, monochrome, 16/24/32/48 px reduction,
  horizontal wordmark, and attribution separation.
- Type: full hierarchy, paragraphs, links, labels, long translated-like text,
  timestamps, units, revisions, and tabular numerals.
- Navigation: product identity, active/inactive destinations, skip link,
  sidebar trigger, breadcrumb where justified, and keyboard order.
- Actions: primary, secondary, quiet, destructive, icon-only with accessible
  name, split groups, task button, download, reset, undo, and disabled state.
- Forms: text, numeric, select, checkbox, radio, switch, date/time, file upload,
  help, required, valid, warning, invalid, and error-recovery examples.
- Containers: card, selected card, grouped settings, accordion, sidebar,
  toolbar, popover, modal, notification/toast, and callouts.
- Data: compact and comfortable tables, sorting, selected row, empty cells,
  missing values, long identifiers, sticky header, responsive scroll region,
  and export action.
- Plots: single series, comparison, confidence interval, threshold, day/night
  band, missing interval, dense participant case, linear/symlog indication,
  direct labels, and accessible table/summary alternative.
- System states: empty, idle, queued, running, finalizing, complete, warning,
  error, cancelled, stale, retry, no-network, unavailable feature, and no-data
  result.
- Accessibility: visible focus tour, semantic headings, labelled regions,
  live-status example, reduced-motion behavior, contrast swatches, 200% zoom,
  and 320 px no-overflow case.

## 12. Asset matrix

The editable master is vector geometry. Generated moodboards remain
non-shipping reference material; their prompts are recorded in
[`design-workshop/moodboard-prompts.md`](design-workshop/moodboard-prompts.md).

| Asset | Canonical/derived | Required variants | Intended target |
| --- | --- | --- | --- |
| `dev/brand/lightlogweb-logo-master.svg` | Canonical editable source | Full construction layers, editable wordmark, provenance metadata | Design source only |
| `lightlogweb-mark.svg` | Derived vector | Light, dark/reversed, monochrome/current-color | App and reusable UI |
| `lightlogweb-wordmark-horizontal.svg` | Derived vector | Light, dark/reversed, monochrome | App header and documents |
| `lightlogweb-wordmark-stacked.svg` | Derived vector | Light, dark/reversed, monochrome | Square/compact placements |
| `man/figures/logo.svg` | Derived vector | Pkgdown/package context | Pkgdown and repository identity |
| `favicon.svg` | Simplified derived vector | Auto/adaptive where supported | Browser icon |
| `favicon-16.png`, `favicon-32.png`, `favicon-48.png` | Derived raster | Tested pixel-hinted reductions | Browser fallbacks |
| `apple-touch-icon.png` | Derived raster | 180 × 180 | Touch bookmark |
| `icon-192.png`, `icon-512.png` | Derived raster | 192 × 192 and 512 × 512 | Manifest/install contexts if used |
| `lightlogweb-mark-1024.png`, `lightlogweb-mark-2048.png` | Derived raster | Transparent and approved background | High-resolution export |
| Source Sans 3 WOFF2 and license | Vendored dependency | Normal variable; italic only if used | App and gallery typography |
| `brand-provenance.md` | Canonical record | Font license, heritage reference, derivation commands, checksums | Maintenance and release review |

Shipping derivatives belong under an `inst/app/www/brand/` hierarchy unless
the package's final dependency wrapper establishes a different single source.
Do not retain the current raster logos as parallel canonical identities after
the new assets are approved and migrated.

## 13. Next implementation slice

This brief authorizes planning, not silent completion, of the following
implementation sequence:

1. **Theme foundation:** vendor the exact Source Sans 3 assets and license;
   implement the semantic light/dark tokens, future `_brand.yml`, internal
   bslib theme factory, and external Sass without changing public interfaces.
2. **Gallery:** build the isolated component gallery from the inventory above
   and use it to refine responsive density, component states, plots, tables,
   focus, and dark mode.
3. **Logo master:** draw the Measured Day Arc as editable SVG; review the full,
   small, monochrome, dark, and wordmark variants before generating final
   derivatives.
4. **Integration:** apply the approved theme and assets to the existing app
   shell, DT/gt output, plots, task states, notifications, and pkgdown surface.
5. **Verification and owner gate:** run automated contrast and source checks,
   then complete real-browser review before declaring Milestone 2 accepted.

The gallery and logo review may iterate independently, but final derivatives
must come from the accepted master. The app shell must not be used as the only
theme test because that would prematurely make current information architecture
look final.

## 14. Milestone 2 acceptance checklist

### Brand and design system

- [ ] The implementation matches Circadian Field rather than blending all four
      directions indiscriminately.
- [ ] Source Sans 3 is used throughout visible UI and plots, self-hosted from a
      pinned file with its license.
- [ ] Light and dark semantic tokens match this brief or have a documented,
      re-tested adjustment.
- [ ] Cards, navigation, tables, forms, plots, focus, spacing, and every system
      state appear in the gallery.
- [ ] No sample screen is treated as approval of final application information
      architecture.

### Logo and assets

- [ ] The master mark is an editable flat SVG with the hex window, day/night
      boundary, sun-path, solar marker, and ticks perpendicular to the arc.
- [ ] The tick system reads as measured time without resembling a beam, literal
      ruler, clock face, or quantitative lux scale.
- [ ] Full, reduced, one-color, reversed, light, and dark variants remain
      recognizable at their stated sizes.
- [ ] Header, pkgdown, favicon, monochrome, and high-resolution PNG derivatives
      are generated from one accepted master and recorded in provenance.
- [ ] LightLogR heritage and all third-party/institutional identities remain
      clearly separate and correctly attributed.

### Accessibility and resilience

- [ ] WCAG 2.2 AA contrast is measured for normal and large text, controls,
      focus rings, chart marks, links, disabled states, and all system states in
      light and dark modes.
- [ ] Every status and plot distinction has text, icon, line, shape, or pattern
      reinforcement beyond color.
- [ ] All actions work by keyboard in a logical order with a clearly visible
      focus indicator.
- [ ] Async updates use appropriate live-region behavior without stealing
      focus; error and validation messages identify a recovery path.
- [ ] Reduced motion removes nonessential animation without hiding state.
- [ ] The gallery and integrated app have no document-level horizontal clipping
      at 320, 768, and 1440 px or at 200% zoom.

### Technical verification

- [ ] The design system uses native bslib/Bootstrap behavior first, scoped Sass
      second, and no unapproved bespoke JavaScript.
- [ ] Sass compiles through the locked R sass/LibSass toolchain.
- [ ] DT, gt, Shiny controls, plots, task buttons, notifications, modals, and
      navigation are tested in both modes.
- [ ] Automated tests cover token output and critical HTML semantics; a real
      browser reports no LightLogWeb console errors.
- [ ] The development gallery and canonical design sources remain excluded from
      the built R package; only intentional runtime assets ship.
- [ ] No R public API changes are introduced for the theme or gallery.
- [ ] The owner approves the implemented design guide, gallery, and final logo
      before Milestone 2 is marked complete.

## 15. Explicitly deferred

This decision brief does not create or approve final SVG/PNG logo files,
derived favicons, Source Sans 3 binaries, `_brand.yml`, Sass, R theme helpers,
the bslib component gallery, or changes to the production Shiny UI. It also
does not decide final information architecture. Work pauses at this brief until
the next implementation slice is approved.
