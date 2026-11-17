# Prepare workspace closeout

On 2026-11-17 the owner accepted R11 and selected the redesigned workspace as
the default Prepare page. It follows the five slices of Milestone 8 accepted
in commit `7ed29e2`. The reviewed R11 source index covers 226 files and has
SHA-256 `c3f60f7a0f49982d7bb7e409f961dd66cc6f67eb972eaaee1596654ea5809652`.

## Working with a recipe

1. Open **Prepare** with a dataset selected. The recipe begins with **Original
   data**. Hover over that card or activate it with a click, Enter or Space
   to see how the recipe uses the source.
2. Choose **Add step** below the last step and pick an operation. Each saved
   step carries the same icon as its operation in the picker.
3. Save the settings to preview the recipe. On a wide screen the editor stays
   beside the preview so you can change a setting and save again. On smaller
   screens they stack. **Expand preview** opens the full-width result.
4. Select a saved step to edit, reorder, disable or remove it. **Review
   result** preserves unsaved settings and **Return to step** reopens the
   editor. **Apply** commits the saved, previewed recipe to the selected dataset.

The sidebar, editor and preview share a top edge in the wide layout. Feedback
and recovery actions sit above them. Keyboard navigation moves focus to the
heading of the chosen view. Saving in the editor keeps focus there.

## Operation changes accepted during review

**Variability** supports windows starting at, centred on or ending at each
reading. A forward window includes its starting reading and excludes the far
endpoint. Its decision applies to the target at the start. Coverage, timestamp
and observation-count checks run before the constant-window policy. A user
can keep or mask a sufficiently observed constant window, including an
all-zero window, independently of the policy for unevaluable windows. The
default follows the threshold rule and preserves earlier recipe behaviour.

**Aggregate** applies the missing-readings policy separately to each numeric,
categorical and logical column. The threshold uses the proportion missing
among observations in the bin. Use **Gaps** first to count absent timestamps.
All-missing summaries retain the column's missing value type and factor
summaries preserve their levels.

## Verification

Independent browser checks covered operation selection, repeated edits,
automatic previews, draft recovery, cancellation and retry, reordering, Undo,
Apply and derived-variable metadata. Layout and keyboard checks included phone
and ultrawide widths in both themes. The final R11 review verified the filled
**Add step** card below the source or final step, its keyboard focus, and
operation-picker activation with a click or Enter.

The variability checks compared applied data with previews using opposite
policies for constant and unevaluable windows. An independent R check covered
seven aggregation scenarios. The R regression suite covers the control
bindings shared with the classic layout, the Duration, Valid range and
Variability recipe, preview recovery, and the default public launcher.

At closeout, a fresh browser session launched through `app.R`, loaded the
built-in example, saved a Duration step, previewed it automatically and applied
it. The source package passed `R CMD check --no-manual --no-tests` with R
4.6.1. The tests ran separately with `testthat::test_local()` and recorded
7,953 passing assertions. One GLC retry test timed out at
`test-glc-preview.R:4501` on both this version and the previous `dev` commit.
The skipped metadata browser-protocol test passed separately with Node 24.

Earlier reviews recorded a second save on one rapid two-field Transform edit.
Ordinary typing saved both fields on the first click on retest. At narrow
widths, wrapped labels can offset the summary-number baselines.

## Running the app

From the repository root, run `shiny::runApp(".")` and select **Prepare**.
The offline harness uses the same default with bundled examples.

```r
source("dev/prepare-flow-app.R")
shiny::runApp(prepare_flow_app())
```

The internal `lightlogweb_app(prepare_presentation = "classic")` option and
the earlier isolated slice showcase support comparisons with the classic layout.
