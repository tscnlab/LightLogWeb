# LightLogWeb clickable mockup

## Executive direction

LightLogWeb should feel like a scientific workspace, not a generic business dashboard. The central object is always a time series with a measurement context. The first screen after import therefore answers four questions before it offers calculations:

1. What data are present?
2. When are they present?
3. Are the timestamps and intervals trustworthy?
4. Which analysis decisions have already been made?

The mockup translates the established LightLogR workflow—import, inspect and prepare, group, calculate metrics, visualize—into a progressive interface. A persistent analysis recipe connects every interface choice to readable R code. This is both an onboarding device and a reproducibility feature: users can begin without code but never become trapped in a proprietary workflow.

Run the prototype from the repository root with:

```r
shiny::runApp("mockup")
```

The prototype is deliberately isolated from the production modules in `R/`.

## Information architecture

### Landing page

The landing page has one primary message: wearable light data can be explored without hiding the analysis. It offers two paths:

- **Explore the demo** opens the complete workspace with `LightLogR::sample.data.environment`.
- **Start an import** opens a simulated three-step configuration flow for files/device, time zone/participant IDs, and import preview.

The import wizard does not parse arbitrary files. It demonstrates layout, validation order, copy, and the transition into the workspace. This is stated explicitly in the interface.

### Workspace shell

The workspace uses three stable regions:

- A compact header for the active dataset, session state, help, recipe access, and R-script download.
- A workflow sidebar for Overview, Prepare, Group, Metrics, and Visualize.
- A wide working surface with one dominant task per screen.

Metadata, raw data, the recipe, and adding datasets remain accessible but are not peers of the scientific workflow. This removes the current tension between high-value analysis steps and low-level tables competing in one tab row.

### Screen inventory

| Screen | Primary job | Important design decision |
|---|---|---|
| Overview | Understand shape and trustworthiness | Data health and temporal pattern come before metrics. |
| Prepare | Make transformations explicit | Steps are ordered; preview never overwrites the original. |
| Group | Declare the analytical grain | Grouping is separated from metrics so results remain interpretable. |
| Metrics | Select a measure by question/family | Requirements and parameters are visible; thresholds are not presented as universal targets. |
| Visualize | Match a view to an exploratory task | A gallery names what each visualization is good for. |
| Recipe | Inspect and export reproducible code | Every enabled step becomes readable LightLogR code with relative paths. |
| Metadata | Keep context beside data | The mockup highlights future completeness checks for device and protocol reporting. |
| Raw table | Verify source values | The table is available without becoming the main dashboard. |

## Visual system

The “scientific daylight” direction combines a restrained research interface with references to dawn, daylight, and temporal cycles.

### Tokens

| Role | Token | Use |
|---|---|---|
| Primary text | `#0B3041` | Headings, essential labels, axes |
| Interaction | `#0B789C` | Primary actions, active navigation, participant series |
| Sky support | `#85C7D6` / `#DFF1F5` | Selected states, ranges, soft context |
| Dawn accent | `#E9A45C` / `#FFF1DD` | Solar emphasis, cautions, secondary series |
| Success | `#26735B` / `#E5F3ED` | Passed quality checks and applied recipe steps |
| Canvas | `#F5F8FA` | Low-contrast application background |
| Surface | `#FFFFFF` | Cards, controls, modal content |

Color is never the sole indicator of state. Icons and explicit language—“None detected”, “Preview only”, “Added to recipe”—carry the same meaning.

### Type and density

- System fonts avoid a web-font dependency and remain native across operating systems.
- Headings are compact and moderately weighted; long explanatory text is secondary.
- The overview has four small summary surfaces, one quality strip, one dominant plot, and one availability view. It avoids a dense wall of KPI cards.
- Advanced parameters live in accordions or appear only when required.

### Responsive behaviour

- Desktop: persistent workflow sidebar and paired control/preview columns.
- Tablet: sidebar uses bslib’s collapsible behaviour; paired scientific panels stack where needed.
- Mobile: the header reduces to the dataset and essential download action; cards, metric catalog, import facts, and visualization choices become single-column.
- No screen relies on hover alone. Controls remain native Shiny inputs with visible focus indicators.

## Functional prototype versus production behaviour

### Implemented in the mockup

- Landing and workspace navigation.
- Simulated multi-step import configuration.
- Real LightLogR sample data and dynamically rendered plots.
- Quality, coverage, epoch, and date-range presentation based on the bundled dataset.
- Interactive preparation preview with aggregation interval and range choices.
- Grouping preview for Id, date, day type, clock window, and photoperiod context.
- Searchable metric catalog spanning level/dose, threshold/duration, timing, rhythmicity, prior-history, and spectral families. Threshold duration, dose, centroid, bright/dark periods, IV, IS, and exponential moving average use real preview calculations; spectral integration clearly requests the spectral input that the bundled demo does not contain.
- Eight working visualization modes: availability, day overlay, timeline, heatmap, doubleplot, aggregated daily profile, histogram, and cumulative distribution.
- Persistent recipe controls and a real `.R` download generated from current interface values.
- Metadata and raw-table views.

### Intentionally deferred to production

- Parsing uploaded files and device-specific validation.
- Mutating the in-session dataset when “Apply” is selected.
- Undo/redo and branching transformation history.
- Persistent projects, authentication, or server-side storage.
- Real plot, plotted-data, processed-data, and project-bundle downloads.
- Full metadata schema and validation.
- Production non-wear detection or diary merging.
- Publication templates and automated methods text.

## Package and component mapping

| Interface need | Proposed implementation |
|---|---|
| App shell, responsive sidebar, hidden navigation | `bslib::page_fillable()`, `layout_sidebar()`, `sidebar()`, `navset_hidden()` |
| Summary surfaces and panels | `bslib::value_box()`, `card()`, `layout_columns()`, `layout_column_wrap()` |
| Progressive configuration | `bslib::accordion()`, `accordion_panel()`, `input_switch()` |
| Icons | `bsicons::bs_icon()` |
| Interval selection | `shinyWidgets::sliderTextInput()` |
| Reactive plots | `shiny::plotOutput()` and `renderPlot()` with `ggplot2` and LightLogR-derived data |
| Metric previews | LightLogR metric functions called on the participant stream |
| Raw-data inspection | `DT::datatable()` |
| Metadata table | `gt::gt()` |
| R handoff | `shiny::downloadHandler()` with a pure script builder |

No additional JavaScript framework is used.

## Research-derived requirements

The interface recommendations below are not a new reporting standard. They translate recurring requirements from current guidance and the LightLogR teaching workflow into product behaviour.

### Inspect raw temporal structure before summarizing

The current [Technical guide for wearable optical radiation dosimetry and visual experience assessment](https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/) recommends raw time-series visualization and distribution checks during cleaning. The [LightLogR visualization tutorial](https://tscnlab.github.io/LightLogR/articles/Visualizations.html) likewise begins with availability and overview plots before moving into day, timeline, and heatmap views.

**Product consequence:** the overview is plot-led. Histograms, heatmaps, day views, and timelines remain first-class visualization options rather than hidden export formats.

### Treat time, sampling, and gaps as analytical inputs

The Researcher Guide distinguishes explicit missing values, implicit gaps, and irregular observations, noting that implicit gaps are especially problematic for duration calculations. It also stresses that aggregation choices—resolution, binning method, numeric handler, and treatment of missing values—change results.

The [LightLogR import and cleaning tutorial](https://tscnlab.github.io/LightLogR/articles/Import.html) demonstrates checking dominant intervals, DST behaviour, gaps, irregular timestamps, `gap_handler()`, `aggregate_Datetime()`, and partial-data removal.

**Product consequence:** these concepts receive separate, named checks. “Fill gaps” is deliberately worded as converting absent timestamps to explicit `NA`, not imputing exposure. The generated R script records the epoch and numeric handler.

### Make non-wear and invalidation auditable

Wearable measurements may be invalid because of non-wear, occlusion, sensor range, or observations outside the protocol window. Non-wear is not universally identifiable from light alone, and low or zero readings can be valid nighttime exposure.

**Product consequence:** the mockup offers evidence sources rather than an automatic “detect non-wear” promise. A production design should support device flags, external event logs, multimodal/variance rules, manual review, and preservation of an invalidation reason column.

### Keep device and protocol context with the analysis

The Researcher Guide recommends reporting device make/model, wearing location, sampling interval, measured quantities, calibration characteristics, location/season, time zone and DST, wear duration/instructions, compliance criteria, preprocessing, missing-data handling, functions, and software versions. The [V3 framework for wearable light sensors](https://pmc.ncbi.nlm.nih.gov/articles/PMC9806438/) further emphasizes verification, analytical validation, and clinical validation rather than assuming every device output is interchangeable.

**Product consequence:** metadata is not just plot labelling. A future completeness view should organize study-, participant-, device-, and dataset-level context, expose missing reporting fields, and carry relevant values into exports and generated methods text.

### Use location and photoperiod as analytical context

The [LightLogR photoperiod tutorial](https://tscnlab.github.io/LightLogR/articles/photoperiod.html) requires dates, time zone, coordinates, and an explicit photoperiod definition. The Researcher Guide describes photoperiod as a particularly accessible contextual variable for light field studies.

**Product consequence:** photoperiod cannot silently activate without location and time-zone context. It is a grouping choice with visible dependencies, not decorative night shading alone.

### Expose metric flexibility rather than imply consensus

The [LightLogR metrics tutorial](https://tscnlab.github.io/LightLogR/articles/Metrics.html) demonstrates that metric interpretation depends on grouping, parameters, and whether periods can cross midnight. Current guidance also notes that the field does not have one universally validated metric set for all outcomes.

**Product consequence:** metrics are grouped by analytical idea—level/dose, duration/threshold, timing, rhythmicity, prior history, and spectrum. Function names, input requirements, units, defaults, and current groups remain visible. Health-oriented recommendations such as the [daytime, evening, and nighttime light recommendations](https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571) can later be offered as clearly cited comparison profiles, not silently embedded defaults.

### Preserve code and software provenance

The Researcher Guide treats analysis code as a research output and recommends reporting routines, functions, software, versions, and open-source availability. The [LightLogR course series](https://tscnlab.github.io/LightLogR_webinar/) explicitly teaches open, reproducible workflows from import through metrics.

**Product consequence:** the R handoff is always available, not a final export afterthought. It uses relative input paths, explicit parameter values, package/version comments, and `sessionInfo()`.

### ENLIGHT is informative but not a field-dosimetry checklist

The [ENLIGHT consensus checklist](https://pmc.ncbi.nlm.nih.gov/articles/PMC10704221/) was developed for laboratory studies of ocular light interventions and non-visual physiology. Its emphasis on light-source specification, spectral/photometric quantities, timing, exposure geometry, background light, and protocol context is useful for metadata design. It should not be presented as a complete reporting checklist for unconstrained wearable field dosimetry, where wear location, occlusion, non-wear, device validation, sampling, gaps, and participant behaviour introduce different problems.

**Product consequence:** LightLogWeb may link ENLIGHT-relevant fields to laboratory/intervention workflows, while field-study readiness should be based on wearable-specific guidance and future domain consensus.

## R handoff contract

The generated script is designed to be understandable before it is executed. It contains:

- A project-relative `data/` file search.
- The selected device, file-format version, time zone, DST behaviour, and ID method.
- Range invalidation that retains timestamps.
- Explicit gap handling without imputation.
- Selected aggregation interval and numeric function.
- Daily missingness threshold.
- Photoperiod coordinates, solar-depression definition, and declared groups.
- The selected metric call and parameters.
- A LightLogR or ggplot visualization matching the chosen view.
- LightLogR/R version comments and `sessionInfo()`.

No Shiny input IDs or prototype-only state names are emitted. A production implementation should build this script from the same typed analysis-recipe object that executes the web analysis, preventing divergence between results and exported code.

## Recommended production roadmap

1. **Typed recipe and immutable data stages**
   - Define schema-validated import, metadata, preprocessing, grouping, metric, and visualization steps.
   - Keep original, prepared, grouped, and result objects separate.
   - Add undo/redo, step disabling, reordering where valid, and dependency invalidation.

2. **Production import and validation**
   - Reuse current LightLogWeb import modules and LightLogR device support.
   - Add file-format/version detection, sample previews, mixed-device prevention, clear DST warnings, and import reports.

3. **Quality and annotation workspace**
   - Implement gap/irregular/missing tables and plots per stream and participant-day.
   - Add auditable non-wear/occlusion annotations and reason codes.
   - Support auxiliary time-stamped data such as sleep/wake, diaries, protocol events, and environmental measurements.

4. **Metric and visualization registry**
   - Derive metric cards and parameter forms from a central registry of LightLogR functions, input types, units, requirements, help links, and compatible groupings.
   - Reuse the same registry for script generation and validation.

5. **Projects and exports**
   - Save/load recipes without embedding sensitive raw data by default.
   - Export processed data, metrics, figures, code, metadata, and a manifest as a documented project bundle.
   - Add publication-ready figure/table presets and generated methods text that remains editable and fully sourced.

## Accessibility acceptance points

- All actions have visible text; icon-only actions retain accessible names.
- Native inputs preserve keyboard operation and expected screen-reader semantics.
- Focus indicators are not removed and remain visible against all surfaces.
- Status combines text, icon, and color.
- Plot axes include quantity and unit; plot context is repeated in nearby visible text.
- Layout remains usable at 320 px without horizontal page scrolling.
- Motion is reduced when the operating system requests it.
- High-contrast user preferences strengthen borders and secondary text.

## Primary references

- [LightLogR documentation and tutorials](https://tscnlab.github.io/LightLogR/)
- [Import and cleaning](https://tscnlab.github.io/LightLogR/articles/Import.html)
- [Metrics](https://tscnlab.github.io/LightLogR/articles/Metrics.html)
- [Visualizations](https://tscnlab.github.io/LightLogR/articles/Visualizations.html)
- [Durations, states and clusters](https://tscnlab.github.io/LightLogR/articles/states.html)
- [Photoperiod](https://tscnlab.github.io/LightLogR/articles/photoperiod.html)
- [Interactive LightLogR course series](https://tscnlab.github.io/LightLogR_webinar/)
- [Technical guide for wearable optical radiation dosimetry and visual experience assessment](https://rda-wg-visualexperiencedata.github.io/ResearcherGuide/)
- [Verification, analytical validation and clinical validation of wearable dosimeters and light loggers](https://pmc.ncbi.nlm.nih.gov/articles/PMC9806438/)
- [How to report light exposure in human chronobiology and sleep research experiments](https://pmc.ncbi.nlm.nih.gov/articles/PMC6609447/)
- [ENLIGHT consensus checklist](https://pmc.ncbi.nlm.nih.gov/articles/PMC10704221/)

Research sources and online documentation were reviewed for this design on 13 July 2026.
