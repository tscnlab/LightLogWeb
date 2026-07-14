# LightLogWeb

LightLogWeb is the production Shiny interface and reproducible console layer for [LightLogR](https://tscnlab.github.io/LightLogR/). It supports the research workflow from logger import through quality review, preparation, grouping, metrics, figures, exports, and local project bundles.

Shiny remains reactive, but it does not contain a second analysis implementation. Reactives decide when to run; exported deterministic functions perform the work. The same recipe can therefore be run in the application, from an R console, or as generated R code.

## Install and launch

```r
# install.packages("remotes")
remotes::install_github("tscnlab/LightLogR")
remotes::install_github("tscnlab/LightLogWeb")

library(LightLogWeb)
LightLogWeb()
```

LightLogWeb requires R 4.3 or newer and LightLogR 0.10.3 or newer.

## Console workflow

```r
data <- llw_dataset(
  LightLogR::sample.data.environment,
  metadata = list(variable = "MEDI", variable_unit = "lx", timezone = "Europe/Berlin"),
  name = "sample.data.environment"
)

recipe <- llw_recipe() |>
  llw_recipe_add("preparation", list(gap_policy = "explicit_na", interval = "5 min")) |>
  llw_recipe_add("grouping", list(dimensions = c("participant", "date"))) |>
  llw_recipe_add(
    "metric",
    list(metrics = list(
      list(id = "duration_above_threshold", parameters = list(threshold = 250)),
      list(id = "dose")
    ))
  ) |>
  llw_recipe_add("visualization", list(type = "timeline", id = "Participant"))

analysis <- llw_run(data, recipe)
analysis$metrics
analysis$plots
writeLines(llw_build_script(recipe, data_expression = "LightLogR::sample.data.environment"))
```

Use `llw_metric_registry()` to inspect metric requirements, defaults, units, cautions, and direct links to the official LightLogR function documentation. Spectral integration and nvRC/nvRD analyses are intentionally left to custom modules because their input contracts require more than one selected measurement variable.

## Privacy and projects

`llw_save_project()` creates a local `.llw` ZIP bundle. Raw and prepared participant data are excluded by default; data-free projects retain source fingerprints and request matching source files when reopened. No authentication, accounts, database, or server-side user storage are added by the package.

## Extensions and testing

Startup-provided `llw_module()` objects add namespaced Shiny screens while keeping validation, analysis, and code rendering callable from the console. See the [module-authoring vignette](https://tscnlab.github.io/LightLogWeb/articles/module-authoring.html).

Offline checks include small, deidentified, CC BY 4.0 fixtures from TUM, KNUST, UCR, and RISE. Their repository commits, DOIs, extraction details, and checksums are recorded in the installed fixture manifest. Live `melidosData` tests are opt-in and run separately.
