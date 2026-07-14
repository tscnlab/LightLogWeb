# scripts, manifests, and errors remain reviewable

    Code
      cat(llw_build_script(recipe, data_expression = "data.frame()",
        include_session_info = FALSE), sep = "\n")
    Output
      # LightLogWeb reproducible analysis
      # Recipe schema: 1.0.0
      library(LightLogWeb)
      library(LightLogR)
      
      data <- data.frame()
      if (!inherits(data, "llw_dataset")) data <- llw_dataset(data)
      dataset <- data
      metric_results <- list()
      plot_results <- list()
      module_results <- list()
      if (!exists("modules")) modules <- list()
      
      # Metric
      metric_results[["dose"]] <- llw_metrics(dataset, metrics = list(list(id = "dose")))
      
      analysis <- list(dataset = dataset, metrics = metric_results, plots = plot_results, modules = module_results)
      analysis

---

    Code
      str(manifest, max.level = 2, give.attr = FALSE)
    Output
      List of 10
       $ format        : chr "LightLogWeb manifest"
       $ schema_version: chr "1.0.0"
       $ generated_at  : chr "<generated-at>"
       $ dataset       :List of 5
        ..$ name               : chr "dataset"
        ..$ metadata           :List of 9
        ..$ rows_raw           : int 2880
        ..$ rows_prepared      : int 2880
        ..$ source_fingerprints: list()
       $ recipe        :List of 1
        ..$ :List of 8
       $ outputs       : list()
       $ result        : NULL
       $ versions      :List of 3
        ..$ LightLogWeb: chr "0.1.0"
        ..$ LightLogR  : chr "0.10.3"
        ..$ R          : chr "4.5.0"
       $ citations     :List of 2
        ..$ LightLogR  : chr "https://tscnlab.github.io/LightLogR/"
        ..$ LightLogWeb: chr "https://tscnlab.github.io/LightLogWeb/"
       $ session_info  : chr "<session-info>"

---

    Code
      llw_recipe_add(llw_recipe(), "metric", list(not_a_parameter = 1))
    Condition
      Error in `llw_abort()`:
      ! Unknown parameters for metric step: not_a_parameter.

