pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

gallery_section <- function(id, eyebrow, title, description, ...) {
  tags$section(
    id = id,
    class = "llw-gallery-section",
    tags$header(
      tags$p(class = "llw-eyebrow", eyebrow),
      tags$h2(title),
      tags$p(class = "llw-view-lede", description)
    ),
    ...
  )
}

gallery_specimen <- function(title, ..., class = NULL) {
  tags$article(
    class = paste(c("llw-gallery-specimen", class), collapse = " "),
    tags$h3(title),
    ...
  )
}

gallery_token_swatch <- function(name, value) {
  tags$div(
    class = "llw-token-swatch",
    tags$div(
      class = "llw-token-swatch__chip",
      style = htmltools::css(`--llw-swatch` = value),
      `aria-hidden` = "true"
    ),
    tags$div(
      class = "llw-token-swatch__label",
      tags$span(name),
      tags$code(value)
    )
  )
}

gallery_token_panel <- function(mode) {
  tokens <- lightlogweb_tokens(mode)
  tags$div(
    class = "llw-gallery-specimen",
    `data-bs-theme` = mode,
    tags$h3(paste(tools::toTitleCase(mode), "mode")),
    tags$div(
      class = "llw-token-grid",
      Map(gallery_token_swatch, names(tokens), unname(tokens))
    )
  )
}

gallery_spacing_panel <- function() {
  spaces <- c(
    space_1 = 4,
    space_2 = 8,
    space_3 = 12,
    space_4 = 16,
    space_6 = 24,
    space_8 = 32,
    space_12 = 48,
    space_16 = 64
  )
  tags$div(
    class = "llw-spacing-demo",
    Map(
      function(name, pixels) {
        tags$div(
          class = "llw-spacing-row",
          tags$code(paste0("--llw-", gsub("_", "-", name))),
          tags$div(
            class = "llw-spacing-bar",
            style = htmltools::css(
              `--llw-demo-width` = paste0(pixels * 3, "px")
            ),
            `aria-hidden` = "true"
          ),
          tags$span(paste0(pixels, " px"))
        )
      },
      names(spaces),
      spaces
    )
  )
}

gallery_identity <- function() {
  gallery_section(
    "identity",
    "01 · Identity",
    "Measured daylight",
    paste(
      "A flat hexagonal window connects LightLogWeb to LightLogR while the",
      "day boundary, solar point, and perpendicular ticks make the new mark",
      "distinctly about light measured across time."
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide",
      gallery_specimen(
        "Primary lockup",
        tags$div(
          class = "llw-brand-stage",
          tags$img(
            src = lightlogweb_asset_url(
              "brand/lightlogweb-wordmark-horizontal-light.svg"
            ),
            alt = "LightLogWeb measured day arc and wordmark",
            width = "560"
          )
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "The app header uses the live Source Sans 3 wordmark plus the same mark asset."
        )
      ),
      gallery_specimen(
        "Dark and reversed",
        tags$div(
          class = "llw-brand-stage llw-brand-stage--dark",
          tags$img(
            src = lightlogweb_asset_url(
              "brand/lightlogweb-wordmark-horizontal-dark.svg"
            ),
            alt = "LightLogWeb dark-mode wordmark",
            width = "560"
          )
        )
      ),
      gallery_specimen(
        "One-color reduction",
        tags$div(
          class = "llw-brand-stage",
          tags$img(
            src = lightlogweb_asset_url(
              "brand/lightlogweb-wordmark-horizontal-monochrome.svg"
            ),
            alt = "Monochrome LightLogWeb wordmark",
            width = "560"
          )
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "Silhouette, arc, and spacing retain the identity without the amber cue."
        )
      ),
      gallery_specimen(
        "Small-size reductions",
        tags$div(
          class = "llw-brand-size-row",
          tags$div(
            class = "llw-brand-size",
            tags$img(
              src = lightlogweb_asset_url("brand/favicon-16.png"),
              alt = "",
              width = "16",
              height = "16"
            ),
            "16 px"
          ),
          tags$div(
            class = "llw-brand-size",
            tags$img(
              src = lightlogweb_asset_url("brand/favicon.svg"),
              alt = "",
              width = "24",
              height = "24"
            ),
            "24 px"
          ),
          tags$div(
            class = "llw-brand-size",
            tags$img(
              src = lightlogweb_asset_url("brand/favicon-32.png"),
              alt = "",
              width = "32",
              height = "32"
            ),
            "32 px"
          ),
          tags$div(
            class = "llw-brand-size",
            tags$img(
              src = lightlogweb_asset_url("brand/favicon-48.png"),
              alt = "",
              width = "48",
              height = "48"
            ),
            "48 px"
          )
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "Detail is removed by size: the 16 px icon keeps only hex, arc, and sun."
        )
      ),
      gallery_specimen(
        "Attribution stays separate",
        lightlogweb_wordmark(),
        tags$hr(),
        tags$p(
          class = "llw-secondary",
          paste(
            "TUM, TSCN, MeLiDos, and funder identities belong in an about or",
            "footer region. They never enter the product mark or recolor it."
          )
        )
      )
    )
  )
}

gallery_foundations <- function() {
  gallery_section(
    "foundations",
    "02 · Foundations",
    "Type, color, space, and surfaces",
    paste(
      "Source Sans 3 carries every visible role. Semantic tokens make light",
      "and dark modes equivalent systems instead of unrelated skins."
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide",
      gallery_specimen(
        "Type hierarchy",
        tags$p(class = "llw-eyebrow", "Orientation label"),
        tags$div(class = "llw-type-display", "Make light legible"),
        tags$div(class = "llw-type-page-heading", "Page heading"),
        tags$div(class = "llw-type-section-heading", "Section heading"),
        tags$div(class = "llw-type-subsection-heading", "Subsection heading"),
        tags$p(
          "Body text explains consequential choices in plain language and stays within a comfortable reading measure."
        ),
        tags$p(
          class = "llw-secondary",
          "Secondary text supports the decision without carrying essential meaning by color alone."
        ),
        tags$p(
          class = "llw-tabular",
          tags$strong("Tabular values: "),
          "2026-07-20 14:32 · revision 0007 · 1 440 lx · 00:05:00"
        ),
        tags$p(
          lang = "de",
          "Längere, übersetzungsähnliche Bezeichnung: Analysezeitzone für die Darstellung der persönlichen Lichtexposition"
        ),
        tags$p(tags$a(href = "#forms", "Persistent, underlined link"))
      ),
      gallery_specimen(
        "Spacing and radii",
        gallery_spacing_panel(),
        tags$p(
          class = "llw-secondary mt-3",
          "Controls use 6 px, cards 12 px, and large shells 18 px radii."
        )
      )
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide mt-4",
      gallery_token_panel("light"),
      gallery_token_panel("dark")
    )
  )
}

gallery_navigation_and_containers <- function() {
  gallery_section(
    "navigation",
    "03 · Navigation and containers",
    "Quiet orientation, explicit grouping",
    paste(
      "Active navigation uses text weight and a structural marker as well as",
      "color. Cards group decisions or evidence; borders and space do most of",
      "the work."
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide",
      gallery_specimen(
        "Flat navigation",
        tags$div(class = "mb-3", lightlogweb_wordmark()),
        navset_card_underline(
          nav_panel("Import", tags$p("Source specification")),
          nav_panel("Inspect", tags$p("Evidence and provenance")),
          nav_panel("Prepare", tags$p("Draft, preview, apply"))
        ),
        tags$nav(
          class = "mt-3",
          `aria-label` = "Breadcrumb",
          tags$ol(
            class = "breadcrumb mb-0",
            tags$li(
              class = "breadcrumb-item",
              tags$a(href = "#navigation", "Dataset")
            ),
            tags$li(
              class = "breadcrumb-item active",
              `aria-current` = "page",
              "Provenance"
            )
          )
        )
      ),
      gallery_specimen(
        "Cards and grouped settings",
        card(
          card_header("Analysis time zone"),
          tags$p("Europe/Berlin · inherited from the source manifest")
        ),
        card(
          class = "llw-card--selected mt-3",
          card_header(
            icon("check-circle", `aria-hidden` = "true"),
            " Selected primary variable"
          ),
          tags$p("Melanopic EDI · lux")
        ),
        tags$div(
          class = "llw-grouped-settings mt-3",
          tags$h4("Preview settings"),
          tags$p(class = "mb-0", "50 rows · no silent sampling · UTC")
        )
      ),
      gallery_specimen(
        "Accordion and sidebar",
        accordion(
          accordion_panel(
            "Consequential choices",
            tags$p("Draft values do not change prepared data until Apply.")
          ),
          accordion_panel(
            "Advanced arguments",
            tags$p("Progressively disclosed and described in plain language.")
          )
        ),
        tags$div(
          class = "mt-3",
          layout_sidebar(
            sidebar = sidebar(
              title = "Preview controls",
              selectInput(
                "gallery_sidebar_tz",
                "Time zone",
                c("UTC", "Europe/Berlin")
              ),
              actionButton(
                "gallery_sidebar_apply",
                "Apply preview",
                class = "btn-primary"
              )
            ),
            card(
              card_header("Evidence stays primary"),
              tags$p("Plot or table area")
            )
          )
        )
      ),
      gallery_specimen(
        "Toolbar, popover, modal, and toast",
        tags$div(
          class = "llw-toolbar",
          tags$strong("Revision 7"),
          tags$div(
            class = "llw-action-row",
            popover(
              actionButton(
                "gallery_provenance",
                tags$span(class = "visually-hidden", "Show provenance"),
                icon = icon("fingerprint"),
                class = "btn-outline-secondary llw-icon-button"
              ),
              title = "Provenance",
              "Source bytes unchanged · recipe revision 7"
            ),
            actionButton(
              "gallery_modal",
              "Open decision dialog",
              class = "btn-outline-secondary"
            ),
            actionButton(
              "gallery_notice",
              "Show notification",
              class = "btn-outline-secondary"
            )
          )
        )
      )
    )
  )
}

gallery_actions_and_forms <- function() {
  gallery_section(
    "forms",
    "04 · Actions and forms",
    "Persistent labels and one primary action",
    paste(
      "Draft, preview, apply, reset, undo, and destructive actions remain",
      "visibly distinct. Validation stays with the relevant field and tells",
      "the user how to recover."
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide",
      gallery_specimen(
        "Action hierarchy",
        tags$div(
          class = "llw-action-row",
          actionButton(
            "gallery_apply",
            "Apply recipe",
            icon = icon("check"),
            class = "btn-primary"
          ),
          actionButton(
            "gallery_preview",
            "Preview",
            icon = icon("eye"),
            class = "btn-outline-secondary"
          ),
          actionButton(
            "gallery_reset",
            "Reset",
            icon = icon("rotate-left"),
            class = "btn-link"
          ),
          actionButton(
            "gallery_undo",
            "Undo",
            icon = icon("rotate-left"),
            class = "btn-outline-secondary"
          ),
          actionButton(
            "gallery_delete",
            "Delete",
            icon = icon("trash"),
            class = "btn-outline-danger"
          ),
          actionButton(
            "gallery_icon_only",
            tags$span(class = "visually-hidden", "Inspect source provenance"),
            icon = icon("fingerprint"),
            class = "btn-outline-secondary llw-icon-button"
          ),
          input_task_button(
            "gallery_task_button",
            "Calculate metric",
            icon = icon("calculator"),
            class = "btn-primary"
          ),
          downloadButton(
            "gallery_download",
            "Export CSV",
            class = "btn-outline-secondary"
          ),
          actionButton(
            "gallery_disabled",
            "Unavailable feature",
            icon = icon("lock"),
            disabled = "disabled",
            `aria-disabled` = "true"
          )
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "Hover, active, focus, disabled, and keyboard states use the same semantic boundary system."
        )
      ),
      gallery_specimen(
        "Core controls",
        textInput(
          "gallery_name",
          "Dataset display name",
          value = "Office week · participant P014",
          placeholder = "For example: Office week"
        ),
        numericInput(
          "gallery_rows",
          "Preview rows",
          value = 50,
          min = 1,
          max = 500
        ),
        selectInput(
          "gallery_variable",
          "Primary variable (lux)",
          c("Melanopic EDI" = "MEDI", "Illuminance" = "Illuminance")
        ),
        dateInput(
          "gallery_date",
          "Start date",
          value = as.Date("2026-07-20"),
          weekstart = 1
        ),
        fileInput(
          "gallery_file",
          "Source files",
          multiple = TRUE,
          accept = c(".csv", ".txt")
        ),
        checkboxInput(
          "gallery_dst",
          "Collection crossed a daylight-saving transition"
        ),
        radioButtons(
          "gallery_scale",
          "Display scale",
          c("Linear" = "linear", "Symmetric log" = "symlog"),
          inline = TRUE
        ),
        input_switch("gallery_advanced", "Show advanced arguments")
      ),
      gallery_specimen(
        "Validation and recovery",
        tags$div(
          class = "llw-field--valid",
          textInput("gallery_valid", "Participant identifier", value = "P014"),
          tags$div(
            class = "llw-field-message",
            icon("check-circle", `aria-hidden` = "true"),
            "Valid and unique in this import."
          )
        ),
        tags$div(
          class = "llw-field--warning mt-4",
          textInput("gallery_warning", "Source time zone", value = "Etc/GMT-1"),
          tags$div(
            class = "llw-field-message",
            icon("exclamation-triangle", `aria-hidden` = "true"),
            "This fixed offset has no daylight-saving rules. Confirm it matches the logger."
          )
        ),
        tags$div(
          class = "llw-field--invalid mt-4",
          textInput("gallery_invalid", "Dataset display name", value = ""),
          tags$div(
            class = "llw-field-message",
            bsicons::bs_icon("x-octagon"),
            "Enter a non-empty name that is not already used in this session."
          )
        )
      ),
      gallery_specimen(
        "Long labels and narrow actions",
        tags$div(
          class = "llw-grouped-settings",
          checkboxInput(
            "gallery_long_label",
            paste(
              "Automatically identify participant IDs from the portion of",
              "each filename matched by the reviewed regular expression"
            )
          ),
          tags$p(
            class = "llw-help",
            "The label wraps before the control or page overflows. Placeholders are examples, never labels."
          )
        ),
        tags$div(
          class = "llw-action-row mt-3",
          actionButton(
            "gallery_narrow_apply",
            "Apply participant mapping",
            class = "btn-primary"
          ),
          actionButton(
            "gallery_narrow_back",
            "Back to draft",
            class = "btn-outline-secondary"
          )
        )
      )
    )
  )
}

gallery_data <- function() {
  data.frame(
    Participant = c(
      "P014",
      "P014",
      "participant-with-a-very-long-identifier",
      "P021",
      "P021"
    ),
    Datetime = as.POSIXct(
      c(
        "2026-07-20 08:00:00",
        "2026-07-20 08:01:00",
        "2026-07-20 08:02:00",
        "2026-07-20 08:00:00",
        "2026-07-20 08:01:00"
      ),
      tz = "Europe/Berlin"
    ),
    `Melanopic EDI (lux)` = c(12.4, 86.2, NA, 412.8, 227.1),
    Quality = c(
      "Observed",
      "Observed",
      "Missing",
      "Observed",
      "Outside window"
    ),
    Note = c(
      "",
      "Window exposure",
      "No sample recorded",
      "Outdoor interval",
      "Excluded by preview only"
    ),
    check.names = FALSE
  )
}

gallery_summary_table <- function() {
  table <- data.frame(
    Participant = c("P014", "P021", "P034"),
    `Valid days` = c(5L, 4L, 0L),
    `Median melanopic EDI (lux)` = c(82.4, 146.8, NA),
    Status = c("Complete", "Incomplete day", "No valid days"),
    check.names = FALSE
  ) |>
    gt::gt() |>
    gt::fmt_number(columns = "Median melanopic EDI (lux)", decimals = 1) |>
    gt::sub_missing(missing_text = "Missing") |>
    gt::cols_align(
      align = "right",
      columns = c("Valid days", "Median melanopic EDI (lux)")
    )

  htmltools::HTML(gt::as_raw_html(table, inline_css = TRUE))
}

gallery_plot_svg <- function() {
  tags$svg(
    viewBox = "0 0 720 360",
    role = "img",
    `aria-labelledby` = "gallery-plot-title gallery-plot-description",
    tags$title(
      id = "gallery-plot-title",
      "Illustrative personal light exposure"
    ),
    tags$desc(
      id = "gallery-plot-description",
      paste(
        "A cyan solid measured series rises toward midday and falls toward",
        "evening. An amber dashed comparison follows a lower path. A hatched",
        "interval marks missing data and a labelled long-dash line marks a",
        "reference threshold."
      )
    ),
    tags$defs(
      tags$pattern(
        id = "llw-missing-pattern",
        width = "8",
        height = "8",
        patternUnits = "userSpaceOnUse",
        patternTransform = "rotate(45)",
        tags$line(
          x1 = "0",
          y1 = "0",
          x2 = "0",
          y2 = "8",
          class = "llw-plot-grid"
        )
      )
    ),
    tags$rect(
      class = "llw-plot-band",
      x = "70",
      y = "40",
      width = "115",
      height = "255"
    ),
    tags$rect(
      class = "llw-plot-band",
      x = "570",
      y = "40",
      width = "90",
      height = "255"
    ),
    tags$path(
      class = "llw-plot-grid",
      d = "M70 70 H660 M70 145 H660 M70 220 H660 M70 295 H660 M70 40 V295"
    ),
    tags$path(
      class = "llw-plot-ci",
      d = "M80 256 C150 245 190 178 255 160 C330 137 400 92 475 115 C535 132 590 205 650 246 L650 270 C585 226 530 156 475 144 C405 122 335 168 258 186 C190 207 148 270 80 280 Z"
    ),
    tags$path(
      class = "llw-plot-primary",
      d = "M80 268 C150 258 188 193 255 173 C330 150 399 106 475 129 C538 148 590 217 650 258"
    ),
    tags$path(
      class = "llw-plot-comparison",
      d = "M80 279 C155 272 200 225 260 211 C340 190 405 153 478 168 C540 181 592 232 650 270"
    ),
    tags$path(class = "llw-plot-reference", d = "M70 145 H660"),
    tags$text(
      class = "llw-plot-reference-label",
      x = "650",
      y = "136",
      `text-anchor` = "end",
      "Reference · 250 lux"
    ),
    tags$rect(
      class = "llw-plot-missing",
      x = "360",
      y = "40",
      width = "55",
      height = "255"
    ),
    tags$text(
      class = "llw-plot-missing-label",
      x = "387.5",
      y = "58",
      `text-anchor` = "middle",
      "Missing"
    ),
    tags$circle(
      class = "llw-plot-point-primary",
      cx = "255",
      cy = "173",
      r = "6"
    ),
    tags$rect(
      class = "llw-plot-point-comparison",
      x = "471",
      y = "164",
      width = "10",
      height = "10"
    ),
    tags$text(x = "70", y = "325", "00:00"),
    tags$text(x = "218", y = "325", "06:00"),
    tags$text(x = "365", y = "325", `text-anchor` = "middle", "12:00"),
    tags$text(x = "512", y = "325", "18:00"),
    tags$text(x = "660", y = "325", `text-anchor` = "end", "24:00"),
    tags$text(
      x = "24",
      y = "170",
      transform = "rotate(-90 24 170)",
      "Melanopic EDI (lux)"
    )
  )
}

gallery_expandable_plot_card <- function() {
  card(
    full_screen = TRUE,
    class = "llw-expandable-plot-card",
    card_header(
      tags$div(
        class = "llw-expandable-plot-card__header",
        tags$h4(class = "mb-0", "Exposure plot"),
        tags$p(
          class = "llw-secondary mb-0",
          "Europe/Berlin · linear scale"
        )
      )
    ),
    card_body(
      class = "llw-expandable-plot-card__body",
      tags$div(class = "llw-plot-frame", gallery_plot_svg())
    )
  )
}

gallery_data_and_plots <- function() {
  gallery_section(
    "data",
    "05 · Data and plots",
    "Evidence first, encodings reinforced",
    paste(
      "Tables align measures and preserve long identifiers in a labelled",
      "scroll region. Plot distinctions combine hue with line style, point",
      "shape, labels, bands, gaps, and patterns. Plot cards can expand for",
      "focused inspection without changing data or export resolution."
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide",
      gallery_specimen(
        "Compact sortable table",
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Compact personal light exposure table; scroll horizontally when needed",
          tabindex = "0",
          DT::dataTableOutput("gallery_dt")
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "Sorting has text available to assistive technology; missingness is written as Missing."
        )
      ),
      gallery_specimen(
        "Comfortable summary table",
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Participant summary table",
          tabindex = "0",
          gallery_summary_table()
        )
      )
    ),
    gallery_specimen(
      "Expandable plot card",
      class = "mt-4",
      tags$p(
        class = "llw-help mb-3",
        paste(
          "Use the card's Expand control for focused inspection.",
          "The size change affects presentation only."
        )
      ),
      gallery_expandable_plot_card(),
      tags$ul(
        class = "mt-3",
        tags$li("Solid cyan line with circle points: measured series"),
        tags$li("Dashed amber line with square points: comparison series"),
        tags$li("Long-dash labelled line: reference threshold"),
        tags$li("Hatched interval and written label: missing observations"),
        tags$li("Quiet edge bands: night intervals")
      ),
      tags$details(
        tags$summary("Accessible plot summary and values"),
        tags$p(
          "Exposure rises after 06:00, reaches its highest interval before midday, contains a missing interval around noon, then declines toward night."
        ),
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Plot values",
          tabindex = "0",
          tags$table(
            class = "table",
            tags$thead(tags$tr(
              tags$th("Time"),
              tags$th(class = "numeric", "Measured (lux)"),
              tags$th(class = "numeric", "Comparison (lux)")
            )),
            tags$tbody(
              tags$tr(
                tags$td("06:00"),
                tags$td(class = "numeric", "110"),
                tags$td(class = "numeric", "70")
              ),
              tags$tr(
                tags$td("12:00"),
                tags$td(class = "numeric", "Missing"),
                tags$td(class = "numeric", "180")
              ),
              tags$tr(
                tags$td("18:00"),
                tags$td(class = "numeric", "95"),
                tags$td(class = "numeric", "60")
              )
            )
          )
        )
      )
    ),
    tags$div(
      class = "llw-gallery-grid mt-4",
      gallery_specimen(
        "Dense participant case",
        tags$p(
          "Twelve participants move to labelled small multiples before twelve new hues are introduced."
        ),
        tags$div(
          class = "llw-grouped-settings llw-tabular",
          "P001–P004 · morning peak",
          tags$hr(),
          "P005–P008 · midday peak",
          tags$hr(),
          "P009–P012 · evening peak"
        )
      ),
      gallery_specimen(
        "Transformation stays visible",
        llw_status_callout(
          "warning",
          "The symmetric-log view changes presentation only. Underlying observations and exported values remain unchanged.",
          heading = "Symmetric-log display"
        )
      ),
      gallery_specimen(
        "No-data result",
        llw_status_callout(
          "idle",
          "No observations fall inside 20–21 July. Expand the date window or clear the participant filter.",
          heading = "Nothing matches this view"
        )
      )
    )
  )
}

gallery_system_states <- function() {
  messages <- c(
    idle = "Ready to calculate the selected metric.",
    queued = "Metric calculation is waiting for an available worker.",
    running = "Validating parameters and calculating by participant.",
    finalizing = "Assembling result rows and provenance.",
    complete = "Metric result committed to dataset revision 7.",
    warning = "A usable result exists; two participants have incomplete days.",
    error = "Calculation failed. Review the date window and retry; the current dataset is unchanged.",
    cancelled = "Cancelled; no changes applied.",
    stale = "Stale result not applied; the dataset is now at revision 8."
  )
  gallery_section(
    "states",
    "06 · Task and system states",
    "Every state says what happened",
    paste(
      "Color is reinforced by an icon, label, and plain-language consequence.",
      "Dynamic examples use a polite live region and never steal focus."
    ),
    tags$div(
      class = "llw-status-grid",
      Map(
        function(state, message) {
          llw_status_callout(
            state,
            message,
            action = if (identical(state, "error")) {
              actionButton(
                "gallery_retry",
                "Review and retry",
                class = "btn-outline-secondary"
              )
            }
          )
        },
        names(messages),
        unname(messages)
      )
    ),
    tags$div(
      class = "llw-gallery-grid mt-4",
      gallery_specimen(
        "Live status example",
        uiOutput("gallery_live_status"),
        actionButton(
          "gallery_advance_status",
          "Advance task state",
          class = "btn-primary mt-3"
        )
      ),
      gallery_specimen(
        "No network",
        llw_status_callout(
          "error",
          "The registered GLC source could not be reached. Check the connection and retry discovery; no data were downloaded.",
          heading = "Source unavailable"
        )
      ),
      gallery_specimen(
        "Unavailable feature",
        llw_status_callout(
          "warning",
          "Standards-compliant GLC metadata writing is unavailable until glcdp provides a public writer and validator.",
          heading = "Metadata export unavailable"
        )
      ),
      gallery_specimen(
        "Empty session",
        llw_status_callout(
          "idle",
          "Import files or load the deterministic test dataset to begin.",
          heading = "No datasets in this session"
        )
      )
    )
  )
}

gallery_contrast_table <- function() {
  pairs <- list(
    c("Primary text / page", "text", "bg_page", "4.5"),
    c("Muted text / surface", "text_muted", "bg_surface", "4.5"),
    c("Action link / surface", "action", "bg_surface", "4.5"),
    c("Button content / action", "on_action", "action", "4.5"),
    c("Control boundary / surface", "border_control", "bg_surface", "3"),
    c("Focus / page", "focus", "bg_page", "3"),
    c("Success mark / surface", "success", "bg_surface", "3"),
    c("Warning mark / surface", "warning", "bg_surface", "3"),
    c("Danger mark / surface", "danger", "bg_surface", "3")
  )
  rows <- do.call(
    rbind,
    lapply(c("light", "dark"), function(mode) {
      tokens <- lightlogweb_tokens(mode)
      do.call(
        rbind,
        lapply(pairs, function(pair) {
          data.frame(
            Mode = tools::toTitleCase(mode),
            Pair = pair[[1L]],
            Ratio = sprintf(
              "%.2f:1",
              llw_contrast_ratio(tokens[[pair[[2L]]]], tokens[[pair[[3L]]]])
            ),
            Threshold = paste0(pair[[4L]], ":1"),
            Result = if (
              llw_contrast_ratio(tokens[[pair[[2L]]]], tokens[[pair[[3L]]]]) >=
                as.numeric(pair[[4L]])
            ) {
              "Pass"
            } else {
              "Review"
            },
            check.names = FALSE
          )
        })
      )
    })
  )
  rows
}

gallery_accessibility <- function() {
  gallery_section(
    "accessibility",
    "07 · Accessibility and resilience",
    "Structure before decoration",
    paste(
      "The focus tour, contrast matrix, live status, reduced-motion behavior,",
      "semantic headings, labelled regions, 200% zoom, and 320 px layout are",
      "acceptance criteria rather than later polish."
    ),
    tags$div(
      class = "llw-gallery-grid llw-gallery-grid--wide",
      gallery_specimen(
        "Visible keyboard focus tour",
        tags$p(
          "Press Tab through every control; the 3 px ring keeps a 3 px offset."
        ),
        tags$div(
          class = "llw-focus-tour",
          tags$a(href = "#identity", "Identity"),
          actionButton("gallery_focus_button", "Button", class = "btn-primary"),
          textInput(
            "gallery_focus_text",
            "Labelled input",
            value = "Focus remains visible"
          ),
          selectInput(
            "gallery_focus_select",
            "Labelled select",
            c("First", "Second")
          )
        )
      ),
      gallery_specimen(
        "Automated contrast matrix",
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Color contrast results",
          tabindex = "0",
          tableOutput("gallery_contrast")
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "Graphical boundaries use the 3:1 non-text threshold; normal text uses 4.5:1."
        )
      ),
      gallery_specimen(
        "320 px and 200% zoom",
        tags$div(
          style = "width:20rem;max-width:100%;",
          class = "llw-grouped-settings",
          tags$h4("Narrow decision group"),
          tags$p(
            "Long labels wrap and actions stack without document-level horizontal overflow."
          ),
          tags$div(
            class = "llw-action-row",
            actionButton(
              "gallery_zoom_apply",
              "Apply participant mapping",
              class = "btn-primary"
            ),
            actionButton(
              "gallery_zoom_cancel",
              "Return to draft",
              class = "btn-outline-secondary"
            )
          )
        )
      ),
      gallery_specimen(
        "Reduced motion and live updates",
        llw_status_callout(
          "running",
          "Phase text remains visible even when animation is reduced or absent.",
          heading = "Validating source files"
        ),
        tags$p(
          class = "llw-secondary mt-3",
          "The system preference removes nonessential transitions without hiding state."
        )
      )
    )
  )
}

component_gallery_ui <- function() {
  lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link("gallery-main"),
    tags$header(
      class = "llw-gallery-header",
      tags$div(
        lightlogweb_wordmark(),
        tags$div(
          class = "llw-gallery-header__meta",
          "Milestone 2 · component gallery"
        )
      ),
      tags$nav(
        class = "d-flex flex-wrap gap-3",
        `aria-label` = "Gallery sections",
        tags$a(href = "#foundations", "Foundations"),
        tags$a(href = "#forms", "Forms"),
        tags$a(href = "#data", "Data"),
        tags$a(href = "#states", "States"),
        tags$a(href = "#accessibility", "Accessibility")
      ),
      tags$div(
        class = "llw-mode-control",
        tags$span("Color mode", class = "llw-secondary"),
        input_dark_mode(id = "gallery_mode")
      )
    ),
    tags$main(
      id = "gallery-main",
      class = "llw-gallery-main",
      tabindex = "-1",
      llw_view_header(
        "Circadian Field · implemented system",
        "LightLogWeb design guide",
        paste(
          "This gallery tests the identity and reusable component language.",
          "It deliberately does not approve a final workflow or information",
          "architecture. Toggle color mode and inspect every state."
        )
      ),
      gallery_identity(),
      gallery_foundations(),
      gallery_navigation_and_containers(),
      gallery_actions_and_forms(),
      gallery_data_and_plots(),
      gallery_system_states(),
      gallery_accessibility()
    ),
    tags$footer(
      class = "llw-attribution",
      tags$strong("Identity separation"),
      tags$span(
        paste(
          "LightLogWeb is the product identity. TUM, TSCN, MeLiDos,",
          "European Partnership on Metrology, Wellcome Trust, and Reality",
          "Labs Research are acknowledged separately."
        )
      )
    ),
    theme = lightlogweb_theme(),
    lang = "en"
  ))
}

component_gallery_server <- function(input, output, session) {
  states <- c(
    "idle",
    "queued",
    "running",
    "finalizing",
    "complete",
    "warning",
    "error",
    "cancelled",
    "stale"
  )
  state_index <- reactiveVal(1L)

  output$gallery_live_status <- renderUI({
    state <- states[[state_index()]]
    llw_status_callout(
      state,
      switch(
        state,
        idle = "Ready to validate three source files.",
        queued = "Validation is waiting for a worker.",
        running = "Checking file names and schema.",
        finalizing = "Assembling the validation summary.",
        complete = "Validation completed; no files were changed.",
        warning = "One filename needs participant-ID review.",
        error = "Validation failed; fix the file selection and retry.",
        cancelled = "Cancelled; no changes applied.",
        stale = "Stale validation result not applied."
      ),
      heading = paste("Validation", llw_status_spec(state)$label),
      live = TRUE,
      compact = TRUE
    )
  })

  observe({
    state_index((state_index() %% length(states)) + 1L)
  }) |>
    bindEvent(input$gallery_advance_status, ignoreInit = TRUE)

  observe({
    showModal(modalDialog(
      title = "Apply participant mapping?",
      llw_status_callout(
        "warning",
        "This changes participant identifiers in the prepared revision, not the canonical source bytes.",
        heading = "Review the consequence"
      ),
      tags$p(class = "mt-3", "2 identifiers change · 0 rows removed"),
      footer = tagList(
        modalButton("Return to draft"),
        actionButton(
          "gallery_confirm_modal",
          "Apply mapping",
          class = "btn-primary"
        )
      )
    ))
  }) |>
    bindEvent(input$gallery_modal, ignoreInit = TRUE)

  observe({
    removeModal()
    showNotification(
      "Mapping applied to prepared revision 8.",
      type = "message",
      duration = 6
    )
  }) |>
    bindEvent(input$gallery_confirm_modal, ignoreInit = TRUE)

  observe({
    showNotification(
      "Preview ready: 50 of 24 600 rows are shown; no rows were changed.",
      type = "message",
      duration = 8
    )
  }) |>
    bindEvent(input$gallery_notice, ignoreInit = TRUE)

  observe({
    showNotification("Review the date window, then retry.", type = "warning")
  }) |>
    bindEvent(input$gallery_retry, ignoreInit = TRUE)

  output$gallery_dt <- DT::renderDataTable(
    gallery_data(),
    rownames = FALSE,
    selection = "single",
    class = "compact stripe",
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      scrollY = "240px",
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      language = list(
        aria = list(
          sortAscending = ": sort ascending",
          sortDescending = ": sort descending"
        )
      )
    )
  )

  output$gallery_contrast <- renderTable(
    gallery_contrast_table(),
    striped = TRUE,
    bordered = FALSE,
    spacing = "s"
  )

  output$gallery_download <- downloadHandler(
    filename = function() "lightlogweb-gallery-data.csv",
    content = function(file) {
      utils::write.csv(gallery_data(), file, row.names = FALSE, na = "Missing")
    }
  )
}

component_gallery_app <- function(...) {
  shinyApp(
    ui = component_gallery_ui(),
    server = component_gallery_server,
    ...
  )
}

# Run with:
# source("dev/component-gallery-app.R")
# shiny::runApp(component_gallery_app())
