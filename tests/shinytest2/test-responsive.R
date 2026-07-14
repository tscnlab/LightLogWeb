testthat::test_that("core screens remain usable at production breakpoints", {
  testthat::skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(
    app_dir = normalizePath(testthat::test_path("..", "..", "inst", "app")),
    name = "responsive",
    variant = shinytest2::platform_variant(),
    seed = 20260713,
    height = 900,
    width = 1440
  )
  on.exit(app$stop(), add = TRUE)
  app$click("explore_demo")
  app$wait_for_idle()
  app$expect_screenshot(name = "overview-desktop")
  app$set_window_size(width = 768, height = 1024)
  app$expect_screenshot(name = "overview-tablet")
  app$set_window_size(width = 320, height = 800)
  app$click(selector = ".bslib-sidebar-layout > .collapse-toggle")
  app$wait_for_idle()
  Sys.sleep(0.75)
  app$expect_screenshot(name = "overview-mobile-320")
  app$set_inputs(workflow = "metrics")
  app$wait_for_idle()
  # Headless Chrome can retain a stale native-select compositor layer after
  # replacing a tall Shiny output; force the same repaint a real scroll causes.
  app$run_js("document.querySelector('.llw-app-header').style.transform = 'translateZ(0)'")
  Sys.sleep(2)
  app$expect_screenshot(name = "metrics-mobile-320")
})
