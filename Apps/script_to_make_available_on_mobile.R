shiny::runApp(
  appDir = "Apps/AFL_APP",
  host   = "0.0.0.0",   # listen on every interface
  port   = 3838,
  launch.browser = FALSE
)

shiny::runApp(
  appDir = "Apps/SGM",
  host   = "0.0.0.0",   # listen on every interface
  port   = 3838,
  launch.browser = FALSE
)