# ------------------------------------------------------------------------------
# SETTING UP SELENIUM WEB DRIVER
# ------------------------------------------------------------------------------

# Purpose: set up remotely controlled selenium web driver to access websites
# for scraping

# TOP-LEVEL FUNCTION -----------------------------------------------------------

# Set up selenium driver for web scraping

set_up_selenium <- function(chrome_version, port) {
  
  # Kill any running sessions before firing up selenium
  
  invisible(system(
    "taskkill /im java.exe /f", 
    intern = FALSE, 
    ignore.stdout = FALSE,
    show.output.on.console = FALSE))
  
  # Instantiate driver

  tryCatch(
      rsDriver(
      port = port,
      browser = "chrome",
      chromever = chrome_version,
      verbose = FALSE
      )[["client"]],
    error = function(e) stop("Set-up of selenium driver failed")
  )

}

