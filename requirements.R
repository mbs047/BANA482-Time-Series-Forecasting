packages <- c(
  "tidyverse",
  "lubridate",
  "forecast",
  "tseries",
  "rmarkdown"
)

missing_packages <- setdiff(packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(packages, library, character.only = TRUE))

