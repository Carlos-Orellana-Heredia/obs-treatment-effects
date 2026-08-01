# Required package check --------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "readr", "ggplot2", "WeightIt", "cobalt",
  "geepack", "emmeans", "survival", "broom", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages before running this workflow: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
