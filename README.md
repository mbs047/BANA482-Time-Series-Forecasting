# BANA482 | Time Series Forecasting

I use this repository to keep my BANA482 Case 3 work organized and reproducible. The case focuses on monthly sales time series analysis, including decomposition, stationarity testing, ARIMA modeling, ETS modeling, forecasting, and model evaluation.

## What I Included

- `analysis/case-3-time-series.Rmd` - my reproducible R Markdown analysis.
- `data/` - the training, test, and sample submission CSV files used in the analysis.
- `reports/` - my submitted report exports.
- `docs/` - the case brief, instructions, and grading rubrics.
- `references/` - supporting reference material and starter/reference R code.

## How I Run the Analysis

I run the analysis from the repository root so the relative dataset path works correctly:

```r
rmarkdown::render("analysis/case-3-time-series.Rmd")
```

The R Markdown file uses these R packages:

- `tidyverse`
- `lubridate`
- `forecast`
- `tseries`

If a package is missing, I install it first:

```r
install.packages(c("tidyverse", "lubridate", "forecast", "tseries"))
```

## Project Notes

I keep source files, submitted deliverables, datasets, and assignment details in the repository. I ignore local runtime files such as `.RData`, `.Rhistory`, `.DS_Store`, cache folders, and temporary Office lock files because they are not part of the case submission.

## License

I am sharing the code and project documentation under the MIT License. Course-provided datasets, prompts, rubrics, and reference PDFs remain subject to their original owners and course policies.
