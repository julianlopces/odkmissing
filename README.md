
<!-- README.md is generated from README.Rmd. Please edit that file -->

------------------------------------------------------------------------

### OKRMISSING

``` markdown
# okrmissing

<!-- badges: start -->
[![R-CMD-check](https://github.com/julianlopces/okrmissing/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/julianlopces/okrmissing/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`okrmissing` is an R package to **generate and validate missing value flags** 
for ODK-based survey data.  

It provides tools to:

- Import an ODK questionnaire and propagate relevance rules.  
- Translate ODK relevance conditions into tokens and dummy variables.  
- Build a specification (`spec_for_flags`) for each variable, indicating 
  when it should be considered *relevant*.  
- Generate row-wise missing flags that respect those relevance rules.  


## Installation

You can install the development version of **okrmissing** from GitHub:

```r
# install.packages("remotes")
remotes::install_github("julianlopces/okrmissing")
```

------------------------------------------------------------------------

## Example (Pipeline)

Suppose you have:

- An **ODK questionnaire** in Excel (`Encuesta.xlsx`),  
- A **raw dataset** exported from ODK (`data_raw.xlsx`).

You can run the full missing pipeline like this:

``` r
library(okrmissing)

# Import questionnaire and propagate required flags
ODK_filtrado <- import_odk_propagate_required("Encuesta.xlsx", required_value = "yes")

# Import dataset
datos <- readxl::read_excel("data_raw.xlsx", sheet = "data_raw")

# Build specification from ODK + datos
ODK_procesado <- build_spec_for_flags(datos = datos, ODK_filtrado = ODK_filtrado)

spec_for_flags <- ODK_procesado$spec_for_flags
datos_tokens   <- ODK_procesado$datos_tokens

# Generate missing flags
base_missing <- flags_missing_por_variable(
  data          = datos_tokens,
  spec          = spec_for_flags,
  prefix        = "m",
  numeric_conds = TRUE,
  coerce_target = FALSE
)

# Summarize
variables_missing <- names(base_missing)[grepl("^m_", names(base_missing))]
base_missing <- base_missing |>
  dplyr::mutate(
    total_missing = rowSums(dplyr::pick(dplyr::all_of(variables_missing)), na.rm = TRUE),
    flag_missing  = ifelse(total_missing > 0, 1, 0)
  )
```

## Another alternative with `create_missing_vars()`

Once you have the package there is one function summarizing all

Instead of running each step manually, you can use the wrapper
`create_missing_vars()` which combines the three core functions:

- `import_odk_propagate_required()`
- `build_spec_for_flags()`
- `flags_missing_por_variable()`

``` r
library(okrmissing)

# Run the full pipeline in one step
res <- create_missing_vars(
  path_odk       = "Encuesta.xlsx",          # ODK questionnaire
  required_value = "yes",                    # mark "yes" as required
  path_datos     = "data_raw.xlsx",          # ODK raw data
  sheet_datos    = "data_raw",               # sheet name if Excel
  prefix         = "m",                      # prefix for missing flags
  numeric_conds  = TRUE,
  coerce_target  = FALSE,
  return_all     = TRUE                      # return spec + tokens too
)

# Final dataset with missing flags
head(res$data)

# Inspect how many missings were flagged
table(res$data$flag_missing)
```
