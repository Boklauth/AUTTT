# AUTTT

> **Development has moved to [simPsyStudy](https://github.com/Boklauth/simPsyStudy).**
> AUTTT is no longer actively updated. Continued development of simulation-study
> design, ordinal response generation, and SEM/IRT analysis workflows takes place
> in simPsyStudy. This repository remains available for reference.

**A Unified Treatment in Test Theory**

AUTTT provides tools for simulating ordinal item responses for test-theory research. It includes utilities for factor analysis with ordinal variables (FAOV), graded response models (GRM), and Mplus simulation workflows.

The multiple-condition interface lets you define named sets of theta values, loadings, and thresholds, then generate every combination with separate output folders and recorded seeds.

> **About this guide:** The examples below describe AUTTT's final local development work. The new `simulate_condition()` interface is published in simPsyStudy, not AUTTT's GitHub main branch. For current installation instructions and examples, use the [simPsyStudy README](https://github.com/Boklauth/simPsyStudy#readme). CRAN preparation continues there.

## Installation

For this development version, install from your local AUTTT package folder. Replace the example path with the location on your computer.

```r
# Install remotes once, if needed.
install.packages("remotes")

remotes::install_local("C:/path/to/AUTTT", upgrade = "never")
library(AUTTT)
```

If you update an already loaded package, restart R before loading the newly installed version. Mplus is not needed for the simulation example below; running Mplus analyses requires a separate Mplus installation.

## Quick start: simulate multiple conditions

This example creates **eight conditions**: two sample sizes, two loading levels, and two threshold patterns. With two replications per condition, it produces **16 datasets**.

### 1. Define the factors and theta values

There are six items and two factors. Items 1–3 measure Factor 1; items 4–6 measure Factor 2. Each theta matrix has one row per person and one column per factor.

```r
library(AUTTT)

model <- list(1:3, 4:6)

set.seed(2026)
make_theta <- function(n) {
  factor1 <- rnorm(n)
  factor2 <- 0.4 * factor1 + sqrt(1 - 0.4^2) * rnorm(n)
  cbind(Factor1 = factor1, Factor2 = factor2)
}

theta_sets <- list(
  N100 = make_theta(100),
  N200 = make_theta(200)
)
```

This generates normal theta values with a population correlation of 0.4. Sample correlations will vary. These matrices are held fixed across the item-response replications.

### 2. Define loadings and thresholds

Supply one standardized FAOV loading per item. A threshold vector is shared by all items; alternatively, supply a matrix with one row per item. Two thresholds give three response categories.

```r
loading_sets <- list(
  low = rep(0.4, 6),
  high = rep(0.8, 6)
)

threshold_sets <- list(
  symmetric = c(-0.5, 0.5),
  asymmetric = c(-1.2, 0.2)
)
```

Use **FAOV thresholds**, not already converted GRM intercepts. The function converts the loadings and thresholds internally.

### 3. Run the simulation

```r
# Choose a new folder for each run. This path is relative to getwd().
output_dir <- file.path(getwd(), "auttt_example_run_01")

conditions <- simulate_condition(
  model = model,
  theta_sets = theta_sets,
  loading_sets = loading_sets,
  threshold_sets = threshold_sets,
  replications = 2,
  output_dir = output_dir,
  seed = 1234,
  file_prefix = "demo"
)

conditions[, c("condition", "theta", "loading", "threshold", "status")]
```

The function creates the output directory and a subfolder for each condition. If the destination already contains matching condition folders or run metadata, it stops before overwriting them. To repeat the example, change `auttt_example_run_01` to a new folder name.

### 4. Read a generated dataset

```r
first_file <- file.path(
  output_dir, conditions$folder[1], "demo_grm_rep1.dat"
)

responses <- read.table(first_file, header = FALSE)
dim(responses)  # 100 persons, 6 items
head(responses)
```

The `.dat` files contain space-separated responses with no header or row names. This example uses categories 1, 2, and 3. Binary simulations use categories 0 and 1.

## What is saved?

| Location | Files | Purpose |
| --- | --- | --- |
| Run folder | `conditions.csv` | Condition names, seeds, sample sizes, status, and any error messages |
| Run folder | `simulation_plan.rds` | Input sets, design, simulator, and R session information |
| Run folder | `folders.Rdata` | Condition-folder names for subsequent workflows |
| Each condition | `demo_grm_rep1.dat`, `demo_grm_rep2.dat` | Simulated item responses |
| Each condition | `parameters.rds` | Loadings, thresholds, converted parameters, and replication seeds |
| Each condition | `study_cell.Rdata` | Simulator return object for that condition |
| Each condition | Response-probability CSV files | Observed response summaries |
| Each condition | Theta CSV and replication-list `.dat` file | Theta inputs and dataset filenames |

A failed condition stops the run and records the error in `conditions.csv`; completed outputs are preserved. Automatic resuming is not currently supported.

## Response-generation methods: `"U"` and `"N"`

The single-condition function `simdata_grm()` provides two methods for drawing the random values used to assign item-response categories:

| Method | Random draw | How it is used |
| --- | --- | --- |
| `"U"` | Uniform distribution, `U(0, 1)` | Draws a value between 0 and 1 for each person and item, then compares it with the model's cumulative response probabilities to assign a category. |
| `"N"` | Standard normal distribution, `N(0, 1)` | Draws a standard normal value and converts it to a probability using `pnorm(z, lower.tail = FALSE)`, then uses that probability to assign a category. |

These options describe the **response-generation draws**, not the distribution of the supplied theta values. Either method uses the theta matrix you provide.

In the current implementation, `"U"` draws separately for each item, whereas `"N"` draws one normal value per person and reuses its transformed probability across that person's items. This shared draw introduces dependence between item responses conditional on theta, so the two implementations should not be treated as interchangeable.

The multiple-condition function `simulate_condition()` currently uses **`"U"` only** and does not accept a `method` argument. The quick-start example above therefore uses uniform draws for response generation.

## Input requirements and current scope

- The condition interface currently supports **two or more factors**, with consecutive item numbers within each factor. Every item must belong to exactly one factor.
- Theta matrices must have finite numeric values, at least two rows, and nonconstant columns.
- Loadings must be strictly between -1 and 1. Thresholds must be finite and strictly increasing within each item.
- Name every input set using letters, digits, underscores, or hyphens, starting with a letter or digit. Names must be unique within each list, ignoring case.
- The interface uses the existing `simdata_grm()` **method `"U"`**. It evaluates the upper normal tail at `a * theta + d`; it preserves that engine's sign convention and statistical behavior.
- Threshold sets vary fastest, then loading sets, then theta sets. For condition `i` and replication `r`, the response-generation seed is `seed + (i - 1) * replications + r`. The caller's random-number state is restored afterward.
- `simulate_conditions()` is an alias for `simulate_condition()` for users of the earlier standalone prototype.

## Other tools and help

AUTTT also includes `TSK()` for thresholds and response-distribution summaries, matrix-conversion helpers, theta-generation functions, and Mplus file-generation and output utilities. These functions are undergoing review as part of CRAN preparation.

```r
?simulate_condition
?TSK
?create_theta_mvn
```

See [NEWS.md](NEWS.md) for development changes. Report problems through the [GitHub issue tracker](https://github.com/Boklauth/AUTTT/issues), including a small reproducible example and the output of `sessionInfo()`.

## Author and license

Developed by **Bo Klauth**. AUTTT is distributed under the [MIT license](LICENSE).
