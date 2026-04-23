# run_all.R ──────────────────────────────────────────────────────────────────
# Reproduces the full analysis pipeline. Run from the project root:
#   Rscript run_all.R
# Figures are saved to figures/; intermediate data to output/.
# ─────────────────────────────────────────────────────────────────────────────

# Helper: extract R code from an Rmd and source it in the current environment
run_rmd <- function(path) {
  tmp <- tempfile(fileext = ".R")
  knitr::purl(path, output = tmp, quiet = TRUE)
  source(tmp, local = FALSE)
}

# ── 1. LLM validation ────────────────────────────────────────────────────────
for (modelName in c("o4mini", "5mini")) {
  message(sprintf("Step 1a: Validating LLM classification (%s)...", modelName))
  source("01_quality_validation/validate_classification.R")

  message(sprintf("Step 1c: Validating LLM inclusion decisions (%s)...", modelName))
  source("01_quality_validation/validate_inclusion.R")

  message(sprintf("Step 1d: Validating LLM country extraction (%s)...", modelName))
  source("01_quality_validation/validate_countries.R")
}

# 1b: Resolve unknown countries (o4mini only); produces output/fullClassifCountryo4mini.csv
message("Step 1b: Resolving unknown countries...")
source("01_quality_validation/resolve_unknown_countries.R")

# ── 2. Data preparation ───────────────────────────────────────────────────────
message("Step 2: Preparing analysis datasets...")
run_rmd("02_data_preparation/data_prep.Rmd")

# ── 3. Main figures ───────────────────────────────────────────────────────────
message("Step 3a: Generating Figure 1...")
run_rmd("03_figures/figure1.Rmd")

message("Step 3b: Generating Figure 2...")
run_rmd("03_figures/figure2.Rmd")

message("Step 3c: Generating Figure 3 (opportunity map)...")
run_rmd("03_figures/figure3.Rmd")

# ── 4. Robustness analysis ────────────────────────────────────────────────────
message("Step 4: Running robustness analysis...")
run_rmd("04_robustness/robustness_analysis.Rmd")

message("Done. Figures in figures/, intermediate data in output/.")
