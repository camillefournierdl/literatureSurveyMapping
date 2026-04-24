library(tidyverse)

"%ni%" = Negate("%in%")

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

norm_gini <- function(n) {
  p <- n / sum(n)
  G <- 1 - sum(p^2)
  G / (1 - 1/length(n))
}

# ── Step 0: Raw literature ────────────────────────────────────────────────────

lit  <- read.csv("data/raw/literature_dataset.csv")
excl <- read.csv("output/papers_to_exclude.csv")

n_raw  <- nrow(lit)
n_excl <- nrow(excl)
after_dedup_ids <- lit$ID[lit$ID %ni% excl$ID]

# ── Step 2: LLM inclusion screening ──────────────────────────────────────────

inclRaw <- read.csv("data/llm_classifications/inclusion_o4mini.csv") %>%
  rename(ID = custom_id)

inclSum <- inclRaw %>%
  group_by(ID) %>%
  summarize(
    nIncl = sum(inclusionChatGPTClean == "included", na.rm = TRUE),
    nExcl = sum(inclusionChatGPTClean == "excluded", na.rm = TRUE),
    nBlnk = sum(inclusionChatGPTClean == "",         na.rm = TRUE)
  ) %>%
  filter(ID %in% after_dedup_ids)

inclSum$gini <- mapply(
  function(a, b, c) norm_gini(c(a, b, c)),
  inclSum$nExcl, inclSum$nIncl, inclSum$nBlnk
)

n_uncertain <- sum(inclSum$gini >= 0.5 | inclSum$nBlnk >= 2)
n_llm_excl  <- sum(inclSum$gini <  0.5 & inclSum$nBlnk < 2 & inclSum$nIncl <= 2)
n_llm_incl  <- sum(inclSum$gini <  0.5 & inclSum$nBlnk < 2 & inclSum$nIncl >  2)

incl_ids <- inclSum %>%
  filter(gini < 0.5, nBlnk < 2, nIncl > 2) %>%
  pull(ID)

# ── Step 3: Topic classification quality filter ───────────────────────────────
# mirrors validate_classification.R: Blank_count < 2, then inclusion + dedup filters

classifRaw <- read.csv("data/llm_classifications/classification_o4mini.csv") %>%
  rename(ID = custom_id)

classifQuality <- classifRaw %>%
  group_by(ID) %>%
  summarize(
    Blank_count = sum(
      classificationChatGPTClean == "" | classificationChatGPTClean == "Blank",
      na.rm = TRUE
    )
  ) %>%
  filter(Blank_count < 2)

n_classif_quality_drop <- n_distinct(classifRaw$ID) - nrow(classifQuality)

classif_final_ids <- classifQuality %>%
  filter(ID %in% incl_ids) %>%
  pull(ID)

n_classif <- length(classif_final_ids)

# ── Step 4: No topic category identified ─────────────────────────────────────
# mirrors resolve_unknown_countries.R: subset(dataClassification, None != 1)
# Use the output file which has the mode-aggregated None flag

classifOut <- read.csv("output/dataClassifToPloto4mini.csv")

# restrict to the correctly filtered set
classifOut_filtered <- classifOut %>% filter(ID %in% classif_final_ids)

n_none    <- sum(classifOut_filtered$None == 1)
n_has_cat <- sum(classifOut_filtered$None != 1)
has_cat_ids <- classifOut_filtered %>% filter(None != 1) %>% pull(ID)

# ── Step 5: Country identification breakdown ──────────────────────────────────
# Mirrors resolve_unknown_countries.R, restricted to the 474 eligible papers.

# 5a: o4mini LLM (mode of 3 runs)
ctyRaw <- read.csv("data/llm_classifications/countries_o4mini.csv") %>%
  rename(ID = custom_id)

ctyMode <- ctyRaw %>%
  group_by(ID) %>%
  summarize(countryMode = getmode(countriesChatGPTClean)) %>%
  filter(ID %in% has_cat_ids) %>%
  mutate(countryMode = ifelse(countryMode %in% c("None", ""), "Unknown", countryMode))

n_cty_o4mini     <- sum(ctyMode$countryMode != "Unknown")
n_cty_unknown_1  <- sum(ctyMode$countryMode == "Unknown")
unknown_ids_1    <- ctyMode %>% filter(countryMode == "Unknown") %>% pull(ID)

# 5b: EH manual corrections (fuzzy-matched to unknown papers)
eh <- read.csv("data/raw/unknown_countries_manual_EH.csv", sep = ";") %>%
  rename(ID = X) %>%
  filter(ID %in% unknown_ids_1, !is.na(country), country != "")

n_cty_eh      <- sum(unknown_ids_1 %in% eh$ID)
unknown_ids_2 <- unknown_ids_1[unknown_ids_1 %ni% eh$ID]

# 5c: 5mini LLM (mode of 3 runs) on remaining unknowns
cty5Raw <- read.csv("data/llm_classifications/countries_5mini.csv") %>%
  rename(ID = custom_id)

cty5Mode <- cty5Raw %>%
  group_by(ID) %>%
  summarize(countryMode = getmode(countriesChatGPTClean)) %>%
  filter(ID %in% unknown_ids_2) %>%
  mutate(countryMode = ifelse(countryMode %in% c("None", ""), "Unknown", countryMode))

n_cty_5mini   <- sum(cty5Mode$countryMode != "Unknown")
unknown_ids_3 <- unknown_ids_2[unknown_ids_2 %ni% (cty5Mode %>% filter(countryMode != "Unknown") %>% pull(ID))]

# 5d: CF manual corrections on remaining unknowns
cf <- read.csv("data/raw/unknown_countries_manual_CF.csv", sep = ";") %>%
  filter(ID %in% unknown_ids_3, !is.na(countryCF))

n_cty_cf    <- nrow(cf)
n_no_cty    <- length(unknown_ids_3) - n_cty_cf

# Final country file (cross-check)
cty <- read.csv("output/fullClassifCountryo4mini.csv") %>%
  select(ID, countryPartialF) %>%
  distinct(ID, .keep_all = TRUE)

n_with_cty <- sum(has_cat_ids %in% cty$ID)
n_no_cty   <- sum(has_cat_ids %ni% cty$ID)

# ── Print step-by-step ────────────────────────────────────────────────────────

cat(sprintf("\nStep 0 — Raw literature:                                         %d\n", n_raw))
cat(sprintf("Step 1 — Remove duplicates (%d) + retracted (%d):              -%d  ->  %d\n",
            sum(excl$reason == "duplicate"),
            sum(excl$reason == "retracted"),
            n_excl, n_raw - n_excl))
cat(sprintf("Step 2 — LLM inclusion screening (5 runs/paper, o4mini):\n"))
cat(sprintf("          Removed — uncertain (gini >= 0.5 or >= 2 blank runs): -%d\n", n_uncertain))
cat(sprintf("          Removed — excluded by LLM (< 3 of 5 runs included):   -%d\n", n_llm_excl))
cat(sprintf("          Included by LLM:                                        %d\n", n_llm_incl))
cat(sprintf("Step 3 — Classification quality filter (>= 2 blank runs):       -%d  ->  %d\n",
            n_llm_incl - n_classif, n_classif))
cat(sprintf("Step 4 — No topic category identified (None == 1):               -%d  ->  %d\n",
            n_none, n_has_cat))
cat(sprintf("Step 5 — Country identification (of %d eligible papers):\n", n_has_cat))
cat(sprintf("          Identified by o4mini LLM (3 runs/paper):                 %d\n", n_cty_o4mini))
cat(sprintf("          Unknown after o4mini:                                     %d\n", n_cty_unknown_1))
cat(sprintf("            Resolved by EH manual review:                        +%d\n", n_cty_eh))
cat(sprintf("            Resolved by 5mini LLM (3 runs/paper):                +%d\n", n_cty_5mini))
cat(sprintf("            Resolved by CF manual review:                         +%d\n", n_cty_cf))
cat(sprintf("            No country identified (all methods exhausted):          %d\n", n_no_cty))
cat(sprintf("          Final: with country -> %d   |   no country -> %d\n", n_with_cty, n_no_cty))

# ── Summary table ─────────────────────────────────────────────────────────────

cat("\n")
cat(sprintf("%-56s  %4s\n", "Stage", "N"))
cat(strrep("-", 63), "\n")
cat(sprintf("%-56s  %4d\n", "Raw literature",                                   n_raw))
cat(sprintf("%-56s  %4d\n", "After removing duplicates + retracted",            n_raw - n_excl))
cat(sprintf("%-56s  %4d\n", "After LLM inclusion screening",                    n_llm_incl))
cat(sprintf("%-56s  %4d\n", "After classification quality filter",              n_classif))
cat(sprintf("%-56s  %4d\n", "After removing papers with no topic category",     n_has_cat))
cat(sprintf("%-56s  %4d\n", "After removing papers with no country identified", n_with_cty))
cat(strrep("-", 63), "\n")
cat(sprintf("%-56s  %4d\n", "Final unique papers in analysis",                  n_with_cty))
cat("\n")
