library(tidyverse)
library(stringdist)

lit <- read_csv("data/raw/literature_dataset.csv", show_col_types = FALSE)

# ── 1. Retracted papers ──────────────────────────────────────────────────────
# Flag any paper whose title contains "retract" (case-insensitive)
retracted <- lit %>%
  filter(str_detect(Title, regex("retract", ignore_case = TRUE))) %>%
  select(ID, Title, Authors, Year, DOI)

cat("=== RETRACTED PAPERS ===\n")
print(retracted, n = Inf)

# ── 2. Exact duplicates by DOI ───────────────────────────────────────────────
doi_dupes <- lit %>%
  filter(!is.na(DOI), DOI != "") %>%
  group_by(DOI) %>%
  filter(n() > 1) %>%
  arrange(DOI) %>%
  select(ID, DOI, Title, Authors, Year, Source)

cat("\n=== EXACT DOI DUPLICATES ===\n")
print(doi_dupes, n = Inf)

# ── 3. Near-duplicate titles (Jaro-Winkler similarity ≥ 0.93) ───────────────
# Normalise titles for comparison
titles_norm <- tolower(str_squish(str_replace_all(lit$Title, "[^A-Za-z0-9 ]", " ")))

n <- nrow(lit)
near_dupes_list <- list()

for (i in seq_len(n - 1)) {
  sims <- stringsim(titles_norm[i], titles_norm[(i + 1):n], method = "jw")
  matches <- which(sims >= 0.93) + i
  if (length(matches) > 0) {
    for (j in matches) {
      near_dupes_list[[length(near_dupes_list) + 1]] <- tibble(
        ID_a    = lit$ID[i],
        ID_b    = lit$ID[j],
        Title_a = lit$Title[i],
        Title_b = lit$Title[j],
        Authors_a = lit$Authors[i],
        Authors_b = lit$Authors[j],
        Year_a  = lit$Year[i],
        Year_b  = lit$Year[j],
        DOI_a   = lit$DOI[i],
        DOI_b   = lit$DOI[j],
        Source_a = lit$Source[i],
        Source_b = lit$Source[j],
        title_sim = round(sims[j - i], 4)
      )
    }
  }
}

near_dupes <- bind_rows(near_dupes_list)

cat("\n=== NEAR-DUPLICATE TITLES (Jaro-Winkler ≥ 0.93) ===\n")
print(near_dupes %>% select(ID_a, ID_b, title_sim, Year_a, Year_b, Authors_a, Authors_b, Title_a, Title_b), n = Inf)

# ── 4. Cluster duplicate pairs and decide which to keep ─────────────────────
# Union-find over all near-duplicate pairs
dup_ids <- unique(c(near_dupes$ID_a, near_dupes$ID_b))
parent  <- setNames(as.character(dup_ids), as.character(dup_ids))

uf_find <- function(x) {
  x <- as.character(x)
  while (parent[x] != x) x <- parent[x]
  x
}
uf_unite <- function(a, b) {
  ra <- uf_find(a); rb <- uf_find(b)
  if (ra != rb) parent[ra] <<- rb
}
for (i in seq_len(nrow(near_dupes))) uf_unite(near_dupes$ID_a[i], near_dupes$ID_b[i])

# For each group keep the entry with: DOI > Scopus source > lowest ID
dup_groups <- lit %>%
  filter(ID %in% dup_ids) %>%
  mutate(
    group   = sapply(as.character(ID), uf_find),
    has_doi = !is.na(DOI) & DOI != "",
    # Higher score = preferred to keep
    score   = has_doi * 1000 + (Source == "Scopus") * 100 - ID
  ) %>%
  group_by(group) %>%
  mutate(action = if_else(score == max(score), "KEEP", "FLAG_DUPLICATE")) %>%
  ungroup() %>%
  arrange(group, desc(score)) %>%
  select(group, ID, Source, Year, has_doi, action, Title, Authors, DOI)

cat("\n=== DUPLICATE GROUP DECISIONS ===\n")
print(dup_groups %>% select(group, ID, Source, Year, has_doi, action), n = Inf)

# ── 5. Build final exclusion list ────────────────────────────────────────────
ids_retracted  <- retracted$ID
ids_dup_flag   <- dup_groups %>% filter(action == "FLAG_DUPLICATE") %>% pull(ID)
ids_to_exclude <- sort(unique(c(ids_retracted, ids_dup_flag)))

cat("\n=== FINAL IDs TO EXCLUDE ===\n")
cat("Retracted:  ", paste(ids_retracted,  collapse = ", "), "\n")
cat("Duplicates: ", paste(sort(ids_dup_flag), collapse = ", "), "\n")
cat("All:        ", paste(ids_to_exclude, collapse = ", "), "\n")
cat("Total:      ", length(ids_to_exclude), "papers\n")

exclusion_list <- tibble(
  ID     = ids_to_exclude,
  reason = case_when(ID %in% ids_retracted ~ "retracted", TRUE ~ "duplicate")
) %>%
  left_join(lit %>% select(ID, Title, Authors, Year, DOI, Source), by = "ID") %>%
  arrange(reason, ID)

# ── 6. Save all output ───────────────────────────────────────────────────────
write_csv(retracted,       "output/candidate_retracted.csv")
write_csv(doi_dupes,       "output/candidate_doi_duplicates.csv")
write_csv(near_dupes,      "output/candidate_near_duplicate_titles.csv")
write_csv(exclusion_list,  "output/papers_to_exclude.csv")

cat("\nFiles written to output/:\n")
cat("  candidate_retracted.csv             —", nrow(retracted), "papers\n")
cat("  candidate_doi_duplicates.csv        —", nrow(doi_dupes), "papers\n")
cat("  candidate_near_duplicate_titles.csv —", nrow(near_dupes), "pairs\n")
cat("  papers_to_exclude.csv               —", nrow(exclusion_list), "papers to exclude\n")

