library(tidyverse)

if (!exists("modelName")) modelName <- "o4mini"

papers_to_exclude <- read.csv("output/papers_to_exclude.csv")

## first load gpt-coded labeled data
datasetGPT <- read.csv(paste("data/llm_classifications/classification_", modelName, ".csv", sep = ""))

#### exclude papers that are excluded from the first script ####
datasetGPTinclusion <- read.csv(paste("data/llm_classifications/inclusion_", modelName, ".csv", sep = ""))

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# similarly for Gini
norm_gini <- function(n) {
  p <- n / sum(n)
  G <- 1 - sum(p^2)
  G / (1 - 1/length(n))
}

datasetGPTsummary <- datasetGPTinclusion %>%
  # filter(inclusionChatGPTClean %in% c("excluded", "included")) %>% 
  group_by(custom_id) %>% 
  summarize(inclusionChatGPTCleanMode = getmode(inclusionChatGPTClean),
            countExclusion = sum(inclusionChatGPTClean == "excluded", na.rm = TRUE),
            countInclusion = sum(inclusionChatGPTClean == "included", na.rm = TRUE),
            countBlank = sum(inclusionChatGPTClean == "", na.rm=T))

datasetGPTsummary$gini_norm <- mapply(
  function(a, b, c) norm_gini(c(a, b, c)),
  datasetGPTsummary$countExclusion,
  datasetGPTsummary$countInclusion,
  datasetGPTsummary$countBlank
)

#IDs to check manually for exclusion from inclusion data
IDmanual2 <- unique(subset(datasetGPTsummary, gini_norm > 0.5)$custom_id)
IDmanual3 <- subset(datasetGPTsummary, countBlank > 1)$custom_id

save(IDmanual2, IDmanual3, file = "output/checkIDmanualInclusion.RData")

# these are the uncertain ones, for now I remove here
datasetGPTsummary <- subset(datasetGPTsummary, gini_norm < 0.5)
datasetGPTsummary <- subset(datasetGPTsummary, countBlank < 2)

datasetInclusion <- subset(datasetGPTsummary, countInclusion > 2)

#### preparing the classification dataset from chatgpt ####
datasetGPTsimple <- datasetGPT %>%
  select(custom_id, choice_index, classificationChatGPTClean) %>% 
  rename(ID = custom_id)

datasetGPTsimple$classificationChatGPTClean <- ifelse(datasetGPTsimple$classificationChatGPTClean == "", "Blank", datasetGPTsimple$classificationChatGPTClean)

getmode <- function(v) {
  v <- na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

colsGPT <- c("Perceptions", "Behaviors", "Policy", "Health", "Priority", "None", "Blank")

# find a way to measure uncertainty for each category

# for sure, count, but then what, count the number for every category? Could work

datasetGPTsimple_wide <- datasetGPTsimple %>%
  group_by(ID, choice_index) %>% 
  # Remove brackets and single quotes
  mutate(categories = gsub("\\[|\\]|'", "", classificationChatGPTClean)) %>%
  # Split the string into multiple rows
  separate_rows(categories, sep = ",\\s*") %>%
  mutate(categories = str_replace_all(categories, " ", "")) %>% 
  # Create a dummy column
  mutate(dummy = 1) %>%
  # Pivot to a wide format, filling missing values with 0
  tidyr::pivot_wider(names_from = categories, values_from = dummy, values_fill = 0) %>% 
  group_by(ID) %>% 
  summarise(
    across(
      all_of(colsGPT),
      .fns = list(
        mode  = getmode,            # your existing mode function
        count = ~ sum(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

#IDs to check manually for inclusion from classification data
IDmanual <- unique(subset(datasetGPTsimple_wide, Blank_count > 1)$ID)
save(IDmanual, file = "output/checkIDmanualClassif.RData")

# calculate uncertainty

mode_cols <- datasetGPTsimple_wide %>%
  select(ends_with("_count"), -None_count)

overall_counts <- mode_cols %>%
  summarise(
    total_0 = sum(. == 0, na.rm = TRUE),
    total_5 = sum(. == 5, na.rm = TRUE)
  )

overall_counts

total_classifications <- sum(!is.na(as.matrix(mode_cols)))
total_classifications

###
summary_counts <- datasetGPTsimple_wide %>%
  select(ends_with("_count"), -None_count) %>%
  pivot_longer(everything(), values_to = "value") %>%
  summarise(
    n_0_5 = sum(value %in% c(0, 5), na.rm = TRUE),
    n_1_4 = sum(value %in% c(1, 4), na.rm = TRUE),
    n_2_3 = sum(value %in% c(2, 3), na.rm = TRUE)
  )

summary_counts

per_category_summary <- datasetGPTsimple_wide %>%
  select(ends_with("_count"), -None_count) %>%
  pivot_longer(
    everything(),
    names_to = "category",
    values_to = "value"
  ) %>%
  group_by(category) %>%
  summarise(
    total = sum(!is.na(value)),
    n_0_5 = sum(value %in% c(0, 5), na.rm = TRUE),
    n_1_4 = sum(value %in% c(1, 4), na.rm = TRUE),
    n_2_3 = sum(value %in% c(2, 3), na.rm = TRUE),
    prop_0_5 = n_0_5 / total,
    prop_1_4 = n_1_4 / total,
    prop_2_3 = n_2_3 / total,
    .groups = "drop"
  )

per_category_summary

datasetGPTsimple_wide_certain <- subset(datasetGPTsimple_wide, Blank_count < 2)

colsGPT <- c("Perceptions", "Behaviors", "Policy", "Health", "Priority", "None")

datasetGPTsimple_wide_fit <- datasetGPTsimple_wide_certain %>%
  rename(
    Behavior = Behaviors_mode,
    Perceptions = Perceptions_mode,
    Policy = Policy_mode,
    Health = Health_mode,
    Priority = Priority_mode,
    None = None_mode
  ) %>%
  mutate(reader = "ChatGPT",
         ID = ID) %>%
  select(ID, Perceptions, Priority, Policy, Health, Behavior, None, reader) %>%
  filter(ID %in% datasetInclusion$custom_id) %>%
  filter(ID %ni% papers_to_exclude$ID)

write.csv(datasetGPTsimple_wide_fit, paste("output/dataClassifToPlot", modelName, ".csv", sep = ""), row.names = F) # save for future plotting, after QA

## now load hand-labeled data
datasetHand <- read.csv("data/raw/hand_classification.csv", sep = ";")

datasetHand_fit <- datasetHand %>% 
  mutate(None = ifelse(Inclusion == 0, 1, 0)) %>% 
  select(ID, Perceptions, Priority, Policy, Health, Behavior, None, reader)

#### Merging classifications to make comparisons
dataCompare <- rbind(datasetHand_fit, datasetGPTsimple_wide_fit)

dataCompare <- dataCompare %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  ungroup()

# remove duplicate
dataCompare <- dataCompare %>%
  distinct(ID, reader, .keep_all = TRUE)

colsBoth <- c("Perceptions", "Behavior", "Policy", "Health", "Priority", "None")

class_cols <- colsBoth

mismatch_summary <- dataCompare %>%
  group_by(ID) %>%
  summarize(across(all_of(class_cols), ~ n_distinct(.)), .groups = "drop") %>%
  # Identify IDs where at least one classification column has more than one unique value
  filter(if_any(all_of(class_cols), ~ . > 1))

# View the summary of IDs with mismatches
print(mismatch_summary)

####### reader specific 
# Convert classification columns to character and pivot to long format
long_data <- dataCompare %>%
  select(ID, reader, all_of(class_cols)) %>%
  mutate(across(all_of(class_cols), as.character)) %>%
  pivot_longer(cols = all_of(class_cols), 
               names_to = "variable", 
               values_to = "response")

# Self join to compare responses from different readers for the same ID and variable
pairwise <- long_data %>%
  inner_join(long_data, by = c("ID", "variable"), suffix = c("_1", "_2")) %>%
  filter(reader_1 < reader_2) %>%   # to avoid duplicate pairs
  mutate(agreement = ifelse(response_1 == response_2, "Agree", "Disagree"))

# Create a contingency table of overall agreement/disagreement
agreement_table <- table(pairwise$agreement)
print(agreement_table)

reader_pair_summary <- pairwise %>%
  group_by(reader_1, reader_2, agreement) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = agreement, values_from = count, values_fill = 0)

print(reader_pair_summary)

##### comparing columns 
# Create a contingency table for each classification variable
agreement_tables <- pairwise %>%
  group_by(variable, agreement) %>%
  summarize(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = agreement, values_from = count, values_fill = 0)

print(agreement_tables)

### 
# Get list of unique raters
raters <- unique(dataCompare$reader)

# Create all unique pairs of raters
pairs <- combn(raters, 2, simplify = FALSE)

# Prepare a data frame to store percent agreement results
agreement_results <- data.frame(Pair = character(), 
                                Category = character(), 
                                PercentAgreement = numeric(),
                                PairType = character(),
                                stringsAsFactors = FALSE)
categories <- class_cols

# Loop over each pair and each category
for (pair in pairs) {
  rater1 <- pair[1]
  rater2 <- pair[2]
  
  # Subset df to rows for the two raters
  df_pair <- dataCompare %>% 
    filter(reader %in% c(rater1, rater2)) %>%
    group_by(ID) %>% 
    filter(n() == 2) %>%  # Keep only papers rated by both
    ungroup()
  
  for (cat in categories) {
    # For the given category, pivot to wide format so that each row is a paper 
    # with two columns for the two raters' responses.
    df_wide <- df_pair %>% 
      select(ID, reader, !!sym(cat)) %>% 
      pivot_wider(names_from = reader, values_from = !!sym(cat))
    
    # Only process if there is any data after pivoting
    if (nrow(df_wide) > 0) {
      # Calculate the raw percent agreement
      agreement <- mean(df_wide[[rater1]] == df_wide[[rater2]], na.rm = TRUE)
      
      # Label the pair type
      pair_type <- ifelse(grepl("ChatGPT", paste(rater1, rater2)), "Human-Model", "Human-Human")
      
      # Append the result
      agreement_results <- rbind(agreement_results,
                                 data.frame(Pair = paste(rater1, rater2, sep = "-"),
                                            Category = cat,
                                            PercentAgreement = agreement,
                                            PairType = pair_type,
                                            stringsAsFactors = FALSE))
      
    }
  }
}

# Display the results
print(agreement_results)

agreement_results %>% 
  group_by(PairType) %>% 
  summarize(averageAgreement = mean(PercentAgreement))

agreement_results %>% 
  group_by(PairType, Category) %>% 
  summarize(averageAgreement = mean(PercentAgreement))

### visualise this

p1 <- ggplot(agreement_results, aes(x = Category, y = PercentAgreement, fill = PairType)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.7) +
  geom_jitter(aes(shape = PairType),
              position = position_dodge(width = 0.8), alpha = 1) +
  scale_fill_viridis_d() +
  labs(x = "Category",
       y = "Percent Agreement") +
  theme_minimal()

ggsave(plot = p1, filename = paste("01_quality_validation/plots/classificationGPTHumanBoxplot", modelName, ".png", sep = ""),
       dpi=600, width = 16, height = 12, units='cm')

agg_data <- agreement_results %>%
  group_by(Category, PairType) %>%
  summarise(
    mean_agreement = mean(PercentAgreement, na.rm = TRUE),
    sd_agreement = sd(PercentAgreement, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(se = sd_agreement / sqrt(n))

# Plot bar chart with error bars (using standard error)
p2 <- ggplot(agg_data, aes(x = Category, y = mean_agreement, fill = PairType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_agreement - se, ymax = mean_agreement + se),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d() +
  labs(x = "Category",
       y = "Average Percent Agreement") +
  theme_minimal()

ggsave(plot = p2, filename = paste("01_quality_validation/plots/classificationGPTHuman", modelName, ".png", sep = ""),
       dpi=600, width = 16, height = 12, units='cm')

### getting more insights
# Calculate the prevalence (positive rate) for each category per reader
prevalence_data <- dataCompare %>%
  pivot_longer(cols = categories, names_to = "Category", values_to = "Value") %>%
  group_by(reader, Category) %>%
  summarise(positive_rate = mean(Value, na.rm = TRUE),
            .groups = "drop")

# View the prevalence data
print(prevalence_data)


################ maybe new version of above

# Step 1: Calculate prevalence (positive rate) for each category per reader
prevalence_data <- dataCompare %>%
  pivot_longer(cols = categories, names_to = "Category", values_to = "Value") %>%
  group_by(reader, Category) %>%
  summarise(positive_rate = mean(Value, na.rm = TRUE),
            .groups = "drop")

# Example plot: Compare positive rates per category for each reader
p3 <- ggplot(prevalence_data, aes(x = Category, y = positive_rate, fill = reader)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d() +
  labs(x = "Category",
       y = "Positive Rate") +
  theme_minimal()

ggsave(plot = p3, filename = paste("01_quality_validation/plots/classificationGPTHumanPositiveRateResearchers", modelName, ".png", sep = ""),
       dpi=600, width = 16, height = 12, units='cm')

# Step 2: For human raters, compute the mean positive rate and standard deviation per category.
human_prevalence <- prevalence_data %>%
  filter(reader != "ChatGPT") %>%
  group_by(Category) %>%
  summarise(mean_human_rate = mean(positive_rate, na.rm = TRUE),
            sd_human_rate = sd(positive_rate, na.rm = TRUE),
            .groups = "drop")

# Step 3: For the model, extract the positive rate for each category.
model_prevalence <- prevalence_data %>%
  filter(reader == "ChatGPT") %>%
  select(Category, model_rate = positive_rate)

# Step 4: Merge the human and model results by Category.
comparison_data <- left_join(model_prevalence, human_prevalence, by = "Category")

# Step 5: Reshape for plotting the two groups (Model and Humans).
comparison_long <- comparison_data %>%
  pivot_longer(cols = c(model_rate, mean_human_rate),
               names_to = "Rater", values_to = "Rate") %>%
  mutate(Rater = recode(Rater,
                        "model_rate" = "Model",
                        "mean_human_rate" = "Humans"))

comparison_long$sd_human_rate <- ifelse(comparison_long$Rater == "Model", 0, comparison_long$sd_human_rate)

# Step 6: Plot the comparison with error bars (only for the human group).
p4 <- ggplot(comparison_long, aes(x = Category, y = Rate, fill = Rater)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
  # Add error bars only for Humans
  geom_errorbar(data = comparison_long,
                aes(x = Category,
                    ymin = Rate - sd_human_rate,
                    ymax = Rate + sd_human_rate),
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d() +
  labs(x = "Category",
       y = "Positive Rate") +
  theme_minimal()

ggsave(plot = p4, filename = paste("01_quality_validation/plots/classificationGPTHumanPositiveRate", modelName, ".png", sep = ""),
       dpi=600, width = 16, height = 12, units='cm')







