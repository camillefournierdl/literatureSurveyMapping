# Replication material for the paper: **"Where Knowledge Matters Most, We Know the Least:Global Patterns and Blind Spots in Air Pollution Public Opinion"**

## Authors

- [Ella Henninger](https://github.com/ellahenninger)
- [Camille Fournier de Lauriere](https://github.com/camillefournierdl)
- [Andina Nabilla](https://github.com/andinazn)
- Raeesa Moolla
- E. Keith Smith

Henninger, E., Fournier de Lauriere, C., Nabilla, A., Moolla, R., & Smith, K. (202X).
Public Views on Air Pollution: Mapping Global Patterns in Research and Survey Coverage.
*Journal Name*, *Volume*(Issue), pages. https://doi.org/10.

## Overview

This repository contains all code and data needed to reproduce the paper's results.
We review the existing scientific literature and cross-national surveys on public opinion of air pollution, identifying geographic and thematic gaps in research coverage. 
To determine whether a paper should be included, and to identify the countries and topics of focus for more than 700 scientific papers, we use a LLM (GPT-4o-mini and GPT-o4-mini).

The pipeline covers three tasks performed by the LLM:
1. **Inclusion/exclusion** — filtering the literature to papers directly relevant to public opinion of air pollution
2. **Country extraction** — identifying which country/countries each paper studies
3. **Topic classification** — coding each paper into one of five thematic categories

All LLM outputs are validated against human researcher annotations (n = 59).

## Repository Structure

```
├── run_all.R                        # Master script — runs the full pipeline
├── data/
│   ├── raw/                         # Input data files
│   └── llm_classifications/         # GPT classification outputs (2 models × 3 tasks)
├── 01_quality_validation/           # Validate LLM outputs against human annotations
├── 02_data_preparation/             # Merge and clean data for analysis
├── 03_figures/                      # Generate main paper figures
├── 04_robustness/                   # Summary statistics and robustness checks
├── 09_llm_classifications/          # Example code with prompts for the use of the llm (jupyter notebooks)
├── output/                          # Intermediate CSVs (produced by pipeline)
└── figures/
    ├── main/                        # Publication-ready figures (PDF and PNG)
    └── robustness/                  # Robustness check figures
```

## Data Sources

| File | Description |
|---|---|
| `data/raw/literature_dataset.csv` | Full literature corpus submitted to LLM |
| `data/raw/survey_classification.csv` | Survey items with human-coded country and topic |
| `data/raw/pm25_v6.csv` | Population-weighted PM2.5 exposure by country (V6, primary) |
| `data/raw/pm25_v5.csv` | PM2.5 exposure (V5, used only for Hong Kong and Taiwan) |
| `data/raw/ghs_population.csv` | GHS population estimates by country |
| `data/raw/world_bank_income_groups.csv` | World Bank income group classifications |
| `data/raw/combined_lit_social_science.csv` | Social science journal subset for robustness analysis |
| `data/raw/hand_classification.csv` | Human-coded paper classifications and inclusion |
| `data/raw/hand_classification_countries.csv` | Human-coded country assignments |
| `data/llm_classifications/` | LLM outputs (inclusion, countries, categories × 2 models) |

Pollution levels for each country downloaded from the van Donkelaar group at https://www.satpm.org/.

## Reproducing Results

Run the full pipeline from the project root:

```bash
Rscript run_all.R
```

## Output

- `output/` — intermediate CSVs consumed by figure scripts
- `figures/main/` — publication-ready figures (PDF and PNG)
- `figures/robustness/` — robustness check figures
