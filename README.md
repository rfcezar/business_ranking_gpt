# Replication Package – *Ranking Business Trade Preferences Using GPT*

## Structure

All files are placed in a flat directory. The contents of this package include:

- `code.R` — Main R script to replicate all empirical results and figures.
- `btm.stan` — Stan model specification for Bayesian estimation of Bradley-Terry models using pairwise comparison data.  
- `alpha_summary.csv` — Summary of Bradley-Terry estimated positions per organization.  
- `dyads_sampled_1000.csv` — Sample of pairwise comparisons used in the robustness tests.  
- `results_poe.csv` — Output with PoE tests results for ``gpt-4.1-mini,” ``gpt-4.1-nano,” ``gpt-4o,” and ``gpt-4o-mini.”  
- `session_info.txt` — Software environment information for reproducibility.  
- `business_ranking_gpt.Rproj` — RStudio project file.  

## How to Run the Replication

To install the required R packages, open R and run the code block below before executing the replication script.

1. Open R or RStudio.  
2. Set the working directory to the folder where this `README.md` is located (i.e., the root of the replication package).  
3. Open and execute the script `analysis.R`.

The script will:

- Load all required packages  
- Read the input datasets  
- Perform all estimations and tests  
- Reproduce the figures and estimates reported in the project

## Required R Packages

## Required R Packages

The analysis depends on the following R packages:

- `rstan`  
- `dplyr`  
- `ggplot2`  
- `httr`  
- `jsonlite`  
- `stringr`  
- `progress`  
- `irr`  
- `reshape2`  
- `BradleyTerry2`  
- `stargazer`

You can install them with:

```r
install.packages(c(
  "rstan",
  "dplyr",
  "ggplot2",
  "httr",
  "jsonlite",
  "stringr",
  "progress",
  "irr",
  "reshape2",
  "BradleyTerry2",
  "stargazer"
))
```
