################################################################################
###### Replication code for "Ranking Business Trade Preferences Using GPT" #####
################################################################################

rm(list = ls())       
graphics.off()        
cat("\014")         

# === 1. Loading packages, API keys and setting the samples ===

library(rstan)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(stringr)
library(progress)
library(irr)
library(reshape2)
library(BradleyTerry2)
library(stargazer)

# For results of initial classification, load 'dyads.csv' available at: 
# https://drive.google.com/file/d/1RX_gLZ8oHhF4CVqPTZ6QvgDV9aPs9OKP/view?usp=sharing

# === 2. Setting OpenAI and Serper API keys ===

openai_key <- "YOUR KEY"
headers <- add_headers(
  Authorization = paste("Bearer", openai_key),
  `Content-Type` = "application/json"
)

serper_api_key <- "YOUR KEY"

# === 3. Load data and remove duplicates of initial textual data ===

data <- read.csv("data.csv", stringsAsFactors = FALSE)
lobby_unique <- data %>%
  distinct(submission_code, .keep_all = TRUE)

# === 4. Create unique paired combinations (no self-comparisons or mirrored pairs) ===
dyads <- cross_join(
  lobby_unique %>% mutate(id = row_number()),
  lobby_unique %>% mutate(id_b = row_number())
) %>%
  filter(id < id_b) %>%
  select(
    name_A = name.x,
    name_B = name.y,
    text_A = middle.x,
    text_B = middle.y
  )

# === 5. Sample dyads for main analysis and robustness ===
set.seed(456)
dyads_sampled_20000 <- dyads %>% sample_n(20000)
set.seed(123)
dyads_sampled_1000 <- dyads %>% sample_n(1000)

############################# MAIN ANALYSIS ####################################

# === 1. Perform API calls and retrieve results ===

# Function to classify text pairs using GPT
classify_text_pair <- function(text_A, text_B) {
  stopifnot(is.character(text_A), is.character(text_B))
  
  prompt <- paste0(
    "You will be shown two texts. Your task is to determine which one is more explicitly supportive of the protection of foreign investment.\n\n",
    "Be conservative. Only choose A or B if there is a clear difference.\n\n",
    "Respond using only one character:\n",
    "- 'A' if A is more supportive,\n",
    "- 'B' if B is more supportive,\n",
    "- 'T' if they are similar or unclear.\n\n",
    "Comment A:\n", text_A, "\n\n",
    "Comment B:\n", text_B
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    headers,
    body = toJSON(list(
      model = "gpt-4.1-mini",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0,
      max_tokens = 20
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    result <- content(response, as = "parsed")$choices[[1]]$message$content
    str_trim(toupper(result))
  } else {
    NA
  }
}

# Initialize parameters
start_index <- 1 
output_path_partial <- "dyads_text_partial.csv"
output_path_final <- "dyads.csv"

# Create column if not already present
if (!"text_comparison" %in% names(dyads_sampled_20000)) {
  dyads_sampled_20000$text_comparison <- NA
}

# Progress bar
pb <- progress_bar$new(
  total = nrow(dyads_sampled_20000) - start_index + 1,
  format = "  Processing [:bar] :current/:total (:percent) eta: :eta"
)

# Loop through each dyad and classify
for (i in start_index:nrow(dyads_sampled_20000)) {
  pb$tick()
  
  res <- classify_text_pair(
    text_A = dyads_sampled_20000$text_A[i],
    text_B = dyads_sampled_20000$text_B[i]
  )
  
  if (res %in% c("A", "B", "T")) {
    dyads_sampled_20000$text_comparison[i] <- res
  } else {
    dyads_sampled_20000$text_comparison[i] <- NA
    message("Unexpected response at row ", i, ": ", res)
  }
  
  if (i %% 100 == 0) {
    write.csv(dyads_sampled_20000, output_path_partial, row.names = FALSE)
  }
  
  Sys.sleep(runif(1, 0.3, 1))
}

# Save final version
write.csv(dyads_sampled_20000, output_path_final, row.names = FALSE)

# === 2. Prepare data for Bayesian Bradley-Terry model ===

# Filter valid dyads (only A or B, excluding T)
dyads_sampled <- dyads %>%
  filter(text_comparison %in% c("A", "B"))

# Add winner/loser columns based on response
dyads_sampled <- dyads_sampled %>%
  mutate(
    winner_name = ifelse(text_comparison == "A", name_A,
                         ifelse(text_comparison == "B", name_B, NA)),
    loser_name  = ifelse(text_comparison == "A", name_B,
                         ifelse(text_comparison == "B", name_A, NA))
  )

# Filter out ties and self-comparisons
dyads_filtered <- dyads_sampled %>%
  filter(!is.na(winner_name), !is.na(loser_name), winner_name != loser_name) %>%
  mutate(
    name_A = pmin(winner_name, loser_name),
    name_B = pmax(winner_name, loser_name),
    win_A = ifelse(winner_name == name_A, 1, 0),
    win_B = ifelse(winner_name == name_B, 1, 0)
  )

# Summarize results
results_summary <- dyads_filtered %>%
  group_by(name_A, name_B) %>%
  summarise(wins_A = sum(win_A), wins_B = sum(win_B), .groups = "drop")

# Select only comparisons with > 10 comparisons

org_counts <- dyads_filtered %>%
  select(name_A, name_B) %>%
  pivot_longer(cols = everything(), values_to = "org_name") %>%
  count(org_name, name = "n_comparisons") %>%
  arrange(desc(n_comparisons))

org_counts <- org_counts %>%
  filter(n_comparisons >= 10) %>%
  pull(org_name)

dyads_filtered <- dyads_filtered %>%
  filter(name_A %in% org_counts & name_B %in% org_counts)

# === 3. Create player indexes for BT model ===

names_all <- unique(c(dyads_filtered$winner_name, dyads_filtered$loser_name))
name_index <- setNames(seq_along(names_all), names_all)

dyads_filtered$player1 <- name_index[dyads_filtered$winner_name]
dyads_filtered$player0 <- name_index[dyads_filtered$loser_name]
dyads_filtered$y <- 1  # binary outcome

# Create Stan data list
stan_data <- list(
  K = length(names_all),
  N = nrow(dyads_filtered),
  player1 = dyads_filtered$player1,
  player0 = dyads_filtered$player0,
  y = dyads_filtered$y
)

# Compile and run the Bradley-Terry model using RStan ===

fit <- stan(
  file = "btm.stan",   # Created following Leo et al. (2025) 
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 123
)

fit_summary <- summary(fit)$summary

# Create data.frame to map index back to organization names
empresa_lookup <- data.frame(
  empresa = names(name_index),
  empresa_index = as.integer(name_index)
)

# Extract alpha estimates (latent positions) 

alpha_summary <- as.data.frame(fit_summary[grep("^alpha\\[", rownames(fit_summary)), ])
alpha_summary$empresa_index <- 1:nrow(alpha_summary)

# Keep only relevant columns and rename
alpha_summary <- alpha_summary %>%
  select(empresa_index, btm_est = `50%`, btm_lwr = `2.5%`, btm_upr = `97.5%`) %>%
  left_join(empresa_lookup, by = "empresa_index")

# === Perform initial visual illustration  ===

# Select organizations for initial visual illustration

org_counts <- dyads_filtered %>%
  select(name_A, name_B) %>%
  pivot_longer(cols = everything(), values_to = "org_name") %>%
  count(org_name, name = "n_comparisons") %>%
  arrange(desc(n_comparisons))


blue_list <- c(
  "Business Roundtable", "National Foreign Trade Council", "U.S. Council for International Business",
  "Emergency Committee for American Trade", "Cargill", "Intel Corporation", "Novartis Corporation",
  "Novelis Corporation", "IBM Corporation", "Boeing Company", "Chevron", "Dow Chemical Company",
  "Allegheny Technologies Incorporated", "Alticor", "Walmart", "MetLife", "VF Corporation",
  "Deere & Company", "CropLife America", "Federal Express", "Campbell Soup Company",
  "General Electric Company", "Semiconductor Industry Association", "The Software Alliance",
  "Information Technology Industry Council", "Retail Industry Leaders Association",
  "Entertainment Software Association", "Biotechnology Industry Organization",
  "Computing Technology Industry Association", "Biotechnology Innovation Organization",
  "The App Association", "Advanced Medical Technology Association",
  "Telecommunications Industry Association", "Association of Global Automakers",
  "Pharmaceutical Research and Manufacturers of America",
  "American Apparel & Footwear Association"
)

red_list <- c(
  "National Sunflower Association", "National Milk Producers Federation", "National Pork Producers Council",
  "National Potato Council", "National Federation of Oil Palm Growers", "National Corn Growers Association",
  "National Asparagus Council", "Iowa Corn Growers Association", "California Association of Winegrape Growers",
  "California Avocado Commission", "California Canning Peach Association", "U.S. Cattlemen’s Association",
  "American Oilseed Coalition", "American Sugar Alliance", "California Cling Peach Board",
  "California Table Grape Commission", "Corn Refiners Association", "American Soybean Association",
  "American Potato Trade Alliance", "Blue Diamond Growers", "California Cherry Board",
  "California Citrus Mutual", "California Tomato Growers Association", "California Walnut Commission",
  "Sunkist Growers", "Almond Board of California", "U.S. Wheat Associates", "National Chicken Council",
  "American Olive Oil Producers Association"
)

# Plotting organizations 

alpha_summary_grouped <- alpha_summary %>%
  filter(empresa %in% c(blue_list, red_list)) %>%
  mutate(group = case_when(
    empresa %in% blue_list ~ "Group 1",
    empresa %in% red_list ~ "Group 2"
  )) %>%
  arrange(group, btm_est) %>%
  mutate(empresa = factor(empresa, levels = empresa))

ggplot(alpha_summary_grouped, aes(x = btm_est, y = empresa, color = group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = btm_lwr, xmax = btm_upr), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("blue", "red")) +
  labs(
    x = "BTM Estimate",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

# Highlight selected organizations in apparel and retail to check for intra-
# industry cleavage (Figure 2)

textile <- c(
  "Retail Industry Leaders Association", "VF Corporation", "American Apparel & Footwear Association",
  "Marcraft Apparel Group", "U.S. Association of Importers of Textiles and Apparel",
  "Footwear Distributors & Retailers of America", "National Council of Textile Organizations",
  "Agathon Associates - Textiles and Trade Consulting"
)

alpha_plot <- alpha_summary %>%
  filter(empresa %in% textile) %>%
  arrange(btm_est) %>%
  mutate(empresa = factor(empresa, levels = empresa))  # preserve order

ggplot(alpha_plot, aes(x = btm_est, y = empresa)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbarh(aes(xmin = btm_lwr, xmax = btm_upr), height = 0.2, color = "blue") +
  labs(
    x = "BTM Estimate",
    y = NULL,
  ) +
  theme_minimal()

# === 4. Classification of all organizations using GPT ===
# For file with alpha estimates with organization type and other covariates, 
# load "alpha_summary.csv".

#  Function to query Serper for organization description ===
get_serper_snippets <- function(org_name) {
  query_body <- list(q = org_name)
  
  response <- POST(
    url = "https://google.serper.dev/search",
    add_headers(
      "X-API-KEY" = serper_api_key,
      "Content-Type" = "application/json"
    ),
    body = toJSON(query_body, auto_unbox = TRUE)
  )
  
  result <- content(response, as = "parsed", simplifyVector = TRUE)
  
  if (!is.null(result$organic)) {
    snippets <- result$organic$snippet
    snippets <- snippets[!is.na(snippets) & snippets != ""]
    if (length(snippets) == 0) return("No results")
    return(paste(snippets, collapse = " "))
  } else {
    return("No results")
  }
}

# Function to classify organization using GPT-4.1
classify_org_type <- function(org_name, org_description) {
  prompt <- paste0(
    "Organization name: ", org_name, "\n\n",
    "Description:\n", org_description, "\n\n",
    "Classify the organization into one of the following categories:\n",
    "Not firm: Not a firm or association of firms (e.g., NGO, university, think tank, foundation)\n",
    "Large: Largest individual firms (Fortune 500)\n",
    "Tech: Association representing technology sector\n",
    "Agri: Association representing agriculture sector\n",
    "Sector: Association representing sectors other than agriculture and technology\n",
    "Cross: Cross-sectoral association\n",
    "Other: Another class of firm or association of firms not listed above\n\n",
    "NA: If you are not confident, unsure, return NA\n\n",
    "Do not explain your choice. Respond with only one category."
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", openai_api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4.1",
      messages = list(
        list(role = "system", content = "You classify organizations using a strict set of categories."),
        list(role = "user", content = prompt)
      ),
      temperature = 0
    ), auto_unbox = TRUE)
  )
  
  result <- content(response, as = "parsed", simplifyVector = FALSE)
  
  if (!is.null(result$choices) &&
      length(result$choices) > 0 &&
      !is.null(result$choices[[1]]$message$content)) {
    return(trimws(result$choices[[1]]$message$content))
  } else {
    return("Error")
  }
}

# Ensure required columns exist in alpha_summary
if (!"org_reference" %in% names(alpha_summary)) {
  alpha_summary$org_reference <- NA_character_
}
if (!"org_type" %in% names(alpha_summary)) {
  alpha_summary$org_type <- NA_character_
}

# Progress bar for classification loop
pb <- progress_bar$new(
  format = "[:bar] :current/:total :percent - ETA: :eta",
  total = nrow(alpha_summary),
  clear = FALSE,
  width = 60
)

# Main loop to classify each organization
for (i in seq_len(nrow(alpha_summary))) {
  if (!is.na(alpha_summary$org_type[i]) && alpha_summary$org_type[i] != "") {
    pb$tick()
    next
  }
  
  org_name <- alpha_summary$empresa[i]
  if (is.na(org_name) || org_name == "") {
    alpha_summary$org_reference[i] <- "No name"
    alpha_summary$org_type[i] <- "Error"
    pb$tick()
    next
  }
  
  ref_text <- tryCatch(
    get_serper_snippets(org_name),
    error = function(e) "Error"
  )
  alpha_summary$org_reference[i] <- ref_text
  
  org_class <- tryCatch(
    classify_org_type(org_name, ref_text),
    error = function(e) "Error"
  )
  alpha_summary$org_type[i] <- org_class
  
  pb$tick()
  
  if (i %% 10 == 0) {
    saveRDS(alpha_summary, "alpha_summary_progress.rds")
  }
}

# Save final result
write.csv(alpha_summary, "alpha_summary.csv")

# ANOVA tests 
anova_result <- aov(btm_est ~ org_type, data = alpha_summary)
tukey_results <- TukeyHSD(anova_result)$org_type
df_tukey <- as.data.frame(tukey_results)
df_tukey$comparison_raw <- rownames(df_tukey)

# Establish ocmparison order
target_comparisons <- c(
  "Sector-Agri", "Cross-Agri", "Large-Agri", "Tech-Agri",
  "Sector-Cross", "Large-Cross", "Tech-Cross",
  "Sector-Large", "Tech-Large", "Tech-Sector"
)

# Create dataframe
df_tukey <- df_tukey %>%
  mutate(
    reversed = sub("(.*)-(.*)", "\\2-\\1", comparison_raw),
    comparison = case_when(
      comparison_raw %in% target_comparisons ~ comparison_raw,
      reversed %in% target_comparisons ~ reversed,
      TRUE ~ NA_character_
    ),
    diff = ifelse(comparison_raw %in% target_comparisons, diff, -diff),
    lwr  = ifelse(comparison_raw %in% target_comparisons, lwr, -upr),
    upr  = ifelse(comparison_raw %in% target_comparisons, upr, -lwr)
  ) %>%
  filter(comparison %in% target_comparisons) %>%
  mutate(comparison = factor(comparison, levels = target_comparisons)) %>%
  arrange(comparison)

# Plot
ggplot(df_tukey, aes(y = comparison, x = diff)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    y = "Comparison",
    x = "Difference in BTM Estimate"
  ) +
  theme_minimal()

# === Run alternative test with manually selected major exporters ===

fortune <- c(
  "IBM Corporation", "Chevron", "ConAgra Foods", "Conagra Brands", "Pfizer",
  "Dow Chemical Company", "Deere & Company", "Boeing Company", "Intel Corporation",
  "Cargill", "New York Life Insurance Company", "Novartis Corporation", "PepsiCo",
  "Walmart", "Citigroup", "Mars", "Chevron Upstream and Gas", "Bristol-Myers Squibb",
  "Federal Express Corporation", "Cisco Systems", "Mastercard", "MetLife",
  "General Electric Company", "Prudential Financial", "AbbVie", "Tesla", "ExxonMobil",
  "Caterpillar", "Procter & Gamble", "Ford Motor Company", "United Parcel Service",
  "Raytheon", "Abbott", "Campbell Soup Company", "Tyson Foods", "S&P Global",
  "Union Pacific Railroad", "Pepsico", "Cigna", "Coca-Cola"
)

alpha_summary_filtered <- alpha_summary %>%
  filter(
    (org_type == "Sector") |
      (org_type == "Large" & empresa %in% fortune)
  )

t_test_result <- t.test(btm_est ~ org_type, data = alpha_summary_filtered)
print(t_test_result)

######################## ROBUSTNESS TESTS ######################################

# === 1. Testing if BTm uncertainty and length influence the outputs ===

# Add average text length per organization
# File 'alpha_summary.csv' contains avg_length and standard errors

avg_length_df <- lobby_unique %>%
  mutate(length = nchar(middle)) %>%
  group_by(name) %>%
  summarise(avg_length = mean(length, na.rm = TRUE), .groups = "drop")

alpha_summary <- alpha_summary %>%
  left_join(avg_length_df, by = c("empresa" = "name"))

# Generate standard errors for WLS

alpha_summary <- alpha_summary %>%
  mutate(se_btm = (btm_upr - btm_lwr) / (2 * 1.96))

# Run OLS and WLS regressions accounting for uncertainty and comment length 

ols_len <- lm(btm_est ~ avg_length, data = alpha_summary)
summary(ols_len)

wls_len <- lm(btm_est ~ avg_length, data = alpha_summary, weights = 1 / se_btm^2)
summary(wls_len)

ols_type <- lm(btm_est ~ org_type, data = alpha_summary)
summary(ols_type)

wls_type <- lm(btm_est ~ org_type, data = alpha_summary, weights = 1 / se_btm^2)
summary(wls_type)

ols_type_len <- lm(btm_est ~ org_type + avg_length, data = alpha_summary)
summary(ols_type_len)

wls_type_len <- lm(btm_est ~ org_type + avg_length, data = alpha_summary, weights = 1 / se_btm^2)
summary(wls_type_len)

# Export regression results to LaTeX using stargazer

stargazer(
  ols_len, wls_len, ols_type, wls_type, ols_type_len, wls_type_len,
  type = "latex",
  title = "OLS and WLS Regressions",
  column.labels = c("OLS", "WLS", "OLS", "WLS", "OLS", "WLS"),
  dep.var.caption = "DV: BT score",
  covariate.labels = c("Average Text Length", "Org: Cross", "Org: Large", "Org: Sector", "Org: Tech"),
  omit.stat = c("f", "ser"),
  digits = 2,
  align = TRUE,
  no.space = TRUE
)

# === 2. Testing sensitivity to prompt variation ===
# Results are in 'sensitivity.csv' 

#  Create empty columns for prompt variants if not present ===
for (v in 1:5) {
  col <- paste0("variant_", v)
  if (!(col %in% names(dyads_sampled_1000))) {
    dyads_sampled_1000[[col]] <- NA_character_
  }
}

# === 6. Prompt generator for each variant ===
generate_prompt_variant_fixed <- function(text_A, text_B, variant, name_A = NULL, name_B = NULL) {
  prompt <- switch(as.character(variant),
                   "1" = paste0(
                     "You will be shown two texts. Your task is to determine which one is more explicitly supportive of the protection of foreign investment.\n\n",
                     "Be conservative. Only choose A or B if there is a clear difference.\n\n",
                     "Respond using only one character:\n",
                     "- 'A' if A is more supportive,\n",
                     "- 'B' if B is more supportive,\n",
                     "- 'T' if they are similar or unclear.\n\n",
                     "Comment A:\n", text_A, "\n\n",
                     "Comment B:\n", text_B
                   ),
                   "2" = paste0(
                     "You will be shown /two texts. Your task is to determine which one contains more mentions of the protection of foreign investment.\n\n",
                     "Respond using only one character:\n",
                     "- 'A' if Comment A contains more mentions,\n",
                     "- 'B' if Comment B contains more mentions,\n",
                     "- 'T' if both are similar or unclear.\n\n",
                     "Comment A:\n", text_A, "\n\n",
                     "Comment B:\n", text_B
                   ),
                   "3" = paste0(
                     "You will be shown two texts. Your task is to determine which one is more explicitly supportive of the protection of foreign investment.\n\n",
                     "Respond using only one character:\n",
                     "- 'A' if A is more supportive,\n",
                     "- 'B' if B is more supportive,\n",
                     "- 'T' if they are similar or unclear.\n\n",
                     "Comment A:\n", text_A, "\n\n",
                     "Comment B:\n", text_B
                   ),
                   "4" = paste0(
                     "You will be shown two texts. Your task is to determine which one contains more references to the protection of foreign investment.\n\n",
                     "Respond using only one character:\n",
                     "- 'A' if Comment A contains more references,\n",
                     "- 'B' if Comment B contains more references,\n",
                     "- 'T' if both are similar or unclear.\n\n",
                     "Comment A:\n", text_A, "\n\n",
                     "Comment B:\n", text_B
                   ),
                   "5" = paste0(
                     "You will be shown the names of two organizations. Based on what you know, which one is more explicitly supportive of the protection of foreign investment in trade agreements negotiated by the United States?\n\n",
                     "Respond using only one character:\n",
                     "- 'A' if A is more supportive,\n",
                     "- 'B' if B is more supportive,\n",
                     "- 'T' if they are similar or unclear.\n\n",
                     "Organization A:\n", name_A, "\n\n",
                     "Organization B:\n", name_B
                   ),
                   stop("Invalid variant number.")
  )
  return(prompt)
}

# Wrapper function to call classification for a single pair
classify_investment_single <- function(text_A, text_B, variant, name_A = NULL, name_B = NULL) {
  prompt <- generate_prompt_variant_fixed(text_A, text_B, variant, name_A, name_B)
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    headers,
    body = toJSON(list(
      model = "gpt-4.1-mini",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0,
      max_tokens = 20
    ), auto_unbox = TRUE),
    encode = "json"
  )
  if (status_code(response) == 200) {
    result <- content(response, as = "parsed")$choices[[1]]$message$content
    return(str_trim(toupper(result)))
  } else {
    return(NA)
  }
}

# Main loop to classify dyads_sampled_1000 
start_variant <- 1
start_row <- 1
pb <- progress_bar$new(
  total = sum(sapply(start_variant:5, function(v) nrow(dyads_sampled_1000))),
  format = "  Processing [:bar] :current/:total (:percent) eta: :eta"
)

for (v in start_variant:5) {
  col <- paste0("variant_", v)
  row_range <- if (v == start_variant) start_row:nrow(dyads_sampled_1000) else 1:nrow(dyads_sampled_1000)
  
  for (i in row_range) {
    if (!is.na(dyads_sampled_1000[[col]][i])) {
      pb$tick()
      next
    }
    
    res <- if (v == 5) {
      classify_investment_single(NA, NA, variant = v,
                                 name_A = dyads_sampled_1000$name_A[i],
                                 name_B = dyads_sampled_1000$name_B[i])
    } else {
      classify_investment_single(
        dyads_sampled_1000$text_A[i],
        dyads_sampled_1000$text_B[i],
        variant = v
      )
    }
    
    dyads_sampled_1000[[col]][i] <- ifelse(res %in% c("A", "B", "T"), res, NA)
    pb$tick()
    
    if (i %% 100 == 0 && v == start_variant) {
      write.csv(dyads_sampled_1000, "dyads_investment_wide_partial.csv", row.names = FALSE)
    }
    
    Sys.sleep(runif(1, 0.3, 1))  
  }
}

# Export final classified data
write.csv(dyads_sampled_1000, "dyads_sampled_1000.csv", row.names = FALSE)

# === 3. Create agreement and kappa matrices ===
matrix_res <- matrix(NA, 5, 5)
kappa_matrix <- matrix(NA, 5, 5)

for (i in 1:5) {
  for (j in 1:5) {
    col_i <- paste0("variant_", i)
    col_j <- paste0("variant_", j)
    
    matrix_res[i, j] <- mean(dyads_sampled_1000[[col_i]] == dyads_sampled_1000[[col_j]], na.rm = TRUE)
    
    df_temp <- dyads_sampled_1000 %>%
      select(all_of(c(col_i, col_j))) %>%
      filter(!is.na(.data[[col_i]]) & !is.na(.data[[col_j]]))
    
    if (ncol(df_temp) == 2 && nrow(df_temp) > 1 &&
        length(unique(df_temp[[1]])) > 1 &&
        length(unique(df_temp[[2]])) > 1) {
      colnames(df_temp) <- c("rater1", "rater2")
      kappa_matrix[i, j] <- tryCatch({
        kappa2(df_temp, "unweighted")$value
      }, error = function(e) NA)
    } else {
      kappa_matrix[i, j] <- NA
    }
  }
}

rownames(matrix_res) <- colnames(matrix_res) <- paste0("V", 1:5)
rownames(kappa_matrix) <- colnames(kappa_matrix) <- paste0("V", 1:5)

# Plot heatmaps for agreement and kappa
df_kappa <- melt(round(kappa_matrix, 3))
colnames(df_kappa) <- c("Variant_1", "Variant_2", "Kappa")

ggplot(df_kappa, aes(x = Variant_1, y = Variant_2, fill = Kappa)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Kappa, 2)), size = 4, na.rm = TRUE) +
  scale_fill_gradient(low = "#f7f7f7", high = "#08306b", na.value = "grey90", limits = c(0, 1)) +
  labs(title = "Cohen's Kappa", x = "Prompt Variant", y = "Prompt Variant", fill = "Kappa") +
  theme_minimal()

df_agreement <- melt(round(matrix_res, 3))
colnames(df_agreement) <- c("Variant_1", "Variant_2", "Agreement")

ggplot(df_agreement, aes(x = Variant_1, y = Variant_2, fill = Agreement)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Agreement, 2)), size = 4, na.rm = TRUE) +
  scale_fill_gradient2(low = "#f7f7f7", mid = "#f7f7f7", high = "#08306b", midpoint = 0.5, limits = c(0, 1), na.value = "grey90") +
  labs(title = "Correspondence Matrix", x = "Prompt Variant", y = "Prompt Variant", fill = "Agreement") +
  theme_minimal()

# === 4. Run Product-of-Experts using multiple GPT models ===
# For replication, load 'poe.csv'.

# Define models 
models <- c("gpt-4.1-mini", "gpt-4.1-nano", "gpt-4o", "gpt-4o-mini")

# Main functions
generate_prompt <- function(text_A, text_B) {
  paste0("Which is more supportive of foreign investor rights?\n",
         "A: ", text_A, "\n",
         "B: ", text_B, "\n\n",
         "Answer with A, B or T.")
}

get_logprobs_full <- function(model, prompt) {
  body <- list(
    model = model,
    messages = list(list(role = "user", content = prompt)),
    logprobs = TRUE,
    top_logprobs = 5,
    max_tokens = 1,
    temperature = 0
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json",
    headers
  )
  
  if (http_error(response)) {
    stop("Erro na requisição: ", content(response)$error$message)
  }
  
  content(response, as = "parsed", type = "application/json")
}

extract_ABC_logprobs <- function(response) {
  content_logprobs <- response$choices[[1]]$logprobs$content
  top_logprobs <- content_logprobs[[1]]$top_logprobs
  if (is.null(top_logprobs)) return(NULL)
  
  abc <- c("A", "B", "T")
  result <- lapply(abc, function(token) {
    entry <- top_logprobs[sapply(top_logprobs, function(x) x$token == token)]
    if (length(entry) == 0) return(tibble(token = token, logprob = NA_real_))
    tibble(token = token, logprob = entry[[1]]$logprob)
  })
  
  bind_rows(result) %>%
    mutate(prob = exp(logprob)) %>%
    mutate(prob = prob / sum(prob, na.rm = TRUE)) %>%
    select(token, prob)
}

run_all_models <- function(text_A, text_B) {
  prompt <- generate_prompt(text_A, text_B)
  
  model_probs <- map_dfr(models, function(m) {
    Sys.sleep(1.5)
    response <- get_logprobs_full(m, prompt)
    probs <- extract_ABC_logprobs(response)
    if (is.null(probs)) return(NULL)
    probs$model <- m
    probs
  })
  
  if (nrow(model_probs) == 0) return(NULL)
  
  df_wide <- model_probs %>%
    pivot_wider(names_from = token, values_from = prob) %>%
    summarise(
      A = prod(A, na.rm = TRUE),
      B = prod(B, na.rm = TRUE),
      T = prod(T, na.rm = TRUE)
    )
  
  poe_sum <- sum(df_wide, na.rm = TRUE)
  poe_probs <- df_wide / poe_sum
  
  poe_probs %>%
    pivot_longer(cols = everything(), names_to = "token", values_to = "poe_prob") %>%
    filter(poe_prob == max(poe_prob, na.rm = TRUE))
}

# Loop

results_path <- "partial_results_poe.rds"

if (file.exists(results_path)) {
  results_poe <- readRDS(results_path)
  processed_ids <- unique(results_poe$id)
} else {
  results_poe <- tibble()
  processed_ids <- integer()
}

dyads_to_run <- dyads_sampled_1000 %>%
  mutate(id = row_number()) %>%
  filter(!(id %in% processed_ids))

pb <- progress_bar$new(total = nrow(dyads_to_run), format = "[:bar] :current/:total ETA: :eta")

for (i in seq_len(nrow(dyads_to_run))) {
  row <- dyads_to_run[i, ]
  pb$tick()
  tryCatch({
    res <- run_all_models(row$text_A, row$text_B)
    if (!is.null(res)) {
      res <- mutate(res, id = row$id, name_A = row$name_A, name_B = row$name_B)
      results_poe <- bind_rows(results_poe, res)
      saveRDS(results_poe, results_path)
    }
  }, error = function(e) {
    message("Erro na linha ", row$id, ": ", e$message)
  })
}

# Export
write.csv(results_poe, "poe.csv")

# Summarize PoE 
summary(results_poe$poe_prob)

# === 5. Check for transitivity violations ===

test_transitivity <- function(df) {
  library(dplyr)
  
  # Filtra comparações válidas
  df_valid <- df %>%
    filter(text_comparison %in% c("A", "B")) %>%
    mutate(
      winner = ifelse(text_comparison == "A", name_A, name_B),
      loser  = ifelse(text_comparison == "A", name_B, name_A)
    ) %>%
    select(winner, loser)
  
  # Cria um dicionário: quem venceu quem
  win_dict <- split(df_valid$loser, df_valid$winner)
  
  orgs <- unique(c(df_valid$winner, df_valid$loser))
  violations <- 0
  total_checked <- 0
  
  for (A in orgs) {
    Bs <- win_dict[[A]]
    if (is.null(Bs)) next
    
    for (B in Bs) {
      Cs <- win_dict[[B]]
      if (is.null(Cs)) next
      
      for (C in Cs) {
        if (C == A) next  # ignora ciclos triviais
        total_checked <- total_checked + 1
        if (!is.null(win_dict[[C]]) && A %in% win_dict[[C]]) {
          violations <- violations + 1
        }
      }
    }
  }
  
  cat("Verified Triplets:", total_checked, "\n")
  cat("Transitivity Violations:", violations, "\n")
  cat("Violations Proportions:", round(violations / total_checked, 4), "\n")
}

test_transitivity(dyads_filtered)

# Find percentage of violations 

find_transitivity_violations_with_rate <- function(df) {
  df_valid <- df %>%
    filter(text_comparison %in% c("A", "B")) %>%
    mutate(
      winner = ifelse(text_comparison == "A", name_A, name_B),
      loser  = ifelse(text_comparison == "A", name_B, name_A)
    ) %>%
    select(winner, loser)
  
  # Create lookup: who beat whom
  win_dict <- split(df_valid$loser, df_valid$winner)
  orgs <- unique(c(df_valid$winner, df_valid$loser))
  
  # Initialize
  violations <- list()
  count <- 1
  involvement <- c()
  
  # Search for intransitive triads: A > B > C but C > A
  for (A in orgs) {
    Bs <- win_dict[[A]]
    if (is.null(Bs)) next
    for (B in Bs) {
      Cs <- win_dict[[B]]
      if (is.null(Cs)) next
      for (C in Cs) {
        if (C == A) next
        if (!is.null(win_dict[[C]]) && A %in% win_dict[[C]]) {
          violations[[count]] <- data.frame(A = A, B = B, C = C)
          involvement <- c(involvement, A, B, C)
          count <- count + 1
        }
      }
    }
  }
  
  df_violations <- bind_rows(violations)
  
  # Count how many times each org appears in any violation
  viol_counts <- as.data.frame(table(involvement)) %>%
    rename(organization = involvement, violations = Freq)
  
  # Count total number of comparisons involving each org
  compare_counts <- df %>%
    select(name_A, name_B) %>%
    pivot_longer(everything(), values_to = "organization") %>%
    count(organization, name = "comparisons")
  
  # Join and calculate violation rate
  results <- full_join(viol_counts, compare_counts, by = "organization") %>%
    mutate(
      violations = ifelse(is.na(violations), 0, violations),
      rate = violations / comparisons
    ) %>%
    arrange(desc(rate))
  
  return(list(
    violations_df = df_violations,
    proportion_df = results
  ))
}

# Run on dyads_filtered
results <- find_transitivity_violations_with_rate(dyads_filtered)

# Top 20 orgs by transitivity violation rate
top_orgs <- results$proportion_df %>% slice_max(rate, n = 40)

# Plot
ggplot(top_orgs, aes(x = reorder(organization, rate), y = rate)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Transitivity Violation Rate by Organization",
    x = NULL,
    y = "Proportion of Violations"
  ) +
  theme_minimal()

################################################################################
############################## END OF SCRIPT ###################################
################################################################################

sink("session_info.txt")
sessionInfo()
sink()
