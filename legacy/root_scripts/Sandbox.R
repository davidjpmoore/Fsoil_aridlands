library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

#--- Define file pairs ---
file_pairs <- tibble(
  original = c("data/years_sum1_DM.csv",
               "data/years_sum_Pulse0_DM.csv",
               "data/years_sum_Pulse1_DM.csv"),
  new      = c("output/years_sum1_DM.csv",
               "output/years_sum_Pulse0_DM.csv",
               "output/years_sum_Pulse1_DM.csv")
)

#--- Comparison function ---
compare_files <- function(orig_path, new_path, key_vars = NULL, id_vars = NULL) {
  # Read both files
  df_orig <- read_csv(orig_path, show_col_types = FALSE)
  df_new  <- read_csv(new_path, show_col_types = FALSE)
  
  # Guess id columns if not provided (try to use the leftmost non-numeric column)
  if (is.null(id_vars)) {
    id_vars <- names(df_orig)[sapply(df_orig, function(x) !is.numeric(x))][1]
    if (is.na(id_vars)) id_vars <- NULL
  }
  
  # Only use numeric columns that exist in both
  num_vars <- intersect(names(df_orig)[sapply(df_orig, is.numeric)],
                        names(df_new)[sapply(df_new, is.numeric)])
  if (!is.null(key_vars)) num_vars <- intersect(num_vars, key_vars)
  
  # Join on id_vars if present, else assume order matches
  if (!is.null(id_vars) && id_vars %in% names(df_orig) && id_vars %in% names(df_new)) {
    df_merged <- full_join(
      df_orig %>% select(all_of(id_vars), all_of(num_vars)),
      df_new  %>% select(all_of(id_vars), all_of(num_vars)),
      by = id_vars,
      suffix = c("_orig", "_new")
    )
  } else {
    df_merged <- bind_cols(
      df_orig %>% select(all_of(num_vars)) %>% rename_with(~paste0(.x, "_orig")),
      df_new  %>% select(all_of(num_vars)) %>% rename_with(~paste0(.x, "_new"))
    )
  }
  
  # Gather differences
  diffs <- map_dfr(num_vars, function(v) {
    orig_col <- paste0(v, "_orig")
    new_col  <- paste0(v, "_new")
    tibble(
      variable = v,
      diff = df_merged[[new_col]] - df_merged[[orig_col]]
    )
  })
  
  # Summary
  summary_stats <- diffs %>%
    group_by(variable) %>%
    summarize(
      mean_diff = mean(diff, na.rm=TRUE),
      sd_diff   = sd(diff, na.rm=TRUE),
      min_diff  = min(diff, na.rm=TRUE),
      max_diff  = max(diff, na.rm=TRUE),
      n         = n()
    )
  
  # Plot
  diff_plot <- ggplot(diffs, aes(x = diff)) +
    geom_histogram(bins = 30) +
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    labs(title = paste("Difference:", basename(orig_path), "vs", basename(new_path)),
         x = "New - Original", y = "Count")
  
  print(diff_plot)
  print(summary_stats)
  
  # Return both for further processing if needed
  invisible(list(summary = summary_stats, plot = diff_plot, all_diffs = diffs))
}

#--- Run comparisons ---
results <- pmap(file_pairs, ~ compare_files(..1, ..2))
