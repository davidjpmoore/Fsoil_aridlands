# 0) You’ve already run the refactored workflow (run_all.R).
#    That created: out/chamber/chamber_final_dataset.rds

# 1) Load and run the investigation
source("R/99_chamber_model_investigation.R")
res <- run_chamber_model_investigation()   # writes outputs to out/model_eval/chamber/

# 2) Quick look at results
readr::read_csv("out/model_eval/chamber/metrics_chamber.csv") |> print(n=Inf)
readr::read_csv("out/model_eval/chamber/wald_pulse_effects.csv") |> print()

# 3) (Optional) Produce a short auto-summary markdown
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
library(readr); library(dplyr); library(glue)

met   <- read_csv("out/model_eval/chamber/metrics_chamber.csv", show_col_types = FALSE)
wald  <- suppressWarnings(read_csv("out/model_eval/chamber/wald_pulse_effects.csv",
                                   show_col_types = FALSE))
best  <- met |> arrange(AICc) |> slice(1)
delta <- met |> mutate(deltaAICc = AICc - min(AICc))

txt <- glue("
# Chamber model comparison — quick summary

**Data**: out/chamber/chamber_final_dataset.csv  
**Models compared**: Lumped (M0) vs Pulse-split (M1)

## Model selection
Best (by AICc): **{best$model}** (AICc = {round(best$AICc,2)}, k = {best$k}, n = {best$n})  
ΔAICc:  
{paste0(delta$model, ': ', round(delta$AICc,2), collapse='; ')}

CV errors (10-fold):  
- M0 CV_RMSE = {round(met$CV_RMSE[met$model=='M0_lumped'],3)}, CV_MAE = {round(met$CV_MAE[met$model=='M0_lumped'],3)}  
- M1 CV_RMSE = {round(met$CV_RMSE[met$model=='M1_split'],3)},  CV_MAE = {round(met$CV_MAE[met$model=='M1_split'],3)}

## Pulse parameter shifts (Wald tests in M1)
{if (nrow(wald)) paste0(
  wald |> mutate(line = glue('- {param}: est = {round(estimate,3)}, SE = {round(se,3)}, p = {signif(p,3)}')) |> pull(line) |> paste(collapse='\n')
) else 'No pulse deltas tested (or split model not applicable).'}

## Quick read
- {if (best$model=='M1_split') 'Evidence favors pulse-split.' else 'Parsimonious lumped model favored.'}
- Predictive performance: {ifelse(best$model=='M1_split' & met$CV_RMSE[met$model=='M1_split'] < met$CV_RMSE[met$model=='M0_lumped'],
                                  'M1 improves CV error.',
                                  'No clear CV gain for M1.')}

## Next steps
- Inspect residual PNGs in out/model_eval/chamber/ (look for bias vs ST5/SWC5 or wider spread in pulse).
- If pulse residuals are noisier, consider weighted NLS / GNLS with varIdent(~1|pulse).
")

out_md <- "out/model_eval/chamber/summary_quick.md"
writeLines(txt, out_md)
cat('Wrote: ', out_md, '\n')
