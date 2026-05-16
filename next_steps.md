# Next Steps
Last updated: 2026-05-15

---

## Must do — start of next session

- Regenerate all figures in final/figures/ to pick up the four-sided tick fix applied to
  theme_pub() today. Figures 1-9 were all built before or during the tick fix and may not
  reflect the final theme_pub() state. Run each figure script in sequence:
  source("R/106_Fig1_precip.R")
  source("R/108_Fig2_PulseContribution.R")
  source("R/107_Fig3_seasonal.R")
  source("R/110_Fig4b_GPP_resp_twopanel.R")
  source("R/111_Fig5_env_controls.R")
  source("R/112_Fig6_Reco_residuals.R")
  source("R/113_Fig7_Rsoil_residuals.R")
  source("R/114_Fig8a_Reco_modelvobs.R")
  source("R/115_Fig8b_Rsoil_modelvobs.R")
  source("R/116_Fig9_model_pulse_contribution.R")

---

## Figure review — awaiting PI and Anastasia feedback

All 9 publication figures are committed to final/figures/ with companion caption .txt files.
Review priority:

- Fig 3 — confirm normalisation (day 0 = 1) is the right choice for showing seasonal pulse shape
- Fig 4 — two-panel version approved. Delete single-panel scripts (109_Fig4a_*).
- Fig 5 — confirm eddy tower SWC used consistently across all four panels is scientifically
  appropriate
- Figs 6 and 7 — confirm whether shared or independent y-axis scales are appropriate
- Fig 8 — decide whether to include Reco only, Rsoil only, or both in the paper
- Fig 9 — confirm whether manuscript figure or supplementary

---

## Scripts to add to run_all.R once figures are approved

Scripts 106-116 are not yet wired into run_all.R. Add them to Stream 3 in order once
all figures are reviewed and approved. Scripts 103, 104, 105 are currently disabled
pending decision on whether to refactor or retire.

---

## Open architectural decisions

- Scripts 12 and 13 — formally retired or still active?
- SWC-threshold Rsoil switch — should 12_13_chamber_models.R be extended to include
  a SWC-threshold switch for a true three-way Rsoil comparison?
- Single-panel Fig 4a scripts (109_*) — delete once two-panel version confirmed

---

## Deferred infrastructure tasks

- Validation tests on intermediate CSV files
- End-to-end in-memory pipeline — deferred until after manuscript submission
- ggplot2 deprecation warnings (size -> linewidth) — 21 warnings, fix in next figure pass
- figure_style_defaults.md — consider moving to shared Moore Lab repository
