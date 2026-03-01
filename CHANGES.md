# CHANGES.md — Bug Status Report

Branch: `bug-fixes`
Last updated: 2026-03-01

---

## Summary

| Bug | Script | Status |
|-----|--------|--------|
| Bug 1 | `R/07_threshold15_RECO.R` | **Fixed** (commit `e7f91c5`) |
| Bug 2 | `R/07_threshold15_RECO.R` | **Fixed** (this branch) |
| Bug 3 | `R/12_13_chamber_models.R` | **Fixed** (this branch) |
| Bug 4 | `R/14_Robust_RsoilModels.R` | **Outstanding** |
| Bug 5 | All model scripts | **Outstanding** |

---

## Fixed bugs

### Bug 1 — `07_threshold15_RECO.R`: GPPmax mismatch between fitting and prediction

**Commit:** `e7f91c5`

**Problem:** The NonPulse and Pulse models were fitted using per-subset GPPmax values
(`GPPmax_NP_15`, `GPPmax_P_15`), while predictions used the global `All_GPPmax`. Because
`Fref` and `n` are calibrated relative to GPPmax, using a different GPPmax at prediction
time produced physically inconsistent predictions.

**Fix:** Removed the per-subset GPPmax columns. Changed the `m_np` and `m_p` model
formulas to use `All_meanGPP/All_GPPmax` throughout, consistent with `m_all` and all
prediction blocks. GPPmax is now always the global maximum across all days in both fitting
and prediction.

**Files changed:** `R/07_threshold15_RECO.R` (lines 27–38)

---

### Bug 2 — `07_threshold15_RECO.R`: Classification mismatch in `Reco_Combined`

**Commit:** this branch

**Problem:** The NonPulse and Pulse RECO models are trained on SWC-threshold splits
(`SWC < 15%` vs `SWC ≥ 15%`), but `Reco_Combined` switched between them using the
rainfall-event label (`max_pulse_duration == 0` vs `c(8, 14, 20)`). A pulse-window day
that has dried below 15% SWC would have been assigned the Pulse model parameters, and a
non-pulse day retaining moisture above 15% would have been assigned the NonPulse model
parameters, both inconsistently with how those models were trained.

**Fix:** Replaced the `case_when` block (lines 60–64) with a single `ifelse` switching
on the same criterion used for training:

```r
# Before (lines 55–64):
out <- ys1 %>%
  select(date, meanRECO, max_pulse_duration) %>%
  mutate(NonPulseM_15 = pred_np,
         PulseM_15    = pred_p,
         MeanM_15     = pred_all,
         Reco_Combined = dplyr::case_when(
           max_pulse_duration == 0          ~ NonPulseM_15,
           max_pulse_duration %in% c(8,14,20) ~ PulseM_15,
           TRUE                             ~ MeanM_15
         ))

# After:
out <- ys1 %>%
  select(date, meanRECO, All_meanSWC5) %>%
  mutate(NonPulseM_15 = pred_np,
         PulseM_15    = pred_p,
         MeanM_15     = pred_all,
         Reco_Combined = ifelse(All_meanSWC5 >= 0.15, PulseM_15, NonPulseM_15))
```

The spurious `MeanM_15` fallback case (which was never triggered in practice but masked
the mismatch) is also removed.

**Files changed:** `R/07_threshold15_RECO.R` (lines 55–60)

---

### Bug 3 — `12_13_chamber_models.R`: Classification mismatch in `pred_Thr`

**Commit:** this branch

**Problem:** The NonPulse and Pulse Rsoil models (`fit_NP`, `fit_P`) are trained on
rainfall-event-based splits from `NonPulse_sum_chamber.csv` and `Pulse_sum_chamber.csv`,
which are produced by `08_read_chamber.R` using the `pulseIND` flag
(`days_since_rain_event < max_pulse_duration`). The threshold switch on line 92 applied
them using the SWC criterion (`meanSWC >= 0.15`), which is a different — and not
equivalent — classification. A pulse-window day that dried below 15% SWC would have been
routed to the NonPulse model, contrary to how it was trained.

**Fix:** Two lines changed:

1. Added `max_pulse_duration` to the `select()` on line 86 so the rainfall-event label
   is available at switch time.
2. Changed the switch criterion on line 92 from `meanSWC >= 0.15` to
   `max_pulse_duration > 0`, matching the rainfall-event criterion used in training.

```r
# Before (line 86):
new_all <- df_all %>% select(date, meanSWC, meanTsoil, meanGPP, meanRsoil) %>% mutate(GPPmax=GPPmax_global)

# After:
new_all <- df_all %>% select(date, meanSWC, meanTsoil, meanGPP, meanRsoil, max_pulse_duration) %>% mutate(GPPmax=GPPmax_global)

# Before (lines 91–92):
# --- threshold switch (>= 0.15 -> Pulse fit; else NonPulse fit)
pred_Thr <- ifelse(new_all$meanSWC >= 0.15, pred_P, pred_NP)

# After:
# --- threshold switch (pulse event -> Pulse fit; else NonPulse fit)
pred_Thr <- ifelse(new_all$max_pulse_duration > 0, pred_P, pred_NP)
```

**Files changed:** `R/12_13_chamber_models.R` (lines 86, 91–92)

---

## Outstanding bugs

### Bug 4 — `14_Robust_RsoilModels.R`: Data leakage in `cv_year_block_threshold`

**Location:** `R/14_Robust_RsoilModels.R`, lines 217–228

**Problem:** The `cv_year_block_threshold()` function receives `pred_NP_all` and
`pred_P_all` that were generated from models fitted on **all years**, including the
held-out year. The reported cross-validation error for the threshold model (`cv_Thr`) is
therefore optimistic and not comparable to `cv_All`, `cv_NP`, and `cv_P`, which are true
leave-one-year-out CV scores (models re-fitted on training years only inside the loop).

**Required fix:** Inside the CV loop, refit the NP and Pulse component models on the
training years, then generate predictions for the held-out year using those
training-only fits, and apply the threshold switch to those held-out predictions.

**Status:** Not yet fixed. Awaiting confirmation of implementation approach before
proceeding.

---

### Bug 5 — All model scripts: moisture term can go negative at high SWC

**Location:** `R/07_threshold15_RECO.R`, `R/12_13_chamber_models.R`,
`R/14_Robust_RsoilModels.R`

**Problem:** The moisture term in the respiration model is:

```
1 - c4 × (0.1 - SWC)²
```

With the current upper bound of `c4 = 100–200` and pulse-period SWC values reaching
0.25–0.35, this term can become negative, producing physically impossible negative
respiration predictions. For example: `c4 = 50`, `SWC = 0.25` gives
`1 - 50 × 0.0225 = -0.125`. No clamp is applied after prediction.

**Required fix (two options, to be confirmed):**

- **Option A:** Tighten the upper bound on `c4` to `1 / (SWC_max_observed - 0.1)²`,
  preventing the moisture term from reaching zero within the observed data range.
- **Option B:** Clamp predictions to ≥ 0 after computing, preventing negative output
  without constraining the optimiser.

**Status:** Not yet fixed. Scientific decision on preferred approach needed before
proceeding.
