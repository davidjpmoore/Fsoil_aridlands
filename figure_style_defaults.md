# Figure Style Defaults for Lab Projects

A starter guide for figure design across Moore Lab projects. This document captures the working principles, defaults, and conventions that any new lab project should inherit. Project-specific style files can extend or override these defaults, but new projects should start here.

The intent is twofold. First, to give Claude (or any analyst) a sensible default starting point so figures from a fresh project look coherent without negotiation. Second, to make the design choices explicit so they can be discussed, refined, and overridden when a project's needs differ.

## Working Principles

**Figures are documents, not afterthoughts.** A figure is usually the single most-viewed piece of a paper. Treat it with the same care as the prose. If a reader can extract the figure's message without reading the caption, the figure is doing its job.

**Less chrome, more data.** Default to minimal axes, no grid lines, no decorative borders, no extraneous legends. Add elements only when they help the reader interpret the figure.

**Constants over inline styling.** Define theme constants, palettes, dimensions, and font sizes once in a project-level style file. Source them everywhere. Inline styling drift produces inconsistent figures and is hard to maintain.

**Three figure categories, three different sets of conventions.** Diagnostic figures, publication figures, and presentation figures serve different audiences and live in different folders. Don't try to make one figure serve all three roles.

**Plotmath for math, never HTML.** R's plotmath expressions are the standard for superscripts, subscripts, and Greek letters in ggplot2. HTML tags like `<sup>` render literally in most output formats.

**Greyscale should still work** for many projects. Even when a journal accepts color, readers may print in greyscale and slides may be displayed on washed-out projectors. Color palettes that retain meaning in greyscale (viridis, sequential brewer palettes) protect against both.

## Three Figure Categories

| Category | Audience | Folder | Saved as |
|---|---|---|---|
| Diagnostic | Analyst | `review/figures/` | PNG, default dimensions |
| Publication | Journal readers | `final/figures/` | PNG (until journal submission) |
| Presentation | Slide audience | `presentation/figures/` | PNG, 16:9 dimensions |

### Diagnostic figures

Made quickly during analysis. The goal is to see the data. Show every category, every group, every outlier. Default ggplot2 theme is fine. Legend present and labeled clearly so the analyst can interpret without ambiguity. Labels can be terse; units can be implicit. Save dimensions can be whatever fits — 7 × 5 in is a reasonable default.

Diagnostic figures get cleaned up later if they earn promotion to publication or presentation status. Until then, they live in `review/figures/` and don't need to be perfect.

### Publication figures

Made deliberately for the published paper. Single message per figure. Every label has units. Every panel has a purpose. Color and dimensions chosen to work at journal column widths. Legend designed to fit the figure, not added as an afterthought.

Each publication figure has a companion `<figure_name>_caption.txt` file in the same folder. The caption file describes what the figure shows in prose — enough that you can copy it (lightly edited) into the manuscript later. Writing the caption alongside the figure forces articulation of the message and surfaces cases where the figure isn't doing its job.

Publication figures are saved as PNG by default during development. Format conversion to journal-specific requirements (PDF, EPS, TIFF) happens at submission time and is not encoded in this style guide.

### Presentation figures

Made for slides. Larger base font sizes (text needs to be readable from the back of a room, not on a printed page). Wider aspect ratios (16:9 friendly). Often simplified — fewer panels, fewer categories, inline annotations instead of legends. Higher contrast colors.

A publication figure dropped onto a slide is usually unreadable. Don't try to repurpose one for the other.

## Default Theme

A minimal publication-ready theme that works as a starting point for most projects. The principles: minimal chrome, inward ticks (publication convention), thin black panel border, no grid lines, no legend title by default.

```r
theme_pub <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(-3, "pt"),
      axis.text.x = element_text(margin = margin(t = 6)),
      axis.text.y = element_text(margin = margin(r = 6)),
      legend.title = element_blank(),
      legend.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
}
```

Notes on the choices:

- **`base_size = 11`** is a sensible default for publication figures saved at single-column width (~3.5 in). Override to 24 for figures designed at 2× then scaled (typical for double-column figures saved at 14 × 7 in). Override to 18+ for presentation figures.
- **Inward ticks** are a publication convention — they keep the axis chrome inside the panel and produce a cleaner look. The compensating axis text margins (`margin(t = 6)` and `margin(r = 6)`) prevent labels from colliding with the panel border.
- **No legend title by default** because legend titles often duplicate the variable name shown elsewhere. Add a title when it carries information that isn't otherwise visible.
- **`strip.background = element_blank()`** removes the grey rectangle behind facet labels — usually visual noise.

For diagnostic figures, use `theme_bw()` defaults. Save the polish for figures that earn it.

## Default Color Conventions

**For sequential (ordered) data:** `viridis` — perceptually uniform, colorblind-safe, greyscale-safe. Use `scale_color_viridis_c()` or `scale_fill_viridis_c()`. The `option = "magma"` variant works well when "high values are bad" framing is appropriate.

**For diverging data (anomalies, differences from a reference):** `RdBu` or `BrBG` from RColorBrewer via `scale_color_distiller(palette = "RdBu")`. Use these when the data has a meaningful midpoint.

**For categorical data with ≤8 categories:** the Okabe-Ito palette is colorblind-safe and distinguishable in greyscale.

```r
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
```

**For two-category figures where greyscale is preferred:** black for the "foreground" or "treatment" group, white (or very light grey) for the reference, both with black borders. This is the pulse-impact figure convention and works well for publication figures destined for greyscale print.

**Avoid by default:** rainbow palettes, jet, default ggplot2 hue scales for ordered data. They are not perceptually uniform and they collapse to a mess in greyscale.

## Save Dimensions

Target **Nature family** column widths as the working default, since they are tighter than most ecology/Earth science journals and produce figures that work everywhere.

| Use case | Width | Height | DPI | Notes |
|---|---|---|---|---|
| Single column publication | 89 mm (3.5 in) | varies, often 3–4 in | 300 | Smallest text 6 pt at final size |
| Double column publication | 183 mm (7.2 in) | varies, often 4–6 in | 300 | Smallest text 6 pt at final size |
| Design-at-2x for double column | 14 in | 7 in | 300 | Use `base_size = 24`, scale down at journal submission |
| Diagnostic | 7 in | 5 in | 150 | Adequate, fast |
| Presentation (16:9) | 13.33 in | 7.5 in | 200 | Full slide |
| Presentation (half slide) | 6.5 in | 7.5 in | 200 | Two figures side by side |

The design-at-2x approach is useful when you want figures to look good both on screen during development and in print at journal scale. It requires committing to a base font size that scales appropriately (24 pt at 14 × 7 in becomes 12 pt at 7 × 3.5 in).

## Multi-Panel Conventions

**Panel labels:** lowercase letters (a, b, c), bold, positioned upper-left of each panel inside the plotting area. See §Multi-Panel Layout Conventions for the standard annotate() approach. Some journals prefer uppercase (A, B, C) — check before submission.

**Shared legend:** when multiple panels use the same aesthetic mapping, place the legend inside one panel (the rightmost by convention) rather than duplicating it or floating it outside the figure. See §Multi-Panel Layout Conventions Rule 4.

**Panel alignment:** use `align = "hv"` (ggpubr) or `align()` (patchwork) so axes line up cleanly across panels. Misaligned axes look amateurish and make panel comparison harder.

**One message per figure:** if a multi-panel figure is showing three unrelated things, it's three figures, not one. The exception is when the panels are deliberately telling a comparative story.

## Multi-Panel Layout Conventions

These four rules apply to all publication figures with shared axes or shared legends. They encode lab decisions made after reviewing journal submission requirements — project style files should not override them without a documented reason.

### Rule 1 — Shared y-axis row

When a row of panels shares the same y-axis scale, show the y-axis title and tick **labels** on the leftmost panel only. Suppress the labels and title on all other panels in the row, but **never suppress the tick marks** — tick marks must remain visible on all four sides of every panel. Set tight margins on the shared sides so panels appear to touch.

`theme_pub()` now draws inward ticks on all four sides of every panel by default (via `axis.ticks.x.top`, `axis.ticks.y.right`, and their corresponding `axis.ticks.length.*` elements). On non-leftmost panels, suppress only `axis.text.y`. Suppress the y-axis title via `labs(y = NULL)` or `axis.title.y = element_blank()`. Never set `axis.ticks.y = element_blank()` on a shared-axis panel.

```r
# Leftmost panel (a): normal y-axis
p_a <- p_a + theme(plot.margin = margin(t=5, r=2, b=5, l=5, unit="pt"))

# Non-leftmost panels (b, c): suppress labels and title only — keep tick marks
p_b <- p_b + theme(
  axis.text.y  = element_blank(),
  axis.ticks.y = element_line(color = "black", linewidth = 0.4),  # keep marks visible
  axis.title.y = element_blank(),   # or set y = NULL in labs()
  plot.margin  = margin(t=5, r=2, b=5, l=2, unit="pt")
)
```

### Rule 2 — Shared x-axis row

When a row of panels shares the same x-axis variable, retain tick labels (values) on all panels so readers can read exact positions without looking across the figure.

**Critical:** do NOT put the x-axis label on one panel via `labs(x = "...")` while using `labs(x = NULL)` on the others. Layout engines (ggpubr, patchwork, gridExtra) allocate vertical space for the label inside the panel that has it, making that panel's plot area shorter than the others — the panels look the same size in total allocated space but the data regions have different heights.

The correct approach: set `labs(x = NULL)` on every panel in the row, then add the shared label as a `textGrob` in a thin dedicated row below all three panels using `arrangeGrob()`. The label spans the full width of the panel row and all three panels remain identical in height.

```r
library(gridExtra); library(grid)

# Every panel in the row: suppress the x-axis label
p_a <- p_a + labs(x = NULL)
p_b <- p_b + labs(x = NULL)   # <-- NOT labs(x = "Days since rain event")
p_c <- p_c + labs(x = NULL)

# Build the row grob and the shared label grob separately
row_grob  <- arrangeGrob(p_a, p_b, p_c, ncol = 3)
xlab_grob <- textGrob("Days since rain event",
                       vjust = 0.5, gp = gpar(fontsize = 10))

# Stack: panel row / thin label row
layout <- arrangeGrob(
  row_grob, xlab_grob,
  nrow    = 2,
  heights = unit(c(1, 0.07), "null")
)

# Save with png() + grid.draw() because the result is a grob, not a ggplot
png("fig.png", width = 7.2, height = 3, units = "in", res = 300, bg = "white")
grid.draw(layout)
dev.off()
```

### Rule 3 — Panel labels inside the plot area

Place panel labels (a, b, c, …) inside the plotting area using `annotate()`. Do not use `ggpubr::ggarrange(labels = …)` or add them to the margin. Inside placement keeps labels within the figure bounding box, prevents them from being cropped by journal layout systems, and keeps the panel label visually associated with the data it labels.

```r
# Add to every panel — change only the label string
p <- p + annotate(
  "text",
  x         = -Inf, y    = Inf,
  label     = "a",
  hjust     = -0.3, vjust = 1.3,
  fontface  = "bold",
  size      = 5
)
```

`x = -Inf, y = Inf` snaps to the top-left corner of the coordinate system. `hjust = -0.3` nudges the text 30% of its width to the right of the left edge; `vjust = 1.3` drops it 30% of its height below the top edge. Both keep the label fully inside the panel.

Remove labels from `ggarrange()` when using this approach:

```r
ggarrange(p_a, p_b, p_c, nrow = 1, ncol = 3)   # no labels = argument
```

### Rule 4 — Legend placement in multi-panel figures

Place the legend inside the rightmost panel of the relevant row. Position it in the top-right corner using `legend.position` and `legend.justification`. Remove all legend box backgrounds and borders — the legend floats on the data without a box. Never place the legend outside the figure or duplicate it across panels.

```r
# Rightmost panel: legend inside, top-right, no border
p_f <- p_f + theme(
  legend.position      = c(0.97, 0.97),
  legend.justification = c("right", "top"),
  legend.background    = element_blank(),
  legend.box.background = element_blank(),
  legend.text          = element_text(size = 9),
  legend.key.size      = unit(1.0, "lines")
)

# All other panels: legend suppressed
p_d <- p_d + theme(legend.position = "none")
p_e <- p_e + theme(legend.position = "none")
```

## Caption Companion Files

Every publication figure has a `<figure_name>_caption.txt` file in the same folder. The file contains a prose description of the figure suitable for use as the manuscript caption (with light editing).

A useful caption structure:

1. **One-sentence summary** of what the figure shows.
2. **Description of each panel or data element** in the order the reader would naturally scan them.
3. **Methodological details a reader needs to interpret the figure** — sample size, statistical test, error bar definition, color scale meaning.
4. **What the figure does *not* show** if there's a common misreading to head off.

Example:

```
fig_03_choropleth_current_caption.txt

Geographic distribution of currently active FLUXNET sites (n = 569 with valid annual NEE)
as of the April 2026 Shuttle release. Polygons are countries colored by site count
(viridis scale, 0–60 sites). Individual sites are shown as white-filled circles with
black outlines. Currently active is defined as having at least three months of valid
NEE data in any of the four years 2022–2025.
```

The caption file does not need to be production-quality manuscript prose. It needs to be enough that you, six months from now, can read it and remember what the figure was supposed to show.

## Things to Always Do

- Source theme constants and palettes from a single project-level file, not inline
- Use plotmath for super/subscripts and Greek letters
- Set `bg = "white"` explicitly in `ggsave()` to avoid transparent PNG backgrounds
- Test the figure in greyscale (convert to greyscale or print on a greyscale printer) for any figure that may be reproduced that way
- Write the caption file alongside the figure for publication figures
- Save publication figures as PNG during development; convert to journal format at submission

## Things to Always Avoid

- HTML tags (`<sup>`, `<sub>`) in ggplot text — they render literally in PNG and PDF output
- Default ggplot2 hue scales for ordered or sequential data
- Rainbow or jet palettes anywhere
- Legend titles that duplicate the axis label or variable name
- Decorative grid lines on publication figures
- Saving with transparent backgrounds (use `bg = "white"`)
- Mixing diagnostic-quality and publication-quality figures in the same output folder
- Adding panel labels (a, b, c) in the figure margin using `ggpubr::ggarrange(labels = …)` — they can be cropped by journal layout systems; use `annotate()` inside the panel instead (see §Multi-Panel Layout Conventions Rule 3)

## When to Override These Defaults

This document is a starting point, not a constraint. Override when:

- A project has its own style file with project-specific conventions (these take precedence)
- A target journal has specific requirements (font, dimensions, palette) that differ
- A figure type genuinely needs something this guide doesn't cover (custom map projections, network diagrams, phylogenetic trees, etc.)
- A presentation requires a different design language than the publication version

When overriding, document the reason in the project's own style file so the choice is visible to future readers and to Claude.
