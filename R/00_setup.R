# 00_setup.R --------------------------------------------------------------

# Packages
pkgs <- c(
  "dplyr","tidyr","readr","lubridate","ggplot2","ggpubr","purrr","stringr",
  "minpack.lm","broom","scales","rlang"
)
invisible(lapply(pkgs, require, character.only = TRUE))

# Folders
dir.create("out/figs", recursive = TRUE, showWarnings = FALSE)
dir.create("out/derived", recursive = TRUE, showWarnings = FALSE)

# Plot theme
theme_set(
  theme_bw(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.ticks.length = unit(2, "pt"))
)

# Saver
save_plot <- function(p, filename, w=6, h=4, dpi=300) {
  ggsave(file.path("out/figs", filename), p, width = w, height = h, dpi = dpi)
}

# Safe bounded nlsLM
`%||%` <- function(a,b) if (is.null(a)) b else a
fit_nlsLM <- function(formula, data, start, lower=NULL, upper=NULL, ...) {
  nlsLM(formula, data = data, start = start,
        lower = lower %||% rep(-Inf, length(start)),
        upper = upper %||% rep( Inf, length(start)),
        control = nls.lm.control(maxiter = 1000), ...)
}

# Metrics
rmse <- function(obs, mod) sqrt(mean((obs - mod)^2, na.rm=TRUE))
mape <- function(obs, mod) mean(abs((mod - obs)/obs), na.rm=TRUE)*100
rsq  <- function(obs, mod) cor(obs, mod, use="complete.obs")^2



# --- search helpers ------------------------------------------------------

# Grep a directory of scripts for lines that (likely) create/assign a target object
search_assignments <- function(targets, dir = "R") {
  stopifnot(dir.exists(dir))
  files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE)
  if (!length(files)) stop("No R scripts found in: ", dir)
  
  assign_patterns <- function(t) {
    # assignment patterns (left-hand side)
    c(
      sprintf("(^|\\s)%s\\s*<-", t),                  # name <-
      sprintf("(^|\\s)%s\\s*=\\s*[^=]", t),           # name = (not ==)
      sprintf("assign\\s*\\(\\s*['\"]%s['\"]", t),    # assign("name", ...)
      sprintf("mutate\\s*\\([^)]*\\b%s\\s*=", t)      # mutate(..., name = ...)
    )
  }
  
  use_patterns <- function(t) {
    # generic uses (read joins, selects, filters) — for context
    c(
      sprintf("\\b%s\\b", t),
      sprintf("select\\s*\\([^)]*\\b%s\\b", t),
      sprintf("left_join\\s*\\([^)]*\\b%s\\b", t),
      sprintf("right_join\\s*\\([^)]*\\b%s\\b", t),
      sprintf("inner_join\\s*\\([^)]*\\b%s\\b", t),
      sprintf("filter\\s*\\([^)]*\\b%s\\b", t)
    )
  }
  
  results <- list()
  
  for (t in targets) {
    pats <- c(assign_patterns(t), use_patterns(t))
    hits <- list()
    
    for (f in files) {
      lines <- readLines(f, warn = FALSE)
      matched <- integer()
      which_lines <- integer()
      
      for (p in unique(pats)) {
        w <- grep(p, lines, perl = TRUE)
        if (length(w)) {
          which_lines <- unique(c(which_lines, w))
        }
      }
      
      if (length(which_lines)) {
        hits[[basename(f)]] <- data.frame(
          file = basename(f),
          line = which_lines,
          text = lines[which_lines],
          stringsAsFactors = FALSE
        )
      }
    }
    
    results[[t]] <- if (length(hits)) {
      do.call(rbind, hits[order(names(hits))])
    } else {
      data.frame(file = character(), line = integer(), text = character())
    }
  }
  
  results
}

# Pretty-print the results for quick scanning
print_assignment_hits <- function(hitlist) {
  for (nm in names(hitlist)) {
    cat("\n==== ", nm, " ====\n", sep = "")
    df <- hitlist[[nm]]
    if (!nrow(df)) {
      cat("  (no matches)\n")
    } else {
      df <- df[order(df$file, df$line), ]
      apply(df, 1, function(r) {
        cat(sprintf("  %s:%d  %s\n", r["file"], as.integer(r["line"]), r["text"]))
      })
    }
  }
  invisible(hitlist)
}

