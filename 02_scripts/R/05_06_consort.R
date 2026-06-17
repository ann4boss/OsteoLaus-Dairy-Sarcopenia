# =============================================================================
# R/consort.R
# =============================================================================
# Runs apply_exclusions() for each outcome, assembles a CONSORT counts table,
# and renders a publication-ready SVG flow diagram.
#
# Each outcome is specified as a named list with its own outcome column,
# covariate list, and optionally exposure. The only shared step is QC, which
# is run once using the first outcome's specification.
#
# Usage:
#   source("R/exclusion.R")
#   source("R/consort.R")
#
#   shared_covs <- c("age", "sex", "bmi", "sumtot1")
#
#   consort <- build_consort(
#     data     = my_data,
#     qc_table = my_qc_table,
#     outcomes = list(
#       HGS = list(
#         outcome        = "HGS_MAX",
#         covariant_list = c(shared_covs, "dominant_hand"),
#         exposure       = "dairy_total_gday"
#       ),
#       GS = list(
#         outcome        = "GS_mean",
#         covariant_list = shared_covs,
#         exposure       = "dairy_total_gday"
#       ),
#       ALMI = list(
#         outcome        = "ALMI",
#         covariant_list = c(shared_covs, "height"),
#         exposure       = "dairy_total_gday"
#       ),
#       Sarcopenia = list(
#         outcome        = "ewgsop2_sarcopenia_stage",
#         covariant_list = c(shared_covs, "height", "dominant_hand"),
#         exposure       = "dairy_total_gday"
#       )
#     ),
#     pt_col    = "pt",
#     visit_col = ".visit_osteo",
#     visit_min = 2L,
#     svg_path  = "output/consort_diagram.svg"   # optional; NULL to skip writing
#   )
#
#   consort$counts      # long tibble: one row per stage per outcome
#   consort$summary     # wide tibble: one row per outcome
#   consort$svg         # SVG string for htmltools::HTML() / knitr raw chunk
# =============================================================================

#' Build a CONSORT counts table and SVG diagram across multiple outcomes
#'
#' @param data      Data frame passed to `apply_exclusions()`.
#' @param qc_table  QC table passed to `apply_exclusions()`.
#' @param outcomes  Named list of outcome specifications. Each element is itself
#'   a list with fields:
#'   \describe{
#'     \item{`outcome`}{Character. Column name of the outcome variable.}
#'     \item{`covariant_list`}{Character vector. Covariates required for this
#'       outcome. Rows missing any of these are excluded.}
#'     \item{`exposure`}{Character vector. Exposure column(s). If omitted,
#'       falls back to the top-level `default_exposure` argument.}
#'   }
#' @param default_exposure  Fallback exposure used for any outcome that does not
#'   specify its own `exposure` field. Defaults to `character(0)` (no exposure).
#' @param pt_col    Name of the participant ID column (default `"pt"`).
#' @param visit_col Name of the visit column (default `".visit_osteo"`).
#' @param visit_min Minimum required visits per participant (default `2L`).
#' @param impute    Passed through to `apply_exclusions()` (default `FALSE`).
#' @param svg_path  Optional file path to write the SVG. `NULL` skips writing.
#'
#' @return A list with elements `counts`, `summary`, and `svg`.
build_consort <- function(data,
                          qc_table,
                          outcomes,
                          default_exposure = character(0),
                          pt_col           = "pt",
                          visit_col        = ".visit_osteo",
                          visit_min        = 2L,
                          impute           = FALSE,
                          svg_path         = NULL) {
    
    # ── Input validation ──────────────────────────────────────────────────────
    stopifnot(
        is.data.frame(data),
        is.data.frame(qc_table),
        is.list(outcomes),
        length(outcomes) >= 1,
        !is.null(names(outcomes)),
        all(nzchar(names(outcomes)))
    )
    
    required_fields <- c("outcome", "covariant_list")
    for (nm in names(outcomes)) {
        missing_fields <- setdiff(required_fields, names(outcomes[[nm]]))
        if (length(missing_fields) > 0) {
            stop(
                "Outcome '", nm, "' is missing required field(s): ",
                paste(missing_fields, collapse = ", "),
                ".\nEach outcome must be a list with at least 'outcome' and 'covariant_list'."
            )
        }
    }
    
    # ── Helper: resolve exposure for one outcome spec ─────────────────────────
    resolve_exposure <- function(spec) {
        if (!is.null(spec[["exposure"]]) && length(spec[["exposure"]]) > 0) {
            spec[["exposure"]]
        } else {
            default_exposure
        }
    }
    
    # ── 1. Run apply_exclusions() per outcome ─────────────────────────────────
    counts_list <- lapply(names(outcomes), function(label) {
        spec        <- outcomes[[label]]
        outcome_col <- spec[["outcome"]]
        covs        <- spec[["covariant_list"]]
        exp_cols    <- resolve_exposure(spec)
        
        cli::cli_inform(c(
            "i" = "Running exclusions for outcome: {label} ({outcome_col})",
            " " = "Covariates ({length(covs)}): {paste(covs, collapse = ', ')}",
            " " = "Exposure: {if (length(exp_cols) == 0) '(none)' else paste(exp_cols, collapse = ', ')}"
        ))
        
        res <- apply_exclusions(
            data            = data,
            qc_table        = qc_table,
            covariant_list  = covs,
            exposure        = exp_cols,
            outcome         = outcome_col,
            visit_min       = visit_min,
            pt_col          = pt_col,
            visit_col       = visit_col,
            impute          = impute,
            return_tracking = TRUE
        )
        
        res$consort_counts |>
            dplyr::mutate(outcome = label, .before = 1)
    })
    
    counts <- dplyr::bind_rows(counts_list)
    
    # ── 2. Extract key scalars ────────────────────────────────────────────────
    pull_n <- function(tbl, outcome_label, stage_pattern) {
        val <- tbl |>
            dplyr::filter(
                outcome == outcome_label,
                grepl(stage_pattern, stage, ignore.case = TRUE)
            ) |>
            dplyr::pull(n_participants)
        if (length(val) == 0L) NA_integer_ else val[[1L]]
    }
    
    # QC exclusions are data-driven (not outcome-specific) — use the first outcome
    first_label <- names(outcomes)[[1L]]
    n_initial   <- pull_n(counts, first_label, "^Initial sample$")
    n_after_qc  <- pull_n(counts, first_label, "^Remaining after QC$")
    n_excl_qc   <- pull_n(counts, first_label, "^Excluded for QC$")
    
    outcome_stats <- lapply(names(outcomes), function(label) {
        n_final        <- pull_n(counts, label, "^Final analytic sample$")
        n_excl_missing <- pull_n(counts, label, "^Excluded for missing")
        n_excl_range   <- pull_n(counts, label, "^Excluded for implausible")
        n_excl_visits  <- pull_n(counts, label, "^Excluded for insufficient")
        
        # baseline sarcopenia exclusions live in excluded_participants, not
        # consort_counts yet — if you add that stage to consort_counts in
        # exclusion.R, pull it here with: pull_n(counts, label, "^Excluded.*baseline")
        n_excl_total <- sum(
            c(n_excl_missing, n_excl_range, n_excl_visits),
            na.rm = TRUE
        )
        
        is_sarcopenia <- grepl(
            "sarcopenia|ewgsop|fnih",
            outcomes[[label]][["outcome"]],
            ignore.case = TRUE
        )
        
        list(
            label         = label,
            n_final       = n_final,
            n_excl        = n_excl_total,
            is_sarcopenia = is_sarcopenia,
            covariates    = outcomes[[label]][["covariant_list"]]
        )
    })
    
    # ── 3. Wide summary tibble ────────────────────────────────────────────────
    summary_tbl <- dplyr::tibble(
        outcome        = names(outcomes),
        outcome_col    = vapply(outcomes, `[[`, "", "outcome"),
        n_covariates   = vapply(outcomes, function(s) length(s[["covariant_list"]]), 0L),
        n_initial      = n_initial,
        n_excl_qc      = n_excl_qc,
        n_after_qc     = n_after_qc,
        n_excl_outcome = vapply(outcome_stats, `[[`, 0L, "n_excl"),
        n_final        = vapply(outcome_stats, `[[`, 0L, "n_final")
    )
    
    # ── 4. Generate SVG ───────────────────────────────────────────────────────
    svg_string <- .render_consort_svg(
        n_initial     = n_initial,
        n_excl_qc     = n_excl_qc,
        n_after_qc    = n_after_qc,
        outcome_stats = outcome_stats
    )
    
    if (!is.null(svg_path)) {
        dir.create(dirname(svg_path), recursive = TRUE, showWarnings = FALSE)
        writeLines(svg_string, svg_path)
        cli::cli_inform("SVG written to {svg_path}")
    }
    
    list(
        counts  = counts,
        summary = summary_tbl,
        svg     = svg_string
    )
}


# =============================================================================
# Internal SVG renderer
# =============================================================================
.render_consort_svg <- function(n_initial,
                                n_excl_qc,
                                n_after_qc,
                                outcome_stats) {
    
    n_outcomes <- length(outcome_stats)
    
    # Layout constants
    vb_width    <- 680L
    box_w       <- 200L
    box_h       <- 56L
    box_cx      <- vb_width %/% 2L
    
    col_margin  <- 40L
    usable_w    <- vb_width - 2L * col_margin
    out_box_w   <- min(140L, usable_w %/% n_outcomes - 12L)
    col_spacing <- usable_w %/% n_outcomes
    col_xs      <- vapply(
        seq_len(n_outcomes),
        function(i) col_margin + as.integer(floor((i - 0.5) * col_spacing)),
        0L
    )
    
    y_initial  <- 30L
    y_arrow1   <- y_initial + box_h
    y_after_qc <- y_arrow1 + 50L
    y_split    <- y_after_qc + box_h + 20L
    y_excl_top <- y_split + 10L
    excl_h     <- 106L
    y_out_top  <- y_excl_top + excl_h + 10L
    out_h      <- 72L
    y_note_top <- y_out_top + out_h + 24L
    note_h     <- 56L
    vb_height  <- y_note_top + note_h + 30L
    
    fmt <- function(x) {
        if (is.na(x)) return("n\u2009=\u2009?")
        paste0("n\u2009=\u2009", formatC(as.integer(x), format = "d", big.mark = "\u202f"))
    }
    
    box2 <- function(cx, y, w, h, cls, title, sub, rx = 8L, sw = 0.5) {
        x <- cx - w %/% 2L
        sprintf(
            '<g class="%s"><rect x="%d" y="%d" width="%d" height="%d" rx="%d" stroke-width="%.1f"/>
        <text class="th" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">%s</text>
        <text class="ts" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">%s</text>
      </g>',
            cls, x, y, w, h, rx, sw,
            cx, y + as.integer(h * 0.36), title,
            cx, y + as.integer(h * 0.68), sub
        )
    }
    
    varrow <- function(x, y1, y2) {
        sprintf(
            '<line x1="%d" y1="%d" x2="%d" y2="%d" class="arr" marker-end="url(#arrow)"/>',
            x, y1, x, y2
        )
    }
    
    dashed_arrow <- function(x1, y1, x2, y2) {
        sprintf(
            '<line x1="%d" y1="%d" x2="%d" y2="%d" class="arr" stroke-dasharray="4 3" marker-end="url(#arrow)"/>',
            x1, y1, x2, y2
        )
    }
    
    thin_line <- function(x1, y1, x2, y2) {
        sprintf(
            '<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="var(--color-border-secondary)" stroke-width="0.5"/>',
            x1, y1, x2, y2
        )
    }
    
    # ── Defs ──
    defs <- '<defs><marker id="arrow" viewBox="0 0 10 10" refX="8" refY="5"
      markerWidth="6" markerHeight="6" orient="auto-start-reverse">
      <path d="M2 1L8 5L2 9" fill="none" stroke="context-stroke"
            stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
    </marker></defs>'
    
    # ── Main flow ──
    node_initial <- box2(box_cx, y_initial, box_w, box_h, "c-gray",
                         "Initial sample", fmt(n_initial))
    
    arr1 <- varrow(box_cx, y_initial + box_h, y_after_qc)
    
    qc_cx  <- box_cx + box_w %/% 2L + 110L
    qc_y   <- y_arrow1 + 10L
    qc_w   <- 180L
    qc_h   <- 56L
    qc_box <- box2(qc_cx, qc_y, qc_w, qc_h, "c-coral",
                   "Excluded \u2014 QC failure", fmt(n_excl_qc))
    ldr_qc <- dashed_arrow(
        box_cx, y_arrow1 + 25L,
        qc_cx - qc_w %/% 2L, qc_y + qc_h %/% 2L
    )
    
    node_qc <- box2(box_cx, y_after_qc, box_w, box_h, "c-gray",
                    "After QC", fmt(n_after_qc))
    
    conn  <- thin_line(box_cx, y_after_qc + box_h, box_cx, y_split)
    horiz <- thin_line(col_xs[[1L]], y_split, col_xs[[n_outcomes]], y_split)
    
    # ── Per-outcome columns ──
    outcome_els <- lapply(seq_len(n_outcomes), function(i) {
        s  <- outcome_stats[[i]]
        cx <- col_xs[[i]]
        ex <- cx - out_box_w %/% 2L
        
        excl_lines <- if (isTRUE(s$is_sarcopenia)) {
            c("Missing vars / out-of-range,", "&lt;2 visits, or baseline", "sarcopenia")
        } else {
            c("Missing vars / out-of-range,", "or &lt;2 valid visits")
        }
        n_excl_lines <- length(excl_lines)
        line_step    <- 16L
        total_text_h <- (2L + n_excl_lines) * line_step
        box_h_excl   <- max(excl_h, total_text_h + 16L)
        
        title_y  <- y_excl_top + 14L
        count_y  <- title_y + line_step
        detail_ys <- count_y + line_step + seq_len(n_excl_lines) * line_step - line_step
        
        excl_texts <- paste(
            sprintf('<text class="th" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">Excluded</text>',
                    cx, title_y),
            sprintf('<text class="ts" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">%s</text>',
                    cx, count_y, fmt(s$n_excl)),
            paste(mapply(function(ln, ly)
                sprintf('<text class="ts" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">%s</text>',
                        cx, ly, ln),
                excl_lines, detail_ys), collapse = "\n"),
            sep = "\n"
        )
        
        excl_box <- sprintf(
            '<g class="c-coral"><rect x="%d" y="%d" width="%d" height="%d" rx="8" stroke-width="0.5"/>%s</g>',
            ex, y_excl_top, out_box_w, box_h_excl, excl_texts
        )
        
        arr_down  <- varrow(cx, y_split, y_excl_top)
        arr_out   <- varrow(cx, y_excl_top + box_h_excl, y_out_top)
        
        final_node <- sprintf(
            '<g class="c-teal">
        <rect x="%d" y="%d" width="%d" height="%d" rx="8" stroke-width="0.5"/>
        <text class="th" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">%s</text>
        <text class="ts" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">Analytic sample</text>
        <text class="th" x="%d" y="%d" text-anchor="middle" dominant-baseline="central">%s</text>
      </g>',
            ex, y_out_top, out_box_w, out_h,
            cx, y_out_top + 18L, s$label,
            cx, y_out_top + 38L,
            cx, y_out_top + 56L, fmt(s$n_final)
        )
        
        paste(arr_down, excl_box, arr_out, final_node, sep = "\n")
    })
    
    # ── Notes panel ──
    notes <- sprintf(
        '<g class="c-gray">
      <rect x="40" y="%d" width="600" height="%d" rx="8" stroke-width="0.5"/>
      <text class="th" x="340" y="%d" text-anchor="middle" dominant-baseline="central">QC exclusions (applied once, shared across all outcomes)</text>
      <text class="ts" x="340" y="%d" text-anchor="middle" dominant-baseline="central">Missing pt &#183; missing exam date &#183; unstable sex &#183; missing date of birth &#183; duplicate entries</text>
    </g>',
        y_note_top, note_h,
        y_note_top + 20L,
        y_note_top + 40L
    )
    
    sprintf(
        '<svg width="100%%%%" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg" role="img">
  <title>CONSORT participant flow diagram</title>
  <desc>Participant flow from initial sample through QC and outcome-specific exclusions.</desc>
  %s
  %s %s %s %s %s %s %s
  %s
  %s
</svg>',
        vb_width, vb_height,
        defs,
        node_initial, arr1, qc_box, ldr_qc, node_qc, conn, horiz,
        paste(outcome_els, collapse = "\n"),
        notes
    )
}