# =============================================================================
# R/02_02_18_utils_save_log.R
# =============================================================================
# Helpers for capturing cli/message output and writing it to a PDF log file.
#
# Functions:
#   .clean_log_lines()  — strips ANSI codes and wraps long lines
#   save_log_pdf()      — renders a character vector of log lines to a PDF
#   .capture_messages() — evaluates an expression, capturing emitted messages
# =============================================================================

# -----------------------------------------------------------------------------
# .clean_log_lines()
# -----------------------------------------------------------------------------
#' Strip ANSI escape codes and split on embedded newlines.
#'
#' Also wraps any line longer than 110 characters so it fits the PDF page
#' produced by save_log_pdf().
#'
#' @param lines Character vector of raw log lines (may contain ANSI codes
#'   and embedded "\n").
#' @return Character vector of plain-text lines, one per output line.
.clean_log_lines <- function(lines) {
    lines <- cli::ansi_strip(lines)
    lines <- unlist(strsplit(lines, "\n", fixed = TRUE))
    lines[is.na(lines)] <- ""
    unlist(lapply(lines, function(l) {
        if (nchar(l) > 110L) strwrap(l, width = 110L, exdent = 4L) else l
    }))
}

# -----------------------------------------------------------------------------
# save_log_pdf()
# -----------------------------------------------------------------------------
#' Render a character vector of log lines to a PDF file.
#'
#' @param lines Character vector of log lines (ANSI codes stripped automatically).
#' @param path  Output PDF path. Parent directory is created if needed.
#' @return `path` invisibly.
save_log_pdf <- function(lines, path) {
    lines <- .clean_log_lines(lines)

    dir.create(
        dirname(normalizePath(path, mustWork = FALSE)),
        recursive    = TRUE,
        showWarnings = FALSE
    )

    lpp <- 58L  # lines per page

    grDevices::pdf(path, width = 8.5, height = 11, paper = "letter")
    on.exit(grDevices::dev.off(), add = TRUE)

    # Chunk lines into fixed-size pages of `lpp` lines each.
    pages <- if (length(lines) == 0L) {
        list(character(0))
    } else {
        split(lines, ceiling(seq_along(lines) / lpp))
    }

    # One page per chunk: blank plotting canvas, then lines placed top-to-bottom.
    for (pg in pages) {
        graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
        graphics::plot.new()
        graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
        if (length(pg) > 0L) {
            y <- seq(0.98, 0.02, length.out = lpp)[seq_along(pg)]
            for (i in seq_along(pg)) {
                graphics::text(
                    0.01, y[i], pg[i],
                    adj    = c(0, 0.5),
                    cex    = 0.62,
                    family = "mono"
                )
            }
        }
    }

    invisible(path)
}

# -----------------------------------------------------------------------------
# .capture_messages()
# -----------------------------------------------------------------------------
#' Run `expr`, capturing all message-class conditions.
#'
#' Messages are muffled (not printed to the console). Returns a list with
#' `$result` (the value of `expr`) and `$messages` (character vector of
#' captured message strings).
#'
#' @param expr An expression to evaluate.
#' @return List with `$result` (value of `expr`) and `$messages` (character
#'   vector of captured message text).
.capture_messages <- function(expr) {
    msgs   <- character(0)
    result <- withCallingHandlers(
        expr,
        message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        }
    )
    list(result = result, messages = msgs)
}
