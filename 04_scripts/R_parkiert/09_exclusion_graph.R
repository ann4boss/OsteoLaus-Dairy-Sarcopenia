# TODO not working
# =============================================================================
# 08_exclusion_graph.R
# =============================================================================
# Produces CONSORT-style participant flow diagrams from the flow log generated
# by apply_exclusions() (exclusion.R).
#
# The flow log is a single flat tibble with columns:
#   step        integer   0 = start, 1+ = each criterion in sequence
#   criterion   character human-readable label
#   level       character "participant" | "visit" | "partial (visit)"
#   n_excluded  integer   participants or visits removed at this step
#   n_remaining integer   participants or visits remaining after this step
#
# The diagram is split into three vertical sections, each with its own
# centre column:
#
#   Section 1 — Hard exclusions (level == "participant")
#     Participant counts; exclusion boxes to the RIGHT.
#     Starts at "Total enrolled" and ends at "Analysed sample".
#
#   Section 2 — Visit-level exclusions (level == "visit")
#     Visit (row) counts; exclusion boxes to the RIGHT.
#     Starts at the visit count after hard exclusions and ends at
#     "Valid visits for analysis".
#
#   Section 3 — Partial / outcome-specific exclusions (level == "partial (visit)")
#     One horizontal box per outcome, all at the same rank, branching
#     RIGHT from a single "Valid visits" node. These do NOT reduce the
#     centre count — they each report the number of visits excluded from
#     that specific analysis only.
#
# Functions
# ---------
#   make_consort_graph()     Single combined diagram (all three sections).
#   export_consort_graph()   Write grViz object to PNG or SVG file.
#
# =============================================================================


# =============================================================================
# Main diagram function
# =============================================================================

#' Build a three-section CONSORT flow diagram from an apply_exclusions() log.
#'
#' Section 1: hard participant-level exclusions (counts in participants).
#' Section 2: visit-level exclusions (counts in visits / rows).
#' Section 3: partial outcome-specific exclusions, fanning right from the
#'             final valid-visits node.
#'
#' Steps with n_excluded == 0 are silently skipped so the diagram stays
#' uncluttered. Black-and-white styling throughout.
#'
#' @param flow_log  Tibble returned by get_flow_log(annotated_df), i.e.
#'   attr(annotated_df, "flow_log") where annotated_df is the output of
#'   apply_exclusions().
#' @param title     Character. Title printed above the diagram.
#' @param wrap_width Integer. Maximum characters per line inside node labels
#'   before wrapping. Default 38.
#' @return A DiagrammeR grViz object.
make_consort_graph_simple <- function(flow_log, title = "CONSORT Flow") {
    
    if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
        stop("Install DiagrammeR")
    }
    
    # ── Basic validation ───────────────────────────────────────────────
    req <- c("step", "criterion", "level", "n_excluded", "n_remaining")
    if (!all(req %in% names(flow_log))) {
        stop("flow_log missing required columns")
    }
    
    # Split sections
    hard   <- flow_log[flow_log$level == "participant", , drop = FALSE]
    visit  <- flow_log[flow_log$level == "visit", , drop = FALSE]
    part   <- flow_log[flow_log$level == "partial (visit)", , drop = FALSE]
    
    # ── Helpers ───────────────────────────────────────────────────────
    wrap <- function(x, width = 30) {
        paste(strwrap(x, width), collapse = "\\n")
    }
    
    node <- function(id, label) {
        paste0(id, ' [label="', label, '", shape=box]')
    }
    
    edge <- function(a, b, dashed = FALSE) {
        if (dashed) {
            paste0(a, " -> ", b, " [style=dashed]")
        } else {
            paste0(a, " -> ", b)
        }
    }
    
    nodes <- c()
    edges <- c()
    
    # ── SECTION 1: Participants ───────────────────────────────────────
    
    if (nrow(hard) > 0) {
        
        nodes <- c(nodes, node("n0", paste0("Start\nn=", hard$n_remaining[1])))
        
        prev <- "n0"
        step_id <- 1
        
        for (i in seq_len(nrow(hard))) {
            
            if (hard$n_excluded[i] > 0) {
                
                # main chain
                nid <- paste0("n", step_id)
                nodes <- c(nodes, node(nid, paste0("n=", hard$n_remaining[i])))
                edges <- c(edges, edge(prev, nid))
                
                # exclusion box
                eid <- paste0("e", step_id)
                label <- paste0("Excluded n=", hard$n_excluded[i], "\n",
                                wrap(hard$criterion[i]))
                nodes <- c(nodes, node(eid, label))
                edges <- c(edges, edge(prev, eid, TRUE))
                
                prev <- nid
                step_id <- step_id + 1
            }
        }
        
        edges <- c(edges, edge(prev, "v_start"))
        
    } else {
        nodes <- c(nodes, node("n0", "No participant data"))
        edges <- c(edges, edge("n0", "v_start"))
    }
    
    # ── SECTION 2: Visits ─────────────────────────────────────────────
    
    if (nrow(visit) > 0) {
        
        nodes <- c(nodes, node("v_start",
                               paste0("Visits\nn=", visit$n_remaining[1])))
        
        prev <- "v_start"
        step_id <- 1
        
        for (i in seq_len(nrow(visit))) {
            
            if (visit$n_excluded[i] > 0) {
                
                vid <- paste0("v", step_id)
                nodes <- c(nodes, node(vid,
                                       paste0("n=", visit$n_remaining[i], " visits")))
                edges <- c(edges, edge(prev, vid))
                
                eid <- paste0("ve", step_id)
                label <- paste0("Excluded n=", visit$n_excluded[i], "\n",
                                wrap(visit$criterion[i]))
                nodes <- c(nodes, node(eid, label))
                edges <- c(edges, edge(prev, eid, TRUE))
                
                prev <- vid
                step_id <- step_id + 1
            }
        }
        
        nodes <- c(nodes, node("v_final",
                               paste0("Valid visits\nn=", tail(visit$n_remaining,1))))
        edges <- c(edges, edge(prev, "v_final"))
        
    } else {
        nodes <- c(nodes, node("v_start", "No visit data"))
        nodes <- c(nodes, node("v_final", "End"))
        edges <- c(edges, edge("v_start", "v_final"))
    }
    
    # ── SECTION 3: Partial exclusions ──────────────────────────────────
    
    if (nrow(part) > 0) {
        
        edges <- c(edges, edge("v_final", "p_anchor"))
        nodes <- c(nodes, node("p_anchor", "Outcome exclusions"))
        
        for (i in seq_len(nrow(part))) {
            if (part$n_excluded[i] > 0) {
                
                pid <- paste0("p", i)
                label <- paste0(
                    wrap(part$criterion[i]), "\n",
                    "excl=", part$n_excluded[i],
                    " rem=", part$n_remaining[i]
                )
                
                nodes <- c(nodes, node(pid, label))
                edges <- c(edges, edge("p_anchor", pid, TRUE))
            }
        }
    }
    
    # ── Assemble graph ─────────────────────────────────────────────────
    
    dot <- paste(
        c(
            "digraph consort {",
            paste0('label="', title, '"; labelloc="t";'),
            "rankdir=TB;",
            "",
            paste(nodes, collapse = "\n"),
            "",
            paste(edges, collapse = "\n"),
            "}"
        ),
        collapse = "\n"
    )
    
    DiagrammeR::grViz(dot)
}



# =============================================================================
# Export helper
# =============================================================================

#' Export the CONSORT grViz diagram to a PNG or SVG file.
#'
#' Requires the DiagrammeRsvg and rsvg packages for PNG output.
#' SVG output only requires DiagrammeRsvg.
#'
#' @param graph  grViz object returned by make_consort_graph().
#' @param path   Output file path. Extension must be .png or .svg.
#' @param width  Integer. Pixel width for PNG output. Default 1000L.
#' @param height Integer. Pixel height for PNG output. Default 1400L.
#' @return Invisibly returns path. Suitable as a targets file target.
export_consort_graph <- function(graph,
                                 path,
                                 width  = 1000L,
                                 height = 1400L) {
    
    if (!requireNamespace("DiagrammeRsvg", quietly = TRUE))
        cli::cli_abort(
            "Package {.pkg DiagrammeRsvg} required. \\
             Install with: install.packages('DiagrammeRsvg')"
        )
    
    svg_str <- DiagrammeRsvg::export_svg(graph)
    svg_raw <- charToRaw(svg_str)
    
    ext <- tolower(tools::file_ext(path))
    
    if (ext == "svg") {
        writeBin(svg_raw, path)
        
    } else if (ext == "png") {
        if (!requireNamespace("rsvg", quietly = TRUE))
            cli::cli_abort(
                "Package {.pkg rsvg} required for PNG export. \\
                 Install with: install.packages('rsvg')"
            )
        rsvg::rsvg_png(svg_raw, path, width = width, height = height)
        
    } else {
        cli::cli_abort(
            "Unsupported file extension '{ext}' in {.path {path}}. \\
             Use .png or .svg."
        )
    }
    
    cli::cli_inform("CONSORT diagram written to {.path {path}}")
    invisible(path)
}


# =============================================================================
# PRIVATE HELPERS
# =============================================================================

.check_diagrammer <- function() {
    if (!requireNamespace("DiagrammeR", quietly = TRUE))
        cli::cli_abort(
            "Package {.pkg DiagrammeR} required. \\
             Install with: install.packages('DiagrammeR')"
        )
}