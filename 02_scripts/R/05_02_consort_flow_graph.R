create_consort_flowchart <- function(output_path = "03_outputs/descriptives/consort_flow.png",
                                     width = 3600, 
                                     height = 2400,
                                     print_plot = TRUE) {
  
  
  # Create the consort diagram
  consort_graph <- DiagrammeR::grViz("
  digraph consort_diagram {
    # Layout setup
    graph [layout = dot, splines = ortho, nodesep = 0.3, ranksep = 0.4]
    
    # Standard node styling for core stages
    node [shape = box, fontname = Helvetica, fontsize = 10, style = filled, fillcolor = '#F8F9FA', color = '#495057', width = 2.4]
    
    # Styling for invisible layout anchors
    node [label='', shape=point, width=0, height=0] point1;
  
    # ----------------- DEFINE NODES -----------------
    
    # Main stream
    n1 [label = 'Enrolled in OsteoLaus\\n(n = 1475)', shape = box]
    n2 [label = 'Included in analysis\\n(n = 1158)', shape = box, fillcolor = '#E9ECEF']
  
    # Top Main Exclusion Box
    e1 [label = 'Excluded (n = 317):\\l• Missing match between OsteoLaus & CoLaus (n=1)\\l• Missing examination data (n=49)\\l• Missing exposure values (n=42)\\l• Missing shared covariates values (n=4)\\l• Calorie intake out of range (<500 or >3500 kcal) (n=7)\\l• Fewer than 2 visits (n=214)\\l', shape = box, fontname = Helvetica, fontsize = 9, fillcolor = '#F8F9FA', align = left, width = 3.0]
    
    # Four analysis trackers (Headers)
    hgs_h [label = 'HGS Analysis', shape = box]
    alm_h [label = 'ALMI Analysis', shape = box]
    gait_h [label = 'Gait Speed Analysis', shape = box]
    sarc_h [label = 'Sarcopenia Analysis', shape = box]
  
    # Four exclusion notes
    hgs_e [label = 'Excluded:\\lFewer than 2 visits with\\l  complete data (n = 23)\\l', shape = box, fontname = Helvetica, fontsize = 9, fillcolor = '#F8F9FA', align = left, width = 1.5]
    alm_e [label = 'Excluded:\\lFewer than 2 visits with\\l  complete data (n = 92)\\l', shape = box, fontname = Helvetica, fontsize = 9, fillcolor = '#F8F9FA', align = left, width = 1.5]
    gait_e [label = 'Excluded:\\lFewer than 2 visits with\\l  complete data (n = 358)\\l', shape = box, fontname = Helvetica, fontsize = 9, fillcolor = '#F8F9FA', align = left, width = 1.5]
    sarc_e [label = 'Excluded (n = 35):\\l• Sarcopenic at baseline\\l  (n = 12)\\l• Fewer than 2 visits with\\l  complete data (n = 23)\\l', shape = box, fontname = Helvetica, fontsize = 9, fillcolor = '#F8F9FA', align = left, width = 1.5]
  
    # Final analytical cohorts
    hgs_f [label = 'Participants included\\nn = 1135', shape = box, fontname = Helvetica, fontsize = 10, style = 'filled,bold', fillcolor = '#72BCD5', color = '#5990A1', width = 1.8]
    alm_f [label = 'Participants included\\nn = 1066', shape = box, fontname = Helvetica, fontsize = 10, style = 'filled,bold', fillcolor = '#72BCD5', color = '#5990A1', width = 1.8]
    gait_f [label = 'Participants included\\nn = 800', shape = box, fontname = Helvetica, fontsize = 10, style = 'filled,bold', fillcolor = '#72BCD5', color = '#5990A1', width = 1.8]
    sarc_f [label = 'Participants included\\nn = 1123', shape = box, fontname = Helvetica, fontsize = 10, style = 'filled,bold', fillcolor = '#72BCD5', color = '#5990A1', width = 1.8]
  
    # ----------------- DEFINE EDGES (CONNECTIONS) -----------------
    
    # Top main path setup
    n1 -> point1 [dir = none]
    point1 -> n2
    point1 -> e1 [arrowhead = normal]
    { rank = same; point1; e1 }
  
    # 4-way parallel split down from main cohort
    n2 -> hgs_h
    n2 -> alm_h
    n2 -> gait_h
    n2 -> sarc_h
  
    # --- HGS Path ---
    hgs_h -> p_hgs [dir = none]
    p_hgs -> hgs_f
    p_hgs -> hgs_e [arrowhead = normal]
    { rank = same; p_hgs; hgs_e }
  
    # --- ALMI Path ---
    alm_h -> p_alm [dir = none]
    p_alm -> alm_f
    p_alm -> alm_e [arrowhead = normal]
    { rank = same; p_alm; alm_e }
  
    # --- Gait Speed Path ---
    gait_h -> p_gait [dir = none]
    p_gait -> gait_f
    p_gait -> gait_e [arrowhead = normal]
    { rank = same; p_gait; gait_e }
  
    # --- Sarcopenia Path ---
    sarc_h -> p_sarc [dir = none]
    p_sarc -> sarc_f
    p_sarc -> sarc_e [arrowhead = normal]
    { rank = same; p_sarc; sarc_e }
    
    # Force all final outcomes to sit completely level on the bottom plane
    { rank = same; hgs_f; alm_f; gait_f; sarc_f }
  }
  ")
  
  # Print the plot if requested
  if (print_plot) {
    print(consort_graph)
  }
  
  # Export to publication-ready file
  DiagrammeRsvg::export_svg(consort_graph) |>
    charToRaw() |>
    rsvg::rsvg_png(output_path, width = width, height = height)
  
  # Return the graph object invisibly for further use
  invisible(consort_graph)
}