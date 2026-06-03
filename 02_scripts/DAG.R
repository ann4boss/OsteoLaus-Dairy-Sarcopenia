library(dagitty)
library(ggdag)
library(ggraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# =============================================================================
# DAG
# =============================================================================

dag <- dagitty("

dag {

Dairy [exposure]

HGS [outcome]
ALMI [outcome]
GS [outcome]
Sarcopenia [outcome]

Age
Education
Marital
Smoking
Alcohol
PA
TotalEnergy
OtherFoodGroups
BMI

Diabetes
CVD
Hypertension
Hypolipid
Cortico
BP
HRT

VitaminD
Calcium

Age -> Dairy
Age -> HGS
Age -> ALMI
Age -> GS
Age -> Sarcopenia
Age -> Diabetes
Age -> BMI
Age -> OtherFoodGroups
Age -> PA
Age -> Smoking
Age -> Alcohol
Age -> Hypertension
Age -> CVD
Age -> Hypolipid
Age -> HRT
Age -> BP

Education -> Dairy
Education -> Smoking
Education -> Alcohol
Education -> PA
Education -> BMI
Education -> Diabetes
Education -> TotalEnergy
Education -> OtherFoodGroups
Education -> VitaminD
Education -> Calcium

Marital -> Dairy
Marital -> PA

Smoking -> HGS
Smoking -> PA
Smoking -> GS
Smoking -> Sarcopenia
Smoking -> CVD

Alcohol -> HGS
Alcohol -> GS
Alcohol -> Sarcopenia
Alcohol -> Diabetes

PA -> HGS
PA -> ALMI
PA -> GS
PA -> Sarcopenia

TotalEnergy -> Dairy
TotalEnergy -> OtherFoodGroups
TotalEnergy -> BMI
TotalEnergy -> ALMI
TotalEnergy -> HGS

OtherFoodGroups -> BMI

BMI -> Diabetes
BMI -> Hypertension
BMI -> GS
BMI -> Sarcopenia
BMI -> CVD

Diabetes -> HGS
Diabetes -> ALMI
Diabetes -> GS
Diabetes -> Sarcopenia
Diabetes -> Dairy
Diabetes -> CVD
Diabetes -> PA

Hypertension -> PA
Hypertension -> CVD

Hypolipid -> HGS
Hypolipid -> GS

Cortico -> Sarcopenia
BP -> ALMI

HRT -> ALMI
HRT -> HGS

VitaminD -> HGS
VitaminD -> GS
VitaminD -> Dairy

Calcium -> ALMI
Calcium -> Dairy

Dairy -> ALMI
Dairy -> HGS
Dairy -> GS

}

")

# =============================================================================
# Coordinates
# =============================================================================

coordinates(dag) <- list(
    
    x = c(
        Age = 1,
        Education = 1,
        Marital = 1,
        
        Smoking = 2,
        Alcohol = 2,
        PA = 2,
        TotalEnergy = 2,
        OtherFoodGroups = 2,
        
        BMI = 2,
        
        Diabetes = 3,
        CVD = 3,
        Hypertension = 3,
        
        Hypolipid = 4,
        Benzos = 4,
        Cortico = 4,
        BP = 4,
        HRT = 4,
        
        VitaminD = 4,
        Calcium = 4,
        
        Dairy = 5,
        
        ALMI = 6,
        HGS = 6,
        GS = 6,
        
        Sarcopenia = 7
    ),
    
    y = c(
        Age = 9,
        Education = 7,
        Marital = 5,
        
        Smoking = 9,
        Alcohol = 8,
        PA = 7,
        TotalEnergy = 6,
        OtherFoodGroups = 5,
        
        BMI = 4,
        
        Diabetes = 8,
        CVD = 7,
        Hypertension = 6,
        
        Hypolipid = 9,
        Benzos = 8,
        Cortico = 7,
        BP = 6,
        HRT = 5,
        
        VitaminD = 4,
        Calcium = 3,
        
        Dairy = 7,
        
        ALMI = 9,
        HGS = 7,
        GS = 5,
        
        Sarcopenia = 7
    )
)

# =============================================================================
# Tidy DAG
# =============================================================================

base_dag <- tidy_dagitty(dag) %>%
    mutate(
        node_type = case_when(
            name == "Dairy" ~ "Exposure",
            name %in% c("ALMI", "HGS", "GS", "Sarcopenia") ~ "Outcome",
            TRUE ~ "Covariate"
        )
    )

# =============================================================================
# Function to extract edges from path strings
# =============================================================================

extract_path_edges <- function(path_string) {
    
    nodes <- str_extract_all(
        path_string,
        "[A-Za-z0-9_]+"
    )[[1]]
    
    if(length(nodes) < 2) return(NULL)
    
    tibble(
        from = nodes[-length(nodes)],
        to   = nodes[-1]
    )
}

# =============================================================================
# Function to build outcome-specific DAG
# =============================================================================

plot_bias_paths <- function(outcome_name){
    
    cat("\n")
    cat("===================================================\n")
    cat("Outcome:", outcome_name, "\n")
    cat("===================================================\n")
    
    print(
        adjustmentSets(
            dag,
            exposure = "Dairy",
            outcome = outcome_name
        )
    )
    
    bp <- tryCatch(
        backdoorPaths(
            dag,
            exposure = "Dairy",
            outcome = outcome_name
        ),
        error = function(e) NULL
    )
    
    if(is.null(bp) || length(bp) == 0){
        
        bias_edges <- tibble(
            from = character(),
            to = character()
        )
        
    } else {
        
        bias_edges <- map_dfr(
            bp,
            extract_path_edges
        ) %>%
            distinct()
    }
    
    plot_dag <- base_dag %>%
        mutate(
            edge_id = paste(name, to, sep = "__")
        )
    
    bias_edges <- bias_edges %>%
        mutate(
            edge_id = paste(from, to, sep = "__")
        )
    
    plot_dag <- plot_dag %>%
        mutate(
            bias_path = edge_id %in% bias_edges$edge_id
        )
    
    ggplot(
        plot_dag,
        aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend
        )
    ) +
        
        geom_dag_edges(
            aes(colour = bias_path),
            edge_width = 0.7,
            start_cap = circle(3.5, "mm"),
            end_cap = circle(3.5, "mm"),
            arrow_directed = arrow(
                angle = 20,
                length = unit(0.07, "inches"),
                type = "closed"
            )
        ) +
        
        scale_colour_manual(
            values = c(
                "FALSE" = "black",
                "TRUE"  = "#d81b60"
            )
        ) +
        
        geom_dag_point(
            aes(fill = node_type),
            shape = 21,
            size = 5,
            stroke = 0.4
        ) +
        
        geom_label(
            aes(label = name),
            nudge_y = -0.50,
            size = 2.4,
            fill = "white",
            label.size = 0
        ) +
        
        scale_fill_manual(
            values = c(
                Exposure = "#ffe6b7",
                Outcome = "#aadce0",
                Covariate = "#ef8a47"
            )
        ) +
        
        labs(
            title = paste(
                "Open Backdoor Paths: Dairy →",
                outcome_name
            )
        ) +
        
        guides(
            fill = "none",
            colour = "none"
        ) +
        
        theme_void(base_size = 10) +
        theme(
            plot.title = element_text(
                hjust = 0.5,
                face = "bold"
            ),
            plot.margin = margin(
                30, 40, 30, 40
            )
        ) +
        
        coord_cartesian(
            clip = "off"
        )
}

# =============================================================================
# Generate plots
# =============================================================================

plot_HGS <- plot_bias_paths("HGS")
plot_ALMI <- plot_bias_paths("ALMI")
plot_GS <- plot_bias_paths("GS")
plot_Sarcopenia <- plot_bias_paths("Sarcopenia")

# =============================================================================
# Display
# =============================================================================

plot_HGS
plot_ALMI
plot_GS
plot_Sarcopenia