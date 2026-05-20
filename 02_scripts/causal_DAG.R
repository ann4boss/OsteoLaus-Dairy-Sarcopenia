#===============================================================================
# Description: Causal DAG for dairy intake and muscle ageing 

# Author: Anna Boss
# Version: 20Feb2026

# Packages
library(dagitty)
library(ggdag)
library(ggplot2)

# Parameters
# NA
#===============================================================================

dag <- dagitty("
dag {

  DairyIntake

  HandgripStrength
  ALM
  GaitSpeed
  Sarcopenia

  Age
  Education
  PhysicalActivity
  Smoking
  Alcohol
  EnergyIntake
  Macronutrients
  VitaminD
  Calcium
  Comorbidities
  Medications
  BMI
  
  DairyIntake -> HandgripStrength
  DairyIntake -> ALM
  DairyIntake -> GaitSpeed
  DairyIntake -> Sarcopenia
  
}
")

plot(dag, main = "Causal DAG: Dairy Intake and Muscle Ageing", 
     node.size = 0.2, node.color = "lightblue", 
     edge.arrow.size = 0.5, edge.color = "gray")
