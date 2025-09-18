# Load packages ------------------------------

library(tidyverse)
library(weights)
library(rstatix)
library(summarytools)
library(psych)
library(jtools)
library(interactions)
library(stargazer)
library(huxtable)
library(gtsummary)
library(kableExtra)
library(gridExtra)
library(gtsummary)
library(labelled)
library(srvyr)
library(tidyLPA)
library(factoextra)
library(FactoClass)


# Functions ----------------
var_recode <- function(vars, data){
        data |> 
                mutate(across({{vars}},
                              .fns = parse_number,
                              .names = "{.col}c"))
}

var_remove <- function(vars, data){
        data |> 
                mutate(across({{vars}},
                              .fns = ~replace(., 
                                             . > 10,
                                             NA_real_),
                              .names = "{.col}c"))
}


# Load data ---------------------------------------------------------------
biomade <- read_csv(here::here("data", "BioMADE-data-fall2025.csv"))
glimpse(biomade)


# Check gc ----------------------
# Information about gc can be found here: https://survey.qualtrics.com/jfe/form/SV_cMjfp2UFDCxfFGZ
biomade |> freq(gc) # 1,190 gc = 1 (i.e., good completes)

clean <- biomade |> filter(gc == 1)


# Check consent ---------------------
clean |> freq(Q2)


# Check US residency ------------------------
clean |> freq(Q3)


# Check demographics -----------------------
## Region ------------
clean |> freq(Q4)

## Age ------------
clean |> freq(Q6)
clean <- clean |>
        mutate(age = 2025 - Q6)
clean |> freq(age)
clean |> descr(age) # M = 48.4, SD = 17.2

## Gender ------------
clean |> freq(Q7)
clean <- clean |>
        mutate(female = case_when(Q7 == "Female" ~ "female",
                                  Q7 == "Male" ~ "male")) |>
        mutate(female = factor(female,
                                levels = c("male", "female")))
clean |> freq(female) # 51.7% female
