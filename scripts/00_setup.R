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

## Race/Ethnicity ---------------
clean |> freq(Q9)
clean <- clean |>
        mutate(white = case_when(Q9 == "White" ~ "white",
                                 TRUE ~ "non-white")) |>
        mutate(white = factor(white,
                              levels = c("non-white", "white")))
clean |> freq(white) # 73.6% white

## Education ---------------
clean |> freq(Q10)
clean <- clean |>
        mutate(educ = factor(Q10,
                             levels = c("Less than high school",
                                        "High school graduate",
                                        "Some college",
                                        "2-year degree (e.g., associate degree)",
                                        "4-year degree (e.g., bachelors degree)",
                                        "Post-graduate degree (e.g., graduate school, JD, MD, PhD)")))
clean |> freq(educ) # Median: 2-year degree

## Income ---------------
clean |> freq(Q11)
clean <- clean |>
        mutate(income = factor(Q11,
                               levels = c("Less than $25,000",
                                          "$25,000 to $49,999",
                                          "$50,000 to $74,999",
                                          "$75,000 to $99,999",
                                          "$100,000 to $124,999",
                                          "$125,000 to $149,999",
                                          "$150,000 to $174,999",
                                          "$175,000 to $199,999",
                                          "$200,000 or more")))
clean |> freq(income) # Median: $50,000 to $74,999
