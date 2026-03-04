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
var_recode <- function(vars, data) {
  data |>
    mutate(across({{ vars }}, .fns = parse_number, .names = "{.col}c"))
}

var_remove <- function(vars, data) {
  data |>
    mutate(across(
      {{ vars }},
      .fns = ~ replace(
        .,
        . > 10,
        NA_real_
      ),
      .names = "{.col}c"
    ))
}


# Load data ---------------------------------------------------------------
biomade <- read_csv(here::here("data", "BioMADE-GMO_Final_Data.csv"))
glimpse(biomade)

# Check consent and US residency ----------
biomade |> freq(Q1.2) # 1,011 respondents consented and are over 18 yo
biomade |> freq(Q1.3) # 1,011 respondents live in the US

# Check gc -------------------
biomade |> freq(gc) # 1,011 "good completes"

# Check attention check variable -------------
biomade |> freq(Q21.1_5) # everyone passed this attention check
biomade |> freq(Q2.10_7) # everyone also passed this attention check

# Check quota --------------
## Age -------------
biomade |> freq(Q1.6)
biomade <- biomade |>
  rowwise() |>
  mutate(
    Rage = 2025 - Q1.6
  )
biomade |> freq(Rage)

biomade <- biomade |>
  mutate(
    Ragegp = case_when(
      Rage >= 18 & Rage < 35 ~ "18 - 34",
      Rage >= 35 & Rage < 55 ~ "35 - 54",
      Rage >= 55 ~ "55+"
    )
  )

biomade |> freq(Ragegp)

## Gender -------------
biomade |> freq(Q1.7)

biomade <- biomade |>
  mutate(
    female = case_when(
      Q1.7 == "Female" ~ "female",
      Q1.7 == "Male" ~ "male"
    )
  ) |>
  mutate(
    female = factor(
      female,
      levels = c("male", "female")
    )
  )

biomade |> freq(female) # 50.9 % female

## Race ------
biomade |> freq(Q1.9)

biomade <- biomade |>
  mutate(
    white = case_when(
      Q1.9 == "White" ~ "White",
      Q1.9 != "White" ~ "non-White"
    )
  ) |>
  mutate(
    white = factor(
      white,
      levels = c("non-White", "White")
    )
  )

biomade |> freq(white)