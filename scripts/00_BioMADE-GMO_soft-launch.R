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
                mutate(across({{ vars }},
                        .fns = parse_number,
                        .names = "{.col}c"
                ))
}

var_remove <- function(vars, data) {
        data |>
                mutate(across({{ vars }},
                        .fns = ~ replace(
                                .,
                                . > 10,
                                NA_real_
                        ),
                        .names = "{.col}c"
                ))
}


# Load data ---------------------------------------------------------------
biomade <- read_csv(here::here("data", "BioMADE-GMO_Soft_Launch.csv"))
glimpse(biomade)


# Check gc ----------------------------------------
# Information about gc values can be found here: 
# https://qsales.qualtrics.com/jfe/form/SV_cMjfp2UFDCxfFGZ?Q_JFE=qdg

biomade |> freq(gc) # 50 gc = 1

# Filter to keep only gc = 1
clean <- biomade |> filter(gc == 1)
clean |> freq(gc)

# Check experimental stimuli
clean |> freq(`Q3.3_Page Submit`) # GM / food (n = 7)
clean |> freq(`Q6.3_Page Submit`) # GM / bandages (n = 9)
clean |> freq(`Q9.3_Page Submit`) # GM / footwear (n = 9)
clean |> freq(`Q12.3_Page Submit`) # non-GM / food (n = 8)
clean |> freq(`Q15.3_Page Submit`) # non-GM / bandages (n = 8)
clean |> freq(`Q18.3_Page Submit`) # non- GM / footwear (n = 9)

# Appears to be equal numbers assigned to the six conditions.
