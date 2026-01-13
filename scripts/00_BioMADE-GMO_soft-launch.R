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

clean <- clean |> 
        mutate(stim = case_when(
                `Q3.3_Page Submit` == `Q3.3_Page Submit` ~ "GM/food",
                `Q6.3_Page Submit` == `Q6.3_Page Submit` ~ "GM/bandages",
                `Q9.3_Page Submit` == `Q9.3_Page Submit` ~ "GM/footwear",
                `Q12.3_Page Submit` == `Q12.3_Page Submit` ~ "non-GM/food",
                `Q15.3_Page Submit` == `Q15.3_Page Submit` ~ "non-GM/bandages",
                `Q18.3_Page Submit` == `Q18.3_Page Submit` ~ "non-GM/footwear",
        )) |> 
        mutate(stim = factor(
                stim,
                levels = c(
                        "GM/food",
                        "GM/bandages",
                        "GM/footwear",
                        "non-GM/food",
                        "non-GM/bandages",
                        "non-GM/footwear"
                )
        ))
clean |> freq(stim)

# Appears to be equal numbers assigned to the six conditions.

# Check demographics ---------------------------
# Consent
clean |> freq(Q1.2) # All 50 R consented to participate

# US resident
clean |> freq(Q1.3) # all 50 R are US residents

# Region
clean |> freq(Q1.4)

# Age
clean |> freq(Q1.6)
clean <- clean |> 
        rowwise() |> 
        mutate(age = 2026 - Q1.6)
clean |> freq(age)

clean |> 
        select(stim, age) |> 
        anova_test(age ~ stim) # ns; no difference in age across stim categories

# Gender
clean |> freq(Q1.7)
chisq_test(table(clean$Q1.7, clean$stim)) # ns

# Education
clean |> freq(Q1.10)
clean <- clean |> 
        mutate(educ = case_when(
                Q1.10 == "Less than high school" ~ "less than HS",
                Q1.10 == "High school graduate" ~ "HS grad",
                Q1.10 == "Some college" ~ "Some college",
                Q1.10 == "2-year degree (e.g., associate degree)" ~ "2-year college",
                Q1.10 == "4-year degree (e.g., bachelors degree)" ~ "4-year college",
                Q1.10 == "Post-graduate degree (e.g., graduate school, JD, MD, PhD)" ~ "post-grad degree"
        )) |> 
        mutate(educ = factor(
                educ,
                levels = c(
                        "less than HS",
                        "HS grad",
                        "Some college",
                        "2-year college",
                        "4-year college",
                        "post-grad degree"
        )
        ))
clean |> freq(educ)
chisq_test(table(clean$educ, clean$stim)) # ns
