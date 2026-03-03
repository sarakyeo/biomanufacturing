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
biomade <- read_csv(here::here("data", "BioMADE-GMO_Final_Data.csv"))
glimpse(biomade)

# Check consent and US residency ----------
biomade |> freq(Q1.2) # 1,011 respondents consented and are over 18 yo
biomade |> freq(Q1.3) # 1,011 respondents live in the US

# Check gc -------------------
biomade |> freq(gc) # 1,011 "good completes"

# Check attention check variable -------------
biomade |>  freq(Q21.1_5) # everyone passed this attention check
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
  mutate(female = factor(
    female,
    levels = c("male", "female")
  ))

biomade |> freq(female) # 50.9 % female

## Race ------
biomade |> freq(Q1.9)

biomade <- biomade |> 
  mutate(
    white = case_when(
      Q1.9 == "White" ~ "White",
      Q1.9 != "White" ~ "non-White"
    )) |> 
  mutate(white = factor(
    white,
    levels = c("non-White", "White")
  ))

biomade |> freq(white)

# Check randomization -----------------
## GM conditions -------
biomade |> 
  freq(`Q3.3_Page Submit`) # n = 158
biomade |> 
  freq(`Q6.3_Page Submit`) # n = 177
biomade |> 
  freq(`Q9.3_Page Submit`) # n = 164

## Non-GM conditions --------
biomade |> 
  freq(`Q12.3_Page Submit`) # n = 168
biomade |> 
  freq(`Q15.3_Page Submit`) # n = 172
biomade |> 
  freq(`Q18.3_Page Submit`) # n = 172

## Create variable, `stim`
biomade <- biomade |> 
  mutate(
    stim = case_when(
      `Q3.3_Page Submit` == `Q3.3_Page Submit` ~ "GMO / Food",
      `Q6.3_Page Submit` == `Q6.3_Page Submit` ~ "GMO / Bandages",
      `Q9.3_Page Submit` == `Q9.3_Page Submit` ~ "GMO / Footwear",
      `Q12.3_Page Submit` == `Q12.3_Page Submit` ~ "Non-GMO / Food",
      `Q15.3_Page Submit` == `Q15.3_Page Submit` ~ "Non-GMO / Bandages",
      `Q18.3_Page Submit` == `Q18.3_Page Submit` ~ "Non-GMO / Footwear",
    ))

biomade |> freq(stim)

## Create var for GMO/non-GMO
biomade <- biomade |> 
  mutate(
    GMstim = case_when(
      stim == "GMO / Food" | stim == "GMO / Bandages" | stim == "GMO / Footwear" ~ "GMO",
      stim == "Non-GMO / Food" | stim == "Non-GMO / Bandages" | stim == "Non-GMO / Footwear" ~ "Non-GMO"
    ))

biomade |> freq(GMstim)

## Create var for products
biomade <- biomade |> 
  mutate(
    prodstim = case_when(
      stim == "GMO / Food" | stim == "Non-GMO / Food" ~ "Food",
      stim == "GMO / Bandages" | stim == "Non-GMO / Bandages" ~ "Bandages",
      stim == "GMO / Footwear" | stim == "Non-GMO / Footwear" ~ "Footwear",
    ))

biomade |> freq(prodstim)

## Checking whether mean age is different between expt groups
biomade |> 
  select(age, stim) |> 
  anova_test(age ~ stim) # ns

# Checking DVs ---------------------
biomade |> 
  select(
    Q3.5_1:Q3.5_6,
    Q6.5_1:Q6.5_6,
    Q9.5_1:Q9.5_6,
    Q12.5_1:Q12.5_6,
    Q15.5_1:Q15.5_6,
    Q18.5_1:Q18.5_6
  ) |> 
    freq()

## worried
biomade <- biomade |> 
  mutate(
    worried = case_when(
      Q3.5_1 == Q3.5_1 ~ Q3.5_1,
      Q6.5_1 == Q6.5_1 ~ Q6.5_1,
      Q9.5_1 == Q9.5_1 ~ Q9.5_1,
      Q12.5_1 == Q12.5_1 ~ Q12.5_1,
      Q15.5_1 == Q15.5_1 ~ Q15.5_1,
      Q18.5_1 == Q18.5_1 ~ Q18.5_1
    )
  )

biomade |> freq(worried)

## concerned
biomade <- biomade |> 
  mutate(
    concerned = case_when(
      Q3.5_2 == Q3.5_2 ~ Q3.5_2,
      Q6.5_2 == Q6.5_2 ~ Q6.5_2,
      Q9.5_2 == Q9.5_2 ~ Q9.5_2,
      Q12.5_2 == Q12.5_2 ~ Q12.5_2,
      Q15.5_2 == Q15.5_2 ~ Q15.5_2,
      Q18.5_2 == Q18.5_2 ~ Q18.5_2
    )
  )

biomade |> freq(concerned)

## hopeful
biomade <- biomade |> 
  mutate(
    hopeful = case_when(
      Q3.5_3 == Q3.5_3 ~ Q3.5_3,
      Q6.5_3 == Q6.5_3 ~ Q6.5_3,
      Q9.5_3 == Q9.5_3 ~ Q9.5_3,
      Q12.5_3 == Q12.5_3 ~ Q12.5_3,
      Q15.5_3 == Q15.5_3 ~ Q15.5_3,
      Q18.5_3 == Q18.5_3 ~ Q18.5_3
    )
  )

biomade |> freq(hopeful)

## optimistic
biomade <- biomade |> 
  mutate(
    optimistic = case_when(
      Q3.5_4 == Q3.5_4 ~ Q3.5_4,
      Q6.5_4 == Q6.5_4 ~ Q6.5_4,
      Q9.5_4 == Q9.5_4 ~ Q9.5_4,
      Q12.5_4 == Q12.5_4 ~ Q12.5_4,
      Q15.5_4 == Q15.5_4 ~ Q15.5_4,
      Q18.5_4 == Q18.5_4 ~ Q18.5_4
    )
  )

biomade |> freq(optimistic)

## interested
biomade <- biomade |> 
  mutate(
    interested = case_when(
      Q3.5_5 == Q3.5_5 ~ Q3.5_5,
      Q6.5_5 == Q6.5_5 ~ Q6.5_5,
      Q9.5_5 == Q9.5_5 ~ Q9.5_5,
      Q12.5_5 == Q12.5_5 ~ Q12.5_5,
      Q15.5_5 == Q15.5_5 ~ Q15.5_5,
      Q18.5_5 == Q18.5_5 ~ Q18.5_5
    )
  )

biomade |> freq(interested)

## curious
biomade <- biomade |> 
  mutate(
    curious = case_when(
      Q3.5_6 == Q3.5_6 ~ Q3.5_6,
      Q6.5_6 == Q6.5_6 ~ Q6.5_6,
      Q9.5_6 == Q9.5_6 ~ Q9.5_6,
      Q12.5_6 == Q12.5_6 ~ Q12.5_6,
      Q15.5_6 == Q15.5_6 ~ Q15.5_6,
      Q18.5_6 == Q18.5_6 ~ Q18.5_6
    )
  )

biomade |> freq(curious)

biomade |> 
  select(worried, concerned, hopeful, optimistic, interested, curious) |> 
  cortest.bartlett() # sig.

biomade |> 
  select(worried, concerned, hopeful, optimistic, interested, curious) |> 
  KMO() # Overall MSA = 0.77

biomade |> 
  select(worried, concerned, hopeful, optimistic, interested, curious) |> 
  fa.parallel()

biomade |> 
  select(worried, concerned, hopeful, optimistic, interested, curious) |> 
  fa(
    .,
    nfactors = 2,
    fm = "pa",
    max.iter = 1000,
    rotate = "promax"
  ) |> 
  fa.diagram() # positive and negative emotions group together

# Preliminary examination of emotions as DVs individually -------------
biomade |> 
  select(stim, GMstim, prodstim, worried, concerned, hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  anova_test(worried ~ stim) # sig.

biomade |> 
  select(stim, GMstim, prodstim, worried, concerned,  hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  ggplot(aes(x = stim, y = worried)) +
  scale_y_continuous(name = "Worried",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1),
                           expand = c(0,0)) +
  scale_x_discrete(name = "Experimental Conditions",labels = c("GMO/Bandages",
                                                            "GMO/Food",
                                                            "GMO/Footwear",
                                                            "Non-GMO/Bandages",
                                                          "Non-GMO/Food",
                                                        "Non-GMO/Footwear")) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Hopeful
biomade |> 
  select(stim, GMstim, prodstim, worried, concerned, hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  anova_test(hopeful ~ stim) # sig.

biomade |> 
  select(stim, GMstim, prodstim, worried, concerned,  hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  ggplot(aes(x = stim, y = hopeful)) +
  scale_y_continuous(name = "Hopeful",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1),
                           expand = c(0,0)) +
  scale_x_discrete(name = "Experimental Conditions",labels = c("GMO/Bandages",
                                                            "GMO/Food",
                                                            "GMO/Footwear",
                                                            "Non-GMO/Bandages",
                                                          "Non-GMO/Food",
                                                        "Non-GMO/Footwear")) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Curious
biomade |> 
  select(stim, GMstim, prodstim, worried, concerned, hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  anova_test(curious ~ stim) # sig.

biomade |> 
  select(stim, GMstim, prodstim, worried, concerned,  hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  ggplot(aes(x = stim, y = curious)) +
  scale_y_continuous(name = "Curious",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1),
                           expand = c(0,0)) +
  scale_x_discrete(name = "Experimental Conditions",labels = c("GMO/Bandages",
                                                            "GMO/Food",
                                                            "GMO/Footwear",
                                                            "Non-GMO/Bandages",
                                                          "Non-GMO/Food",
                                                        "Non-GMO/Footwear")) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Interested
biomade |> 
  select(stim, GMstim, prodstim, worried, concerned, hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  anova_test(interested ~ stim) # sig.

biomade |> 
  select(stim, GMstim, prodstim, worried, concerned,  hopeful, optimistic, interested, curious) |> 
  na.omit() |> 
  ggplot(aes(x = stim, y = interested)) +
  scale_y_continuous(name = "Interested",
                           limits = c(1,7),
                           breaks = seq(1, 7, 1),
                           expand = c(0,0)) +
  scale_x_discrete(name = "Experimental Conditions",labels = c("GMO/Bandages",
                                                            "GMO/Food",
                                                            "GMO/Footwear",
                                                            "Non-GMO/Bandages",
                                                          "Non-GMO/Food",
                                                        "Non-GMO/Footwear")) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)
