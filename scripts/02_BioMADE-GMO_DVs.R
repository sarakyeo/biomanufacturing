# Run after 01_BioMADE-GMO_exp-cond.R

# Emotional reactions to stimulus ---------------------
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

## worried -------
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
biomade |> 
  group_by() |> 
  descr(worried) # M = 3.61, SD = 1.86

## concerned -------
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

## hopeful -----------
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

## optimistic ----------
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

## interested -----------
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

## curious ------------
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

## Preliminary examination of emotions as DVs individually -------------
biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  anova_test(worried ~ stim) # sig.

biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = worried)) +
  scale_y_continuous(
    name = "Worried",
    limits = c(1, 7),
    breaks = seq(1, 7, 1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    name = "Experimental Conditions",
    labels = c(
      "GMO/Bandages",
      "GMO/Food",
      "GMO/Footwear",
      "Non-GMO/Bandages",
      "Non-GMO/Food",
      "Non-GMO/Footwear"
    )
  ) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Hopeful
biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  anova_test(hopeful ~ stim) # sig.

biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = hopeful)) +
  scale_y_continuous(
    name = "Hopeful",
    limits = c(1, 7),
    breaks = seq(1, 7, 1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    name = "Experimental Conditions",
    labels = c(
      "GMO/Bandages",
      "GMO/Food",
      "GMO/Footwear",
      "Non-GMO/Bandages",
      "Non-GMO/Food",
      "Non-GMO/Footwear"
    )
  ) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Curious
biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  anova_test(curious ~ stim) # sig.

biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = curious)) +
  scale_y_continuous(
    name = "Curious",
    limits = c(1, 7),
    breaks = seq(1, 7, 1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    name = "Experimental Conditions",
    labels = c(
      "GMO/Bandages",
      "GMO/Food",
      "GMO/Footwear",
      "Non-GMO/Bandages",
      "Non-GMO/Food",
      "Non-GMO/Footwear"
    )
  ) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Interested
biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  anova_test(interested ~ stim) # sig.

biomade |>
  select(
    stim,
    GMstim,
    prodstim,
    worried,
    concerned,
    hopeful,
    optimistic,
    interested,
    curious
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = interested)) +
  scale_y_continuous(
    name = "Interested",
    limits = c(1, 7),
    breaks = seq(1, 7, 1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    name = "Experimental Conditions",
    labels = c(
      "GMO/Bandages",
      "GMO/Food",
      "GMO/Footwear",
      "Non-GMO/Bandages",
      "Non-GMO/Food",
      "Non-GMO/Footwear"
    )
  ) +
  theme_bw() +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

# Support -----------------------
biomade |> 
  select(
    Q4.4_1:Q4.4_3,
    Q7.4_1:Q7.4_3,
    Q10.4_1:Q10.4_3,
    Q13.4_1:Q13.4_3,
    Q16.4_1:Q16.4_3,
    Q19.4_1:Q19.4_3
  ) |> 
  freq()
 
biomade <- var_recode(
  data = biomade,
  vars = c(
    Q4.4_1:Q4.4_3,
    Q7.4_1:Q7.4_3,
    Q10.4_1:Q10.4_3,
    Q13.4_1:Q13.4_3,
    Q16.4_1:Q16.4_3,
    Q19.4_1:Q19.4_3
  )
)

biomade |> 
  select(Q4.4_2, Q4.4_2c) |> 
  freq()

## Support for research ------
biomade <- biomade |>
  mutate(
    support1 = case_when(
      Q4.4_1c == Q4.4_1c ~ Q4.4_1c,
      Q7.4_1c == Q7.4_1c ~ Q7.4_1c,
      Q10.4_1c == Q10.4_1c ~ Q10.4_1c,
      Q13.4_1c == Q13.4_1c ~ Q13.4_1c,
      Q16.4_1c == Q16.4_1c ~ Q16.4_1c,
      Q19.4_1c == Q19.4_1c ~ Q19.4_1c
    )
  )

biomade |> freq(support1)

## Support for federal funding of research ------
biomade <- biomade |>
  mutate(
    support2 = case_when(
      Q4.4_2c == Q4.4_2c ~ Q4.4_2c,
      Q7.4_2c == Q7.4_2c ~ Q7.4_2c,
      Q10.4_2c == Q10.4_2c ~ Q10.4_2c,
      Q13.4_2c == Q13.4_2c ~ Q13.4_2c,
      Q16.4_2c == Q16.4_2c ~ Q16.4_2c,
      Q19.4_2c == Q19.4_2c ~ Q19.4_2c
    )
  )

biomade |> freq(support2)

## Support for state funding of research ------
biomade <- biomade |>
  mutate(
    support3 = case_when(
      Q4.4_3c == Q4.4_3c ~ Q4.4_3c,
      Q7.4_3c == Q7.4_3c ~ Q7.4_3c,
      Q10.4_3c == Q10.4_3c ~ Q10.4_3c,
      Q13.4_3c == Q13.4_3c ~ Q13.4_3c,
      Q16.4_3c == Q16.4_3c ~ Q16.4_3c,
      Q19.4_3c == Q19.4_3c ~ Q19.4_3c
    )
  )

biomade |> freq(support3)

## Factor analysis for support variables ---------
biomade |> 
  select(support1:support3) |> 
  na.omit() |> 
  cortest.bartlett() # sig.

biomade |> 
  select(support1:support3) |> 
  na.omit() |> 
  KMO() # Overall MSA = 0.75

biomade |> 
  select(support1:support3) |> 
  na.omit() |> 
  fa.parallel() # 1 factor, 1 component

biomade |> 
  select(support1, support2, support3) |> 
  group_by() |> 
  na.omit() |> 
  psych::alpha() # Cronbach's alpha = 0.9

## Create index of overall support --------
biomade <- biomade |> 
  rowwise() |> 
  mutate(
    support = mean(
      c(support1, support2, support3),
      na.rm = TRUE
    )
  )

biomade |> freq(support)
biomade |>
  group_by() |> 
  descr(support) # M = 5.04, SD = 1.38

# Perceived risks and benefits --------
biomade |> 
  select(
    Q4.4_6:Q4.4_7,
    Q7.4_6:Q7.4_7,
    Q10.4_6:Q10.4_7,
    Q13.4_6:Q13.4_7,
    Q16.4_6:Q16.4_7,
    Q19.4_6:Q19.4_7
  ) |> 
  freq()
 
biomade <- var_recode(
  data = biomade,
  vars = c(
    Q4.4_6:Q4.4_7,
    Q7.4_6:Q7.4_7,
    Q10.4_6:Q10.4_7,
    Q13.4_6:Q13.4_7,
    Q16.4_6:Q16.4_7,
    Q19.4_6:Q19.4_7
  )
)

biomade |> 
  select(Q4.4_6, Q4.4_6c) |> 
  freq()

## Benefits ------
biomade <- biomade |>
  mutate(
    benefits = case_when(
      Q4.4_6c == Q4.4_6c ~ Q4.4_6c,
      Q7.4_6c == Q7.4_6c ~ Q7.4_6c,
      Q10.4_6c == Q10.4_6c ~ Q10.4_6c,
      Q13.4_6c == Q13.4_6c ~ Q13.4_6c,
      Q16.4_6c == Q16.4_6c ~ Q16.4_6c,
      Q19.4_6c == Q19.4_6c ~ Q19.4_6c
    )
  )

biomade |> freq(benefits)
biomade |> 
  group_by() |> 
  descr(benefits) # M = 5.19, SD = 1.34

## Risks ----------
biomade <- biomade |>
  mutate(
    risks = case_when(
      Q4.4_7c == Q4.4_7c ~ Q4.4_7c,
      Q7.4_7c == Q7.4_7c ~ Q7.4_7c,
      Q10.4_7c == Q10.4_7c ~ Q10.4_7c,
      Q13.4_7c == Q13.4_7c ~ Q13.4_7c,
      Q16.4_7c == Q16.4_7c ~ Q16.4_7c,
      Q19.4_7c == Q19.4_7c ~ Q19.4_7c
    )
  )

biomade |> freq(risks)
biomade |> 
  group_by() |> 
  descr(risks) # M = 4.73, SD = 1.42

# Labeling of biomanufactured products ----------
biomade |> 
  select(
    Q4.4_11,
    Q7.4_11,
    Q10.4_11,
    Q13.4_11,
    Q16.4_11,
    Q19.4_11
  ) |> 
  freq()

biomade <- var_recode(
  data = biomade,
  vars = c(
    Q4.4_11,
    Q7.4_11,
    Q10.4_11,
    Q13.4_11,
    Q16.4_11,
    Q19.4_11
  )
)

biomade |> 
  select(Q7.4_11, Q7.4_11c) |> 
  freq()

biomade <- biomade |>
  mutate(
    labeling = case_when(
      Q4.4_11c == Q4.4_11c ~ Q4.4_11c,
      Q7.4_11c == Q7.4_11c ~ Q7.4_11c,
      Q10.4_11c == Q10.4_11c ~ Q10.4_11c,
      Q13.4_11c == Q13.4_11c ~ Q13.4_11c,
      Q16.4_11c == Q16.4_11c ~ Q16.4_11c,
      Q19.4_11c == Q19.4_11c ~ Q19.4_11c
    )
  )

biomade |> freq(labeling)
biomade |> 
  group_by() |> 
  descr(labeling) # M = 5.80, SD = 1.26

# Willingness to pay more for biomanufactured products -------
biomade |> 
  select(
    Q4.5,
    Q7.5,
    Q10.5,
    Q13.5, 
    Q16.5,
    Q19.5
  ) |> 
  freq()

biomade <- biomade |> 
  mutate(
    across(
      .cols = c(Q4.5, Q7.5, Q10.5, Q13.5, Q16.5, Q19.5),
      .fns = ~case_when(
        . == "I would not be willing to purchase a biomanufactured product." ~ 1,
        . == "I would be willing to purchase a biomanufactured product, but not pay more for it." ~ 2,
        . == "I would be willing to pay 1-5% more" ~ 3,
        . == "I would be willing to pay 6-10% more" ~ 4,
        . == "I would be willing to pay 11-15% more" ~ 5,
        . == "I would be willing to pay 16-20% more" ~ 6,
        . == "I would be willing to pay greater than 20% more" ~ 7
      ),
      .names = "{.col}c"
    )
    )

biomade |> 
  select(Q19.5, Q19.5c) |> 
  freq()

biomade <- biomade |> 
  mutate(
    paymore = case_when(
      Q4.5c == Q4.5c ~ Q4.5c,
      Q7.5c == Q7.5c ~ Q7.5c,
      Q10.5c == Q10.5c ~ Q10.5c,
      Q13.5c == Q13.5c ~ Q13.5c,
      Q16.5c == Q16.5c ~ Q16.5c,
      Q19.5c == Q19.5c ~ Q19.5c
    )
  )

biomade |> freq(paymore) # NA = Don't know; skipped (573 NA = 56.7%)
biomade |> 
  group_by() |> 
  descr(paymore) # M = 2.96, SD = 1.68
