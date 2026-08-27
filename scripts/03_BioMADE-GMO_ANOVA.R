# Run after 02_BioMADE-GMO_DVs.R

# Support ---------
biomade |> 
  select(
    stim,
    support, benefits, risks,
    paymore
  ) |> 
  na.omit() |> 
  anova_test(support ~ stim, effect.size = "pes") # sig.

biomade |>
  select(
    stim,
    support,
    benefits,
    risks,
    paymore
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = support)) +
  scale_y_continuous(
    name = "Support",
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

# Benefits ---------
biomade |> 
  select(
    stim,
    support, benefits, risks,
    paymore
  ) |> 
  na.omit() |> 
  anova_test(benefits ~ stim, effect.size = "pes") # ns

biomade |>
  select(
    stim,
    support,
    benefits,
    risks,
    paymore
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = benefits)) +
  scale_y_continuous(
    name = "Perceived benefits",
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

# Risks ---------
biomade |> 
  select(
    stim,
    support, benefits, risks,
    paymore
  ) |> 
  na.omit() |> 
  anova_test(risks ~ stim, effect.size = "pes") # ns

# Labeling ---------
biomade |> 
  select(
    stim,
    support, benefits, risks,
    labeling, paymore
  ) |> 
  na.omit() |> 
  anova_test(labeling ~ stim, effect.size = "pes") # sig. (p = .034)

biomade |>
  select(
    stim,
    support,
    benefits,
    risks,
    labeling,
    paymore
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = labeling)) +
  scale_y_continuous(
    name = "Biomanufactured products should be labeled",
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

# Willingness to pay more ---------
biomade |> 
  select(
    stim,
    support, benefits, risks,
    labeling, paymore
  ) |> 
  na.omit() |> 
  anova_test(paymore ~ stim, effect.size = "pes") # sig.

biomade |>
  select(
    stim,
    support,
    benefits,
    risks,
    labeling,
    paymore
  ) |>
  na.omit() |>
  ggplot(aes(x = stim, y = paymore)) +
  scale_y_continuous(
    name = "Willingness to pay more",
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


biomade |> 
  group_by() |> 
  anova_test(paymore ~ GMstim)

# Checking tech optimism/pessimism measures ------------
biomade |> 
  select(Q2.8_1:Q2.8_10) |> 
  freq()

biomade <- var_recode(
  data = biomade,
  vars = c(Q2.8_1:Q2.8_10)
)

biomade |> 
  select(Q2.8_1c:Q2.8_10c) |> 
  freq()

## Factor analysis -------------
biomade |> 
  select(Q2.8_1c:Q2.8_10c) |> 
  KMO() # overall MSA = .88

biomade |> 
  select(Q2.8_1c:Q2.8_10c) |> 
  cortest.bartlett() # sig.

biomade |> 
  select(Q2.8_1c:Q2.8_10c) |> 
  fa.parallel() # 2 factors, 2 components

fa <- biomade |> 
  select(Q2.8_1c:Q2.8_10c) |> 
  fa(.,
      nfactors = 2,
      fm = "pa",
      max.iter = 100,
      rotate = "promax")

fa |> fa.diagram() # optimism on one factor, pessimism on one factor

biomade <- biomade |> 
  rowwise() |> 
  mutate(
    techopt = mean(
      c(Q2.8_1c, Q2.8_2c, Q2.8_3c, Q2.8_4c, Q2.8_5c),
      na.rm = TRUE
    )
  )

biomade <- biomade |> 
  rowwise() |> 
  mutate(
    techpes = mean(
      c(Q2.8_6c, Q2.8_7c, Q2.8_8c, Q2.8_9c, Q2.8_10c),
      na.rm = TRUE
    )
  )

biomade |> freq(techopt)
biomade |> freq(techpes)

biomade |> 
  select(techopt, techpes) |> 
  group_by() |> 
  descr()

#                     techopt   techpes
# ----------------- --------- ---------
#              Mean      5.24      4.59
#           Std.Dev      1.15      1.34
#               Min      1.00      1.00
#                Q1      4.60      3.80
#            Median      5.40      4.80
#                Q3      6.00      5.60
#               Max      7.00      7.00
#               MAD      1.19      1.48
#               IQR      1.40      1.80
#                CV      0.22      0.29
#          Skewness     -0.59     -0.40
#       SE.Skewness      0.08      0.08
#          Kurtosis      0.32     -0.33
#           N.Valid   1011.00   1011.00
#                 N   1011.00   1011.00
#         Pct.Valid    100.00    100.00

biomade |> 
  select(risks, benefits, support, techopt, techpes) |> 
  cor_test(risks, techopt) # Pearson's r = .11, p = .0129

biomade |> 
  select(risks, benefits, support, techopt, techpes) |> 
  cor_test(benefits, techopt) # Pearson's r = .50, p < .001

biomade |> 
  select(risks, benefits, support, techopt, techpes) |> 
  cor_test(support, techopt) # Pearson's r = .51, p < .001

biomade |> 
  select(risks, benefits, support, techopt, techpes) |> 
  cor_test(risks, techpes) # Pearson's r = .22, p < .001

biomade |> 
  select(risks, benefits, support, techopt, techpes) |> 
  cor_test(benefits, techpes) # ns

biomade |> 
  select(risks, benefits, support, techopt, techpes) |> 
  cor_test(support, techpes) # ns
