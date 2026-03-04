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
