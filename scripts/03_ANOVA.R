# ANOVAs on all DVs
# Curiosity ------------------------
clean |>
  select(stim, curious) |>
  na.omit() |>
  anova_test(curious ~ stim, effect.size = "pes", detailed = TRUE)

# Information seeking ------------------------
clean |>
  select(stim, infoseek) |>
  na.omit() |>
  anova_test(infoseek ~ stim, effect.size = "pes", detailed = TRUE)

# Support ------------------------
clean |>
  select(stim, support) |>
  na.omit() |>
  anova_test(support ~ stim, effect.size = "pes", detailed = TRUE)

# Benefits ------------------------
clean |>
  select(stim, benefits) |>
  na.omit() |>
  anova_test(benefits ~ stim, effect.size = "pes", detailed = TRUE)

# Risks ------------------------
clean |>
  select(stim, risks) |>
  na.omit() |>
  anova_test(risks ~ stim, effect.size = "pes", detailed = TRUE)

clean |>
  select(stim, risks) |>
  na.omit() |>
  group_by() |> 
  pairwise_t_test(risks ~ stim, p.adjust.method = "bonferroni")

clean |>
  select(defn, risks) |>
  na.omit() |>
  group_by() |> 
  t_test(risks ~ defn, detailed = TRUE)
