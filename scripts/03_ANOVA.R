# ANOVAs on all DVs
# Curiosity ------------------------
clean |>
  select(stim, curious) |>
  na.omit() |>
  anova_test(curious ~ stim, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(defn, curious) |>
  na.omit() |>
  anova_test(curious ~ defn, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(issue, curious) |>
  na.omit() |>
  anova_test(curious ~ issue, effect.size = "pes", detailed = TRUE) # ns

# Information seeking ------------------------
clean |>
  select(stim, infoseek) |>
  na.omit() |>
  anova_test(infoseek ~ stim, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(defn, infoseek) |>
  na.omit() |>
  anova_test(infoseek ~ defn, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(issue, infoseek) |>
  na.omit() |>
  anova_test(infoseek ~ issue, effect.size = "pes", detailed = TRUE) # ns

# Support ------------------------
clean |>
  select(stim, support) |>
  na.omit() |>
  anova_test(support ~ stim, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(defn, support) |>
  na.omit() |>
  anova_test(support ~ defn, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(issue, support) |>
  na.omit() |>
  anova_test(support ~ issue, effect.size = "pes", detailed = TRUE) # ns

# Benefits ------------------------
clean |>
  select(stim, benefits) |>
  na.omit() |>
  anova_test(benefits ~ stim, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(defn, benefits) |>
  na.omit() |>
  anova_test(benefits ~ defn, effect.size = "pes", detailed = TRUE) # ns

clean |>
  select(issue, benefits) |>
  na.omit() |>
  anova_test(benefits ~ issue, effect.size = "pes", detailed = TRUE) # ns

# Risks ------------------------
clean |>
  select(stim, risks) |>
  na.omit() |>
  anova_test(risks ~ stim, effect.size = "pes", detailed = TRUE) # sig.

clean |>
  select(stim, risks) |>
  na.omit() |>
  group_by() |> 
  pairwise_t_test(risks ~ stim, p.adjust.method = "bonferroni")

clean |>
  select(defn, risks) |>
  na.omit() |>
  anova_test(risks ~ defn, effect.size = "pes", detailed = TRUE) # sig.

clean |>
  select(defn, risks) |>
  na.omit() |>
  group_by() |> 
  pairwise_t_test(risks ~ defn, p.adjust.method = "bonferroni")

clean |>
  select(issue, risks) |>
  na.omit() |>
  anova_test(risks ~ issue, effect.size = "pes", detailed = TRUE) # ns
