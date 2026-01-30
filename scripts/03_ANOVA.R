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
  select(defn, risks) |>
  na.omit() |>
  group_by() |> 
  pairwise_t_test(risks ~ defn, p.adjust.method = "bonferroni")

clean |>
  select(defn, risks) |>
  na.omit() |>
  group_by(defn) |> 
  descr(risks) # simple: M = 4.35, SD = 1.64; complex: M = 4.63, SD = 1.54

mean.risks <- clean |>
        select(defn, risks) |>
        na.omit() |>
        ggplot(aes(x = defn)) +
        stat_summary(aes(y = risks),
                     fun = mean, 
                     geom = "point") +
        stat_summary(aes(y = risks),
                     fun.data = mean_cl_normal,
                     geom = "errorbar",
                     width = 0.2) +
        scale_x_discrete(name = "Definition",
                         labels = c("simple",
                                    "complex")) +
        scale_y_continuous(name = "Perceived risks",
                           expand = c(0,0),
                           limits = c(1, 7),
                           breaks = seq(1, 7, 1)) +
        jtools::theme_apa() +
        theme(
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15)
        ) 

ggsave(mean.risks,
       filename = "means-risks-plot.png",
       path = "outputs",
       device = "png",
       width = 8,
       height = 8,
       units = "in")

clean |>
  select(issue, risks) |>
  na.omit() |>
  anova_test(risks ~ issue, effect.size = "pes", detailed = TRUE) # ns
