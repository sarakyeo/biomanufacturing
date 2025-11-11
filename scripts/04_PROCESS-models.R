source(here::here("scripts", "process.R"))

# Need to create numeric stimuli variables ----------
clean |> 
  select(defn, issue) |> 
  freq()

clean <- clean |> 
  mutate(ndefn = case_when(
    defn == "simple" ~ 1,
    defn == "complex" ~ 2
  ))

clean <- clean |> 
  mutate(nissue = case_when(
    issue == "fabric" ~ 1,
    issue == "makeup" ~ 2
  ))

clean <- clean |> 
  mutate(nfemale = case_when(
    female == "male" ~ 1,
    female == "female" ~ 2
  ))

clean |> 
  select(defn, ndefn, issue, nissue, nfemale) |> 
  freq()

# DV = funding; Testing simple mediation model with two mediators -----------------
# Y = funding
# M1 = risks
# M2 = benefits
# X = defn
# cov = issue

# m1 <- process(
#   data = clean, 
#   y = "funding",
#   x = "ndefn",
#   m = c("risks", "benefits"),
#   cov = c("nissue", "nfemale"),
#   model = 4,
#   total = 1,
#   contrast = 1,
#   progress = 0,
#   save = 2,
#   boot = 10000,
#   seed = 20251013
# )

# DV = support; Testing simple mediation model with two mediators -----------------
# Y = support
# M1 = risks
# M2 = benefits
# X = defn
# cov = issue

m1 <- process(
  data = clean, 
  y = "support",
  x = "ndefn",
  m = c("risks", "benefits"),
  cov = c("nissue", "nfemale"),
  model = 4,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20251111
)


# DV = support; Testing moderated mediation model with two mediators -----------------
# Y = support
# M1 = risks
# M2 = benefits
# X = issue
# W = female
# cov = defn

m2 <- process(
  data = clean, 
  y = "support",
  x = "ndefn",
  m = c("risks", "benefits"),
  w = "nfemale",
  cov = "nissue",
  model = 8,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20251111
)


mols <- clean |> 
  select(support, female, defn, issue, risks, benefits) |> 
  na.omit() |> 
  lm(formula = support ~ defn + issue + female + risks + benefits + 
    female:defn + female:issue + defn:issue + defn:issue:female)

mols |> summ(center = TRUE)

export_summs(mols,
             model.names = "Support for biomanufacturing research",
             number_format = 2,
             ci_level = .95,
             align = ".",
             statistics = c("N" = "nobs",
                            "Adj. R-squared"= "adj.r.squared",
                            "F" = "statistic",
                            "df" = "df"),
             error_format = "({std.error})",
             error_pos = c("same"),
             coefs = c("(Intercept)" = "(Intercept)",
                       "Definition manipulation (complex)" = "defncomplex",
                       "Issue manipulation (makeup)" = "issuemakeup",
                       "Gender (female)" = "femalefemale",
                       "Perceived risks" = "risks",
                       "Perceived benefits" = "benefits",
                       "Definition × Gender" = "defncomplex:femalefemale",
                       "Issue × Gender" = "issuemakeup:femalefemale",
                       "Definition × Issue" = "defncomplex:issuemakeup",
                       "Definition × Issue × Gender" = "defncomplex:issuemakeup:femalefemale"
                      ))|> 
        set_all_padding(0) |> 
        set_label("tab:OLS-model") |> 
        set_caption("Unstandardized regression coefficients and standard errors (in parentheses) in the OLS model predicting support for biomanufacturing research.") |> 
        set_font_size(11) |> 
        print_latex() |> 
        capture.output(file = here::here("outputs", "tab-motiv-total-eff.tex"))

ixn.plot <- cat_plot(
    model = mols,
    pred = defn,
    modx = female,
    mod2 = issue,
    interval = TRUE,
    int.type = "confidence",
    int.width = .95,
    x.label = "Definition complexity",
    y.label = "Support for biomanufacturing research",
    mod2.labels = c("Fabric", "Makeup"),
    line.thickness = 0.5,
    pred.point.size = 1.5,
    errorbar.width = 0.25,
    colors = c("grey", "black")
  ) +
  theme_apa() + 
  scale_y_continuous(
    limits = c(1,7),
    expand = c(0,0),
    breaks = seq(1,7, 1)
  )

ggsave(plot = ixn.plot,
       here::here("outputs", "int-fig.png"),
       width = 10,
       height = 5)

