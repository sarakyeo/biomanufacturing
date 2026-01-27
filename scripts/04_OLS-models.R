
m0 <- clean |> 
  select(support, female, defn, issue, risks, benefits) |> 
  na.omit() |> 
  lm(formula = risks ~ defn + issue + female + benefits)
m0 |> summ(center = TRUE)

m1 <- clean |> 
  select(support, female, defn, issue, risks, benefits) |> 
  na.omit() |> 
  lm(formula = support ~ defn + issue + female + risks + benefits +
    defn:issue + defn:female + issue:female)
m1 |> summ(center = TRUE)

m2 <- clean |> 
  select(support, female, defn, issue, risks, benefits) |> 
  na.omit() |> 
  lm(formula = support ~ defn + issue + female + risks + benefits +
    defn:issue + female:defn + female:issue +
    defn:issue:female)
m2 |> summ(center = TRUE)

anova(m0, m1, m2)

export_summs(m0, m1, m2,
             model.names = c("Model 1", "Model 2", "Model 3"),
             number_format = 2,
             ci_level = .95,
             align = ".",
             statistics = c("N" = "nobs",
                            "Adj. R-squared"= "adj.r.squared",
                            "F" = "statistic",
                            "df" = "df",
                            "p" = "p.value"),
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
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))

ixn.plot2 <- cat_plot(
    model = m2,
    pred = defn,
    modx = female,
    interval = TRUE,
    int.type = "confidence",
    int.width = .95,
    x.label = "Definition Type",
    y.label = "Support for biomanufacturing research",
    line.thickness = 0.5,
    pred.point.size = 1.5,
    errorbar.width = 0.25,
    colors = c("grey", "black")
  ) +
  theme_apa() + 
  scale_y_continuous(
    limits = c(1,7),
    expand = c(0,0),
    breaks = seq(1,7,1)
  )

ggsave(plot = ixn.plot2,
       here::here("outputs", "int-fig-two-way.png"),
       width = 6,
       height = 5)

ixn.plot3 <- cat_plot(
    model = m2,
    pred = defn,
    modx = female,
    mod2 = issue,
    interval = TRUE,
    int.type = "confidence",
    int.width = .95,
    x.label = "Definition Type",
    y.label = "Support for biomanufacturing research",
    mod2.labels = c("fabric", "makeup"),
    line.thickness = 0.5,
    pred.point.size = 1.5,
    errorbar.width = 0.25,
    colors = c("grey", "black")
  ) +
  theme_apa() + 
  scale_y_continuous(
    limits = c(1,7),
    expand = c(0,0),
    breaks = seq(1,7,1)
  )

ggsave(plot = ixn.plot3,
       here::here("outputs", "int-fig-three-way.png"),
       width = 10,
       height = 5)
