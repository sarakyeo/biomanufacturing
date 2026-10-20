
m0r <- clean |> 
  select(support, female, defn, issue, risks, benefits, familiar, MCself) |> 
  na.omit() |> 
  lm(
    formula = risks ~ defn + issue + female + familiar + MCself +
      defn:issue + defn:familiar + issue:familiar +
      defn:issue:familiar
    )
m0r |> summ(center = TRUE)

m0b <- clean |> 
  select(support, female, defn, issue, risks, benefits, familiar, MCself) |> 
  na.omit() |> 
  lm(
    formula = benefits ~ defn + issue + female + familiar + MCself +
    defn:issue + defn:familiar + issue:familiar +
      defn:issue:familiar
  )
m0b |> summ(center = TRUE)

m1 <- clean |> 
  select(support, female, defn, issue, risks, benefits, familiar, MCself) |> 
  na.omit() |> 
  lm(
    formula = support ~ defn + issue + female + familiar + risks + benefits + MCself +
      defn:issue + defn:familiar + issue:familiar +
      defn:issue:familiar
      )
m1 |> summ(center = TRUE)

export_summs(m0r, m0b, m1,
             model.names = c("Risks", "Benefits", "Support"),
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
                       "Perceived familiarity" = "familiar",
                       "Perceived ease" = "MCself",
                       "Perceived risks" = "risks",
                       "Perceived benefits" = "benefits",
                       "Definition × Issue" = "defncomplex:issuemakeup",
                       "Definition × Familiar" = "defncomplex:familiar",
                       "Issue × Familiar" = "issuemakeup:familiar",
                       "Definition × Issue × Familiar" = "defncomplex:issuemakeup:familiar"
                      ))|> 
        set_all_padding(0) |> 
        set_label("tab:OLS-model") |> 
        set_caption("Unstandardized regression coefficients and standard errors (in parentheses) in the path model predicting support for biomanufacturing research.") |> 
        set_font_size(11) |> 
        print_latex() |> 
        capture.output(file = here::here("outputs", "tab-OLS-model.tex"))


# Without interactions -----
m1r <- clean |> 
  select(support, female, defn, issue, risks, benefits, familiar, MCself) |> 
  na.omit() |> 
  lm(
    formula = risks ~ defn + issue + female + familiar + MCself
    )
m1r |> summ(center = TRUE)

m1b <- clean |> 
  select(support, female, defn, issue, risks, benefits, familiar, MCself) |> 
  na.omit() |> 
  lm(
    formula = benefits ~ defn + issue + female + familiar + MCself
  )
m1b |> summ(center = TRUE)

m2 <- clean |> 
  select(support, female, defn, issue, risks, benefits, familiar, MCself) |> 
  na.omit() |> 
  lm(
    formula = support ~ defn + issue + female + familiar + risks + benefits + MCself
      )
m2 |> summ(center = TRUE)

export_summs(m1r, m1b, m2,
             model.names = c("Risks", "Benefits", "Support"),
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
                       "Perceived familiarity" = "familiar",
                       "Perceived ease" = "MCself",
                       "Perceived risks" = "risks",
                       "Perceived benefits" = "benefits"
                      )) |> 
        set_all_padding(0) |> 
        set_label("tab:OLS-model-supp") |> 
        set_caption("Unstandardized regression coefficients and standard errors (in parentheses) in the path model predicting support for biomanufacturing research.") |> 
        set_font_size(11) |> 
        print_latex() |> 
        capture.output(file = here::here("outputs", "tab-OLS-model-supp.tex"))
