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
# cov = issue, female

m1 <- process(
  data = clean, 
  y = "support",
  x = "ndefn",
  m = c("risks", "benefits"),
  cov = c("nfemale", "nissue"),
  model = 4,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20260127
)


# DV = support; Testing moderated mediation model with two mediators -----------------
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
  w = "nissue",
  cov = "nfemale",
  model = 8,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20251111
)


# DV = support; Testing moderated mediation model with two mediators & two moderators -----------------
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
  w = "nissue",
  z = "nfemale",
  model = 12,
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
# X = defn
# W = issue
# cov = female

m1 <- process(
  data = clean, 
  y = "support",
  x = "ndefn",
  m = c("risks", "benefits"),
  w = "nissue",
  cov = "nfemale",
  model = 8,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20260127
)

# 28Jan26: Model 4 including `familiar`
# Y = support
# M1 = risks
# M2 = benefits
# X = defn
# cov = issue, familiar, female

m1 <- process(
  data = clean, 
  y = "support",
  x = "ndefn",
  m = c("risks", "benefits"),
  cov = c("nfemale", "familiar", "nissue"),
  model = 4,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20260128
)
m1 # copied to Google sheets and created table
# https://docs.google.com/spreadsheets/d/19ZOcCt7smTHIOQ7S1QL5ywKeY2mdj6Fh6E51g216kQk/edit?gid=0#gid=0

