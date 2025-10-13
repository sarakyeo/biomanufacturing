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

clean |> 
  select(defn, ndefn, issue, nissue) |> 
  freq()

# Testing simple mediation model with two mediators -----------------
# Y = funding
# M1 = risks
# M2 = benefits
# X = defn
# cov = issue

m1 <- process(
  data = clean, 
  y = "funding",
  x = "ndefn",
  m = c("risks", "benefits"),
  cov = "nissue",
  model = 4,
  total = 1,
  contrast = 1,
  progress = 0,
  save = 2,
  boot = 10000,
  seed = 20251013
)

