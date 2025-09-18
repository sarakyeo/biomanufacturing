# Creating experimental conditions ------------
clean |> freq(`Q23_Page Submit`) # simple / fabric; n = 302
clean |> freq(`Q38_Page Submit`) # complex / fabric; n = 291
clean |> freq(`Q53_Page Submit`) # simple / makeup; n = 295
clean |> freq(`Q68_Page Submit`) # complex / makeup; n = 302

clean <- clean |> 
        mutate(stim = case_when(
                `Q23_Page Submit` == `Q23_Page Submit` ~ "simple/fabric",
                `Q38_Page Submit` == `Q38_Page Submit` ~ "complex/fabric",
                `Q53_Page Submit` == `Q53_Page Submit` ~ "simple/makeup",
                `Q68_Page Submit` == `Q68_Page Submit` ~ "complex/makeup")) |>
        mutate(stim = factor(stim,
                                 levels = c("simple/fabric",
                                            "complex/fabric",
                                            "simple/makeup",
                                            "complex/makeup")))

clean |> freq(stim)

## Simple vs. complex definition ------------------
clean <- clean |>
        mutate(defn = case_when(
                stim %in% c("simple/fabric", "simple/makeup") ~ "simple",
                stim %in% c("complex/fabric", "complex/makeup") ~ "complex")) |>
        mutate(defn = factor(defn,
                                levels = c("simple", "complex")))
clean |> freq(defn)
# 1,190 good completes: 597 simple, 593 complex

## Fabric vs. makeup ------------------
clean <- clean |>
        mutate(issue = case_when(
                stim %in% c("simple/fabric", "complex/fabric") ~ "fabric",
                stim %in% c("simple/makeup", "complex/makeup") ~ "makeup")) |>
        mutate(issue = factor(issue,
                             levels = c("fabric", "makeup")))
clean |> freq(issue)
# 1,190 good completes: 593 fabric, 597 makeup


# Check manipulation checks ------------------
# 1 = Very easy; 7 = Very difficult
clean |> freq(Q25)
clean <- var_recode(data = clean, vars = Q25)
clean |> freq(Q25c)

clean |> freq(Q40)
clean <- var_recode(data = clean, vars = Q40)
clean |> freq(Q40c)

clean |> freq(Q55)
clean <- var_recode(data = clean, vars = Q55)
clean |> freq(Q55c)

clean |> freq(Q70)
clean <- var_recode(data = clean, vars = Q70)
clean |> freq(Q70c)

clean <- clean |>
        mutate(MCself = case_when(Q25 == Q25 ~ Q25c,
                                  Q40 == Q40 ~ Q40c,
                                  Q55 == Q55 ~ Q55c,
                                  Q70 == Q70 ~ Q70c))

clean |> freq(MCself)

clean |>
    select(MCself, defn) |>
    t_test(MCself ~ defn, detailed = TRUE)

clean |>
    select(MCself, defn) |>
    group_by(defn) |>
    descr(MCself)

    