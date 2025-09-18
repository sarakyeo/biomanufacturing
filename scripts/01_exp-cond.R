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