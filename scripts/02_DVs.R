# Mediating and Dependent variables
# Curiosity ------------
# Simple/Fabrics
clean |>
    select(Q29_1:Q29_4) |>
    freq()
clean <- var_recode(data = clean, vars = Q29_1:Q29_4)
clean |>
    select(Q29_1c:Q29_4c) |>
    freq()

# Complex/Fabrics
clean |>
    select(Q44_1:Q44_4) |>
    freq()
clean <- var_recode(data = clean, vars = Q44_1:Q44_4)
clean |>
    select(Q44_1c:Q44_4c) |>
    freq()

# Simple/Makeup
clean |>
    select(Q59_1:Q59_4) |>
    freq()
clean <- var_recode(data = clean, vars = Q59_1:Q59_4)
clean |>
    select(Q59_1c:Q59_4c) |>
    freq()

# Complex/Makeup
clean |>
    select(Q74_1:Q74_4) |>
    freq()
clean <- var_recode(data = clean, vars = Q74_1:Q74_4)
clean |>
    select(Q74_1c:Q74_4c) |>
    freq()

# Combine into a single columns -------------
clean <- clean |>
    mutate(curious1 = case_when(
        Q29_1c == Q29_1c ~ Q29_1c,
        Q44_1c == Q44_1c ~ Q44_1c,
        Q59_1c == Q59_1c ~ Q59_1c,
        Q74_1c == Q74_1c ~ Q74_1c
    ))
clean |> freq(curious1)

clean <- clean |>
    mutate(curious2 = case_when(
        Q29_2c == Q29_2c ~ Q29_2c,
        Q44_2c == Q44_2c ~ Q44_2c,
        Q59_2c == Q59_2c ~ Q59_2c,
        Q74_2c == Q74_2c ~ Q74_2c
    ))
clean |> freq(curious2)

clean <- clean |>
    mutate(curious3 = case_when(
        Q29_3c == Q29_3c ~ Q29_3c,
        Q44_3c == Q44_3c ~ Q44_3c,
        Q59_3c == Q59_3c ~ Q59_3c,
        Q74_3c == Q74_3c ~ Q74_3c
    ))
clean |> freq(curious3)

clean <- clean |>
    mutate(curious4 = case_when(
        Q29_4c == Q29_4c ~ Q29_4c,
        Q44_4c == Q44_4c ~ Q44_4c,
        Q59_4c == Q59_4c ~ Q59_4c,
        Q74_4c == Q74_4c ~ Q74_4c
    ))
clean |> freq(curious4)

## Factor analysis on curiosity items -------------
clean |>
    select(curious1:curious4) |>
    cortest.bartlett() # sig.

clean |>
    select(curious1:curious4) |>
    KMO() # Overall MSA = .87

clean |>
    select(curious1:curious4) |>
    fa.parallel() # 1 factor, 1 component

clean |>
    select(curious1:curious4) |>
    fa(.,
        nfactors = 1,
        rotate = "promax",
        fm = "pa"
    ) |>
    fa.diagram()

## Cronbach's alpha -------------
clean |>
    select(curious1:curious4) |>
    alpha() # Cronbach's alpha = 0.94

clean <- clean |>
    rowwise() |>
    mutate(
        curious = mean(c(curious1, curious2, curious3, curious4), na.rm = TRUE)
    )
clean |> freq(curious)
descr(clean$curious) # M = 4.79, SD = 1.63
