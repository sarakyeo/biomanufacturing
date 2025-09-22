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


# Information Seeking -----------------------
# Simple/Fabrics
clean |> 
    select(Q30_1:Q30_5) |> 
    freq()
clean <- var_recode(data = clean, vars = c(Q30_1:Q30_5))
clean |> 
    select(Q30_1c:Q30_5c) |> 
    freq()

# Complex/Fabrics
clean |> 
    select(Q45_1:Q45_5) |> 
    freq()
clean <- var_recode(data = clean, vars = c(Q45_1:Q45_5))
clean |> 
    select(Q45_1c:Q45_5c) |> 
    freq()

# Simple/Makeup
clean |> 
    select(Q60_1:Q60_5) |> 
    freq()
clean <- var_recode(data = clean, vars = c(Q60_1:Q60_5))
clean |> 
    select(Q60_1c:Q60_5c) |> 
    freq()

# Complex/Makeup
clean |> 
    select(Q75_1:Q75_5) |> 
    freq()
clean <- var_recode(data = clean, vars = c(Q75_1:Q75_5))
clean |> 
    select(Q75_1c:Q75_5c) |> 
    freq()

# Combine into a single columns -------------
clean <- clean |>
    mutate(infoseek1 = case_when(
        Q30_1c == Q30_1c ~ Q30_1c,
        Q45_1c == Q45_1c ~ Q45_1c,
        Q60_1c == Q60_1c ~ Q60_1c,
        Q75_1c == Q75_1c ~ Q75_1c
    ))
clean |> freq(infoseek1)

clean <- clean |>
    mutate(infoseek2 = case_when(
        Q30_2c == Q30_2c ~ Q30_2c,
        Q45_2c == Q45_2c ~ Q45_2c,
        Q60_2c == Q60_2c ~ Q60_2c,
        Q75_2c == Q75_2c ~ Q75_2c
    ))
clean |> freq(infoseek2)

clean <- clean |>
    mutate(infoseek3 = case_when(
        Q30_3c == Q30_3c ~ Q30_3c,
        Q45_3c == Q45_3c ~ Q45_3c,
        Q60_3c == Q60_3c ~ Q60_3c,
        Q75_3c == Q75_3c ~ Q75_3c
    ))
clean |> freq(infoseek3)

clean <- clean |>
    mutate(infoseek4 = case_when(
        Q30_4c == Q30_4c ~ Q30_4c,
        Q45_4c == Q45_4c ~ Q45_4c,
        Q60_4c == Q60_4c ~ Q60_4c,
        Q75_4c == Q75_4c ~ Q75_4c
    ))
clean |> freq(infoseek4)

clean <- clean |>
    mutate(infoseek5 = case_when(
        Q30_5c == Q30_5c ~ Q30_5c,
        Q45_5c == Q45_5c ~ Q45_5c,
        Q60_5c == Q60_5c ~ Q60_5c,
        Q75_5c == Q75_5c ~ Q75_5c
    ))
clean |> freq(infoseek5)

## Factor analysis on infoseek items -------------
clean |>
    select(infoseek1:infoseek5) |>
    cortest.bartlett() # sig.

clean |>
    select(infoseek1:infoseek5) |>
    KMO() # Overall MSA = .86

clean |>
    select(infoseek1:infoseek5) |>
    fa.parallel() # 2 factors, 1 component

clean |>
    select(infoseek1:infoseek5) |>
    fa(.,
        nfactors = 1,
        rotate = "promax",
        fm = "pa"
    ) |>
    fa.diagram() # single factor solution has better factor loadings

## Cronbach's alpha -------------
clean |>
    select(infoseek1:infoseek5) |>
    alpha() # Cronbach's alpha = 0.91

clean <- clean |>
    rowwise() |>
    mutate(
        infoseek = mean(c(infoseek1, infoseek2, infoseek3, infoseek4, infoseek5), na.rm = TRUE)
    )
clean |> freq(infoseek)
descr(clean$infoseek) # M = 4.24, SD = 1.70
