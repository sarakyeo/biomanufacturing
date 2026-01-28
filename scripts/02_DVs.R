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


# Support for biomanufacturing ------------
# Simple/Fabrics
clean |>
    select(Q31_1:Q31_3) |>
    freq()
clean <- var_recode(data = clean, vars = Q31_1:Q31_3)
clean |>
    select(Q31_1c:Q31_3c) |>
    freq()

# Complex/Fabrics
clean |>
    select(Q46_1:Q46_3) |>
    freq()
clean <- var_recode(data = clean, vars = Q46_1:Q46_3)
clean |>
    select(Q46_1c:Q46_3c) |>
    freq()

# Simple/Makeup
clean |>
    select(Q61_1:Q61_3) |>
    freq()
clean <- var_recode(data = clean, vars = Q61_1:Q61_3)
clean |>
    select(Q61_1c:Q61_3c) |>
    freq()

# Complex/Makeup
clean |>
    select(Q76_1:Q76_3) |>
    freq()
clean <- var_recode(data = clean, vars = Q76_1:Q76_3)
clean |>
    select(Q76_1c:Q76_3c) |>
    freq()

# Combine into a single columns -------------
clean <- clean |>
    mutate(support1 = case_when(
        Q31_1c == Q31_1c ~ Q31_1c,
        Q46_1c == Q46_1c ~ Q46_1c,
        Q61_1c == Q61_1c ~ Q61_1c,
        Q76_1c == Q76_1c ~ Q76_1c
    ))
clean |> freq(support1)

clean <- clean |>
    mutate(support2 = case_when(
        Q31_2c == Q31_2c ~ Q31_2c,
        Q46_2c == Q46_2c ~ Q46_2c,
        Q61_2c == Q61_2c ~ Q61_2c,
        Q76_2c == Q76_2c ~ Q76_2c
    ))
clean |> freq(support2)

clean <- clean |>
    mutate(support3 = case_when(
        Q31_3c == Q31_3c ~ Q31_3c,
        Q46_3c == Q46_3c ~ Q46_3c,
        Q61_3c == Q61_3c ~ Q61_3c,
        Q76_3c == Q76_3c ~ Q76_3c
    ))
clean |> freq(support3)

## Factor analysis on support items -------------
clean |>
    select(support1:support3) |>
    cortest.bartlett() # sig.

clean |>
    select(support1:support3) |>
    KMO() # Overall MSA = .75

clean |>
    select(support1:support3) |>
    fa.parallel() # 1 factor, 1 component

clean |>
    select(support1:support3) |>
    fa(.,
        nfactors = 1,
        rotate = "promax",
        fm = "pa"
    ) |>
    fa.diagram()

## Cronbach's alpha -------------
clean |>
    select(support1:support3) |>
    alpha() # Cronbach's alpha = 0.90

clean <- clean |>
    rowwise() |>
    mutate(
        support = mean(c(support1, support2, support3), na.rm = TRUE)
    )
clean |> freq(support)
descr(clean$support) # M = 4.89, SD = 1.48

## Even though the factor analysis resulted in 1 factor, it might 
## be worth considering separating general support for biomanufacturing
## and support for funding research on biomanufacturing

# Support for funding research -----------
clean |> 
    select(support2, support3) |> 
    freq()

clean |> 
    select(support2, support3) |> 
    cor_test() # Pearson's r = .76, p < .001
 
clean <- clean |> 
    rowwise() |> 
    mutate(funding = mean(
        c(support2, support3),
        na.rm = TRUE
    ))

clean |> freq(funding)
clean |> 
    group_by() |> 
    descr(funding) # M = 4.81, SD = 1.56

# Risks and Benefits ------------
# Simple/Fabrics
clean |>
    select(Q31_4:Q31_5) |>
    freq()
clean <- var_recode(data = clean, vars = Q31_4:Q31_5)
clean |>
    select(Q31_4c:Q31_5c) |>
    freq()

# Complex/Fabrics
clean |>
    select(Q46_4:Q46_5) |>
    freq()
clean <- var_recode(data = clean, vars = Q46_4:Q46_5)
clean |>
    select(Q46_4c:Q46_5c) |>
    freq()

# Simple/Makeup
clean |>
    select(Q61_4:Q61_5) |>
    freq()
clean <- var_recode(data = clean, vars = Q61_4:Q61_5)
clean |>
    select(Q61_4c:Q61_5c) |>
    freq()

# Complex/Makeup
clean |>
    select(Q76_4:Q76_5) |>
    freq()
clean <- var_recode(data = clean, vars = Q76_4:Q76_5)
clean |>
    select(Q76_4c:Q76_5c) |>
    freq()

# Combine into a single columns -------------
clean <- clean |>
    mutate(benefits = case_when(
        Q31_4c == Q31_4c ~ Q31_4c,
        Q46_4c == Q46_4c ~ Q46_4c,
        Q61_4c == Q61_4c ~ Q61_4c,
        Q76_4c == Q76_4c ~ Q76_4c
    ))
clean |> freq(benefits)
descr(clean$benefits) # M = 5.05, SD = 1.53

clean <- clean |>
    mutate(risks = case_when(
        Q31_5c == Q31_5c ~ Q31_5c,
        Q46_5c == Q46_5c ~ Q46_5c,
        Q61_5c == Q61_5c ~ Q61_5c,
        Q76_5c == Q76_5c ~ Q76_5c
    ))
clean |> freq(risks)
descr(clean$risks) # M = 4.49, SD = 1.60

# Relative Risks/Benefits (risks outweight benefits/benefits outweigh risks) --------------
# Risks and Benefits ------------
# Simple/Fabrics
clean |>
    select(Q34) |>
    freq()
clean <- var_recode(data = clean, vars = Q34)
clean |>
    select(Q34c) |>
    freq()

# Complex/Fabrics
clean |>
    select(Q49) |>
    freq()
clean <- var_recode(data = clean, vars = Q49)
clean |>
    select(Q49c) |>
    freq()

# Simple/Makeup
clean |>
    select(Q64) |>
    freq()
clean <- var_recode(data = clean, vars = Q64)
clean |>
    select(Q64c) |>
    freq()

# Complex/Makeup
clean |>
    select(Q79) |>
    freq()
clean <- var_recode(data = clean, vars = Q79)
clean |>
    select(Q79c) |>
    freq()

# Combine into a single columns -------------
clean <- clean |>
    mutate(risksbens = case_when(
        Q34c == Q34c ~ Q34c,
        Q49c == Q49c ~ Q49c,
        Q64c == Q64c ~ Q64c,
        Q79c == Q79c ~ Q79c
    ))
clean |> freq(risksbens) # higher values = more benefits; lower values = more risks; 4 = risks are equal to benefits
descr(clean$risksbens) # M = 4.53, SD = 1.65

clean |> 
  select(risksbens, risks, benefits) |> 
  cor_pmat()


# Cleaning perceived familiarity to include as covariate -------
clean |> freq(Q19)
clean <- var_recode(data = clean, vars = Q19)
clean |> 
        freq(Q19c)
clean <- clean |> 
    mutate(
        familiar = Q19c
    )
clean |> freq(familiar)
clean |> 
    select(familiar) |> 
    na.omit() |> 
    group_by() |> 
    descr(familiar) # M = 3.17, SD = 2.12
