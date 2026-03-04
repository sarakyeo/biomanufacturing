# Run after 00_BioMADE-GMO_setup.R

# Check randomization -----------------
## GM conditions -------
biomade |>
  freq(`Q3.3_Page Submit`) # n = 158
biomade |>
  freq(`Q6.3_Page Submit`) # n = 177
biomade |>
  freq(`Q9.3_Page Submit`) # n = 164

## Non-GM conditions --------
biomade |>
  freq(`Q12.3_Page Submit`) # n = 168
biomade |>
  freq(`Q15.3_Page Submit`) # n = 172
biomade |>
  freq(`Q18.3_Page Submit`) # n = 172

## Create variable, `stim`
biomade <- biomade |>
  mutate(
    stim = case_when(
      `Q3.3_Page Submit` == `Q3.3_Page Submit` ~ "GMO / Food",
      `Q6.3_Page Submit` == `Q6.3_Page Submit` ~ "GMO / Bandages",
      `Q9.3_Page Submit` == `Q9.3_Page Submit` ~ "GMO / Footwear",
      `Q12.3_Page Submit` == `Q12.3_Page Submit` ~ "Non-GMO / Food",
      `Q15.3_Page Submit` == `Q15.3_Page Submit` ~ "Non-GMO / Bandages",
      `Q18.3_Page Submit` == `Q18.3_Page Submit` ~ "Non-GMO / Footwear",
    )
  )

biomade |> freq(stim)

## Create var for GMO/non-GMO
biomade <- biomade |>
  mutate(
    GMstim = case_when(
      stim == "GMO / Food" |
        stim == "GMO / Bandages" |
        stim == "GMO / Footwear" ~ "GMO",
      stim == "Non-GMO / Food" |
        stim == "Non-GMO / Bandages" |
        stim == "Non-GMO / Footwear" ~ "Non-GMO"
    )
  )

biomade |> freq(GMstim)

## Create var for products
biomade <- biomade |>
  mutate(
    prodstim = case_when(
      stim == "GMO / Food" | stim == "Non-GMO / Food" ~ "Food",
      stim == "GMO / Bandages" | stim == "Non-GMO / Bandages" ~ "Bandages",
      stim == "GMO / Footwear" | stim == "Non-GMO / Footwear" ~ "Footwear",
    )
  )

biomade |> freq(prodstim)

## Checking whether mean age is different between expt groups
biomade |>
  select(age, stim) |>
  anova_test(age ~ stim) # ns