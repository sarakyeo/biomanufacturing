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
