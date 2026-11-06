library(tidyverse)
library(tidycensus)
library(sf)

# Download 2020 tract population for Manatee County
pop_2020 <- get_acs(
  geography = "tract",
  state = "FL",
  county = "Manatee",
  variables = "B01003_001",
  year = 2020
) |>
  select(GEOID, pop_2020 = estimate)

# Download 2023 tract population with map geometry
pop_2023 <- get_acs(
  geography = "tract",
  state = "FL",
  county = "Manatee",
  variables = "B01003_001",
  year = 2023,
  geometry = TRUE,
  progress_bar = FALSE
) |>
  select(GEOID, NAME, pop_2023 = estimate)

# Join the two years and calculate population change rate (%)
manatee_tracts <- pop_2023 |>
  left_join(pop_2020, by = "GEOID") |>
  mutate(
    pop_change = pop_2023 - pop_2020,
    pct_change = (pop_2023 - pop_2020) / pop_2020 * 100
  )

# Save spatial dataset to an RDS file so map geometry is preserved
write_rds(manatee_tracts, "manatee_tracts.rds")

# Download 2020 and 2023 tract population for Manatee and surrounding counties
surrounding <- c("Manatee", "Sarasota", "Hillsborough", "Pinellas", "Polk", "Hardee", "DeSoto")

region_2020 <- get_acs(
  geography = "tract",
  state = "FL",
  county = surrounding,
  variables = "B01003_001",
  year = 2020
) |>
  select(GEOID, pop_2020 = estimate)

region_2023 <- get_acs(
  geography = "tract",
  state = "FL",
  county = surrounding,
  variables = "B01003_001",
  year = 2023
) |>
  select(GEOID, NAME, pop_2023 = estimate)

region_tracts <- region_2023 |>
  left_join(region_2020, by = "GEOID") |>
  mutate(
    county = str_extract(NAME, "[^;]+ County") |> str_remove(" County") |> str_trim(),
    pop_change = pop_2023 - pop_2020,
    pct_change = (pop_2023 - pop_2020) / pop_2020 * 100
  ) |>
  filter(!is.na(pct_change), pop_2020 > 0)

write_rds(region_tracts, "region_tracts.rds")

