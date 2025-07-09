library(tidycensus)
library(tidyverse)
library (ggplot2)
library(knitr)

#2010 Total Population
amherst_pop_2010 <- get_acs(
  geography = "county subdivision",
  variables = c("B01003_001"),  # Total population
  state = "NY",
  year = 2010,
  survey = "acs5",
  cache_table = TRUE
)

amherst_pop_2010 <- amherst_pop_2010 %>% 
  filter(str_detect(NAME, "Amherst town, Erie County, New York"))

amherst_pop_2010


#2015 Total Population
amherst_pop_2015 <- get_acs(
  geography = "county subdivision",
  variables = c("B01003_001"),  # Total population
  state = "NY",
  year = 2015,
  survey = "acs5",
  cache_table = TRUE
)

amherst_pop_2015 <- amherst_pop_2015%>% 
  filter(str_detect(NAME, "Amherst town, Erie County, New York"))

amherst_pop_2015

#2020 Total Population
amherst_pop_2020 <- get_acs(
  geography = "county subdivision",
  variables = c("B01003_001"),  # Total population
  state = "NY",
  year = 2020,
  survey = "acs5",
  cache_table = TRUE
)

amherst_pop_2020 <- amherst_pop_2020%>% 
  filter(str_detect(NAME, "Amherst town, Erie County, New York"))

amherst_pop_2015

#2023 Total Population
amherst_pop_2023 <- get_acs(
  geography = "county subdivision",
  variables = c("B01003_001"),  # Total population
  state = "NY",
  year = 2023,
  survey = "acs5",
  cache_table = TRUE
)

amherst_pop_2023 <- amherst_pop_2023%>% 
  filter(str_detect(NAME, "Amherst town, Erie County, New York"))

amherst_pop_2023


amherst_trend <- tibble (
  year = c(2010, 2015, 2020, 2023),
  population = c(
    amherst_pop_2010$estimate,
    amherst_pop_2015$estimate,
    amherst_pop_2020$estimate,
    amherst_pop_2023$estimate
  )
)


ggplot(amherst_trend, aes(x = year, y = population)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "darkred") +
  scale_x_continuous(
    breaks = seq(2010, 2025, by = 5),  # Tick marks every 5 years
    limits = c(2010, 2025)             # Optional: constrain x-axis
  ) +
  labs(
    title = "Amherst, NY Population Trend (2010–2023)",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()


UB_pop_2010 = c(19389, 9659, 29048)
UB_pop_2013 = c(19380, 10020, 29850)
UB_pop_2015 = c(19951, 9855, 29806)
UB_pop_2017 = c(21020, 9628, 30648)
UB_pop_2020 = c(22306, 10041, 32347)
UB_pop_2022 = c(20761, 11338, 32099)
UB_pop_2024 = c(20359, 11544, 31903)

UB_enrollment <- tibble(
  year = c(2010, 2013, 2015, 2017, 2020, 2022, 2024),
  undergrad = c(19389, 19380, 19951, 21020, 22306, 20761, 20359),
  grad = c(9659, 10020, 9855, 9628, 10041, 11338, 11544),
  total = c(29048, 29850, 29806, 30648, 32347, 32099, 31903)
)

kable(UB_enrollment, caption = "UB Enrollment by Year")


UB_long <- UB_enrollment %>%
  pivot_longer(cols = undergrad:total, names_to = "group", values_to = "population")

# Plot
ggplot(UB_long, aes(x = year, y = population, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = seq(2010, 2025, by = 5),
    limits = c(2010, 2025)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "University at Buffalo Enrollment Trends (2010–2024)",
    x = "Year",
    y = "Enrollment",
    color = "Population Type"
  ) +
  theme_minimal()


#Rate of Change
calc_growth <- function(start, end) {
  ((end - start) / start) * 100
}

years <- UB_enrollment$year
ug <- UB_enrollment$undergrad
grad <- UB_enrollment$grad
tot <- UB_enrollment$total

# Calculate growth rates
growth_data <- tibble(
  Interval = c("2010–2015", "2015–2020", "2020–2024"),
  Undergrad = c(
    calc_growth(ug[1], ug[3]),  # 2010 → 2015
    calc_growth(ug[3], ug[5]),  # 2015 → 2020
    calc_growth(ug[5], ug[7])   # 2020 → 2024
  ),
  Graduate = c(
    calc_growth(grad[1], grad[3]),
    calc_growth(grad[3], grad[5]),
    calc_growth(grad[5], grad[7])
  ),
  Total = c(
    calc_growth(tot[1], tot[3]),
    calc_growth(tot[3], tot[5]),
    calc_growth(tot[5], tot[7])
  )
)

# Round and pretty-print
growth_data %>%
  mutate(across(ends_with("_growth"), ~ round(., 2))) %>%
  knitr::kable(caption = "UB Enrollment Growth Rates (%)")

kable(growth_data, caption = "UB Enrollment Rates of Change (%)")%>% 
  kable_styling(bootstrap_options = 'striped')


ECC_pop_2010 = 15084
ECC_pop_2013 = 13649
ECC_pop_2015 = 12022
ECC_pop_2017 = 11135
ECC_pop_2020 = 8364
ECC_pop_2022 = 7843

ECC_enrollment <- tibble(
  year = c(2010, 2013, 2015, 2017, 2020, 2022),
  parttime = c(4827, 4610, 4091, 3866, 3305, 3297),
  fulltime = c(10257, 9039, 7931, 7269, 5059, 4546),
  total = c(15084, 13649, 12022, 11135, 8364, 7843)
)

kable(ECC_enrollment, caption = "ECC Enrollment by Year")


ECC_long <- ECC_enrollment %>%
  pivot_longer(cols = parttime:total, names_to = "group", values_to = "population")

# Plot
ggplot(ECC_long, aes(x = year, y = population, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = seq(2010, 2025, by = 5),
    limits = c(2010, 2025)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Erie Community College Enrollment Trends (2010–2022)",
    x = "Year",
    y = "Enrollment",
    color = "Population Type"
  ) +
  theme_minimal()


#Rate of Change
calc_growth <- function(start, end) {
  ((end - start) / start) * 100
}

years <- ECC_enrollment$year
part <- ECC_enrollment$parttime
full <- ECC_enrollment$fulltime
tot <- ECC_enrollment$total

# Calculate growth rates
growth_data <- tibble(
  Interval = c("2010–2015", "2015–2020", "2020–2024"),
  'Part time' = c(
    calc_growth(part[1], part[3]),  # 2010 → 2015
    calc_growth(part[3], part[5]),  # 2015 → 2020
    calc_growth(part[5], part[6])   # 2020 → 2022
  ),
  'Full time' = c(
    calc_growth(full[1], full[3]),
    calc_growth(full[3], full[5]),
    calc_growth(full[5], full[6])
  ),
  'Total' = c(
    calc_growth(tot[1], tot[3]),
    calc_growth(tot[3], tot[5]),
    calc_growth(tot[5], tot[6])
  )
)

# Round and pretty-print
growth_data %>%
  mutate(across(ends_with("_growth"), ~ round(., 2))) %>%
  knitr::kable(caption = "ECC Enrollment Rates of Change (%)")

kable(growth_data, caption = "ECC Enrollment Rates of Change (%)")%>% 
  kable_styling(bootstrap_options = 'striped')
