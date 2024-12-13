

# Steps -------------------------------------------------------------------

#' 1. Run the whole script to generate the table and figure.
#' 2. What summary statistics are stored in `weight_low` and `weight_high`?
#' 3. Activate the lines that start with hash tag (#). What changes?
#' 4. Replace all `smoke` words with `inc` or `race`. Discuss the change.

# Access to data ----------------------------------------------------------

library(mosaicData)
library(tidyverse)

data("Gestation")

# Summary statistics ------------------------------------------------------

# Generate table
dat_summary <- Gestation %>% 
  select(id, race, inc, smoke, number, gestation, wt) %>% 
  group_by(smoke) %>% 
  summarise(
    weight_mean = mean(wt),
    weight_sd = sd(wt),
    total = n()
  ) %>% 
  mutate(
    weight_sem = weight_sd / sqrt(total),
    weight_low = weight_mean - (1.96 * weight_sem),
    weight_high = weight_mean + (1.96 * weight_sem),
  ) %>% 
  # filter(smoke == "never" | smoke == "now") %>% 
  # mutate(smoke = str_to_title(smoke)) %>% 
  mutate(smoke = fct_reorder(.f = smoke, .x = weight_mean))

# Print table
dat_summary


# Variable distribution ---------------------------------------------------

# Generate and print figure
Gestation %>% 
  select(id, race, inc, smoke, number, gestation,wt) %>% 
  rename(birthweight_oz = wt) %>% 
  # filter(smoke == "never" | smoke == "now") %>% 
  # mutate(smoke = str_to_title(smoke)) %>% 
  mutate(smoke = fct_reorder(.f = smoke, .x = birthweight_oz, .fun = mean, .na_rm = TRUE)) %>% 
  ggplot(aes(x = birthweight_oz)) +
  geom_histogram() +
  facet_grid(vars(smoke)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  geom_vline(data = dat_summary, aes(xintercept = weight_mean)) +
  geom_vline(data = dat_summary, aes(xintercept = weight_low), linetype = 3) +
  geom_vline(data = dat_summary, aes(xintercept = weight_high), linetype = 3) +
  labs(
    title = "Is birth weight associated with maternal smoking*?",
    subtitle = "From the Child Health and Development Studies in 1961 and 1962.",
    x = "Child birth weight (in ounces)",
    y = "Frequency",
    caption = "* Question in the survey: Does the mother smoke?"
  )
