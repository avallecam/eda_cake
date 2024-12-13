
# Steps -------------------------------------------------------------------

#' 1. Run the whole script to generate the table and figure.
#' 2. Modify one line: Replace `smoke` with `inc` or `race`. 
#' 3. Identify: What are we calculating in table and figure outputs?

# Setup ----------------------------------------------------------

# Load packages
library(mosaicData)
library(tidyverse)

# Access data
dat <- Gestation %>% 
  rename(birthweight_oz = wt, exposure = smoke) %>% # Replace `smoke` with `inc` or `race` 
  # filter(parity == 0) %>%
  select(id, birthweight_oz, exposure)

# Print data frame
dat

# Summary statistics ------------------------------------------------------

# Generate table
dat_summary <- dat %>% 
  group_by(exposure) %>% 
  summarise(
    weight_mean = mean(birthweight_oz),
    weight_sd = sd(birthweight_oz),
    total = n()
  ) %>% 
  mutate(
    weight_sem = weight_sd / sqrt(total),
    weight_low = weight_mean - (1.96 * weight_sem),
    weight_high = weight_mean + (1.96 * weight_sem),
  ) %>% 
  mutate(exposure = fct_reorder(.f = exposure, .x = weight_mean))

# Print table
dat_summary


# Variable distribution ---------------------------------------------------

# Generate and print figure
dat %>% 
  mutate(exposure = fct_reorder(.f = exposure, .x = birthweight_oz, .fun = mean, .na_rm = TRUE)) %>% 
  ggplot(aes(x = birthweight_oz)) +
  geom_histogram() +
  facet_grid(vars(exposure)) +
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


# extra questions ---------------------------------------------------------

#' 5. What summary statistics are stored in `weight_low` and `weight_high`?
#' 6. Activate the lines that start with hash tag (#). What changes?
