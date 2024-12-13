
# Steps -------------------------------------------------------------------

#' 1. Change "now" with "once did, not now". 
#' 2. Explain: Why does the output change?

# Test hypothesis ---------------------------------------------------------

# Load packages
library(mosaicData)
library(tidyverse)

# Access data
data("Gestation")

# Run test statistic
Gestation %>% 
  select(smoke, wt) %>% 
  filter(smoke == "never" | smoke == "now") %>% 
  t.test(wt ~ smoke, data = .) %>% 
  broom::tidy()
