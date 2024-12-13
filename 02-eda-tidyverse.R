
# Load packages
library(mosaicData)
library(tidyverse)

# Access data
data("Gestation")

# Test hypothesis ---------------------------------------------------------

Gestation %>% 
  select(smoke, wt) %>% 
  filter(smoke == "never" | smoke == "now") %>% 
  t.test(wt ~ smoke, data = .) %>% 
  broom::tidy()
