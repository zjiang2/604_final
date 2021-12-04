
library(tidyverse)
set.seed(12)

full <- read_csv("../data/combined_county_info.csv")

spec = c(train = .75, test = .15, validate = .10)

g = sample(cut(
  seq(nrow(full)), 
  nrow(full)*cumsum(c(0,spec)),
  labels = names(spec)
))

cci = split(full, g)

saveRDS(cci, file="../data/cci.rds")
