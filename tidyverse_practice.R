rm(list=ls())

library(tidyverse)
library(broom)

df.h <- data.frame( 
  hour     = factor(rep(1:24, each = 21)),
  price    = runif(504, min = -10, max = 125),
  wind     = runif(504, min = 0, max = 2500),
  temp     = runif(504, min = - 10, max = 25)  
)

df.g <- group_split(df.h, hour)

map(df.g, function(x) lm(price ~ wind + temp, data=x))

df.h.model <- df.h %>% nest_by(hour) %>% mutate(mod = list(lm(price ~ wind + temp, data = data)))
df.h.model %>% summarise(tidy(mod))
df.h.model %>% summarise(rsq = summary(mod)$r.squared)

df.h %>% summarise(head(across(), 5))
df.h %>% slice_head(n = 2)
