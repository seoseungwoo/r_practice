rm(list=ls())

library(tidyverse)
library(broom)

df.h <- data.frame( 
  hour     = factor(rep(1:24, each = 21)),
  price    = runif(504, min = -10, max = 125),
  wind     = runif(504, min = 0, max = 2500),
  temp     = runif(504, min = - 10, max = 25)  
)

test_lm_model <- function(df){
  mod <- lm(price ~ wind + temp, data = df)
  return (tidy(summary(mod)))
}

df.g <- group_split(df.h, hour)
# save list
map(df.g, function(x) lm(price ~ wind + temp, data=x))

# save dataframe
map_df(df.g, function(x) test_lm_model(x))
# group_by - summarise로 
df.h %>% group_by(hour) %>% summarise(test_lm_model(.))



# use nest by
# dataframe 안에 list로 저장
df.h.model <- df.h %>% nest_by(hour) %>% mutate(mod = list(lm(price ~ wind + temp, data = data)))
df.h.model %>% summarise(tidy(mod))
df.h.model %>% summarise(rsq = summary(mod)$r.squared)

df.h %>% summarise(head(across(), 5))
df.h %>% slice_head(n = 2)




