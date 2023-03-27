#Create example dataset
#Beth Clark
#27/3/23

library(dplyr)

load("./data-testing/data.penguins.antarctica.Rdata")
d <- data.penguins.antarctica
head(d,2)

df <- d %>%
  select(site_name,penguin_count_recent,
         penguin_count_min,penguin_count_max) %>%
  data.frame()

names(df) <- c("colony_name","count","count_min","count_max")

head(df)
