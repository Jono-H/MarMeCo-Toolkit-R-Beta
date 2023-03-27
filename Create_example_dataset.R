#Create example dataset
#Beth Clark
#27/3/23

library(dplyr)
library(sf)

load("./data-testing/data.penguins.antarctica.Rdata")
d <- data.penguins.antarctica
head(d,2)

table(d$common_name)

adp <- subset(d,common_name == "ADP")

df <- adp %>%
  data.frame() %>%
  select(site_name,penguin_count_recent,
         penguin_count_min,penguin_count_max) 

names(df) <- c("colony_name","colony_size","colony_size_min","colony_size_max")

head(df)

coords <- as.data.frame(st_coordinates(adp))

df$latitude <- coords$X
df$longitude <- coords$Y

head(df)

write.csv(df,"AdeliePenguin_example_dataset.csv",
          row.names = F)
