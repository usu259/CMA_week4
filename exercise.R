library(dplyr)
library(readr)
library("sf")
library(ggplot2)

wildschweine <- read_delim("wildschwein_BE_2056.csv",delim = ",")

sabi <- wildschweine %>% 
  st_as_sf(coords = c("E","N"),crs = 2056, remove = FALSE) %>%
  filter(TierName == "Sabi")

sabi <- sabi %>% filter(DatetimeUTC >= "2015-07-01",
                        DatetimeUTC < "2015-07-03")

ggplot(data = sabi)+
  geom_sf()

## Temporal window v = 60 min -> 4 fixes

distance_by_element <- function(later,now){
  as.numeric(
    st_distance(later,now, by_element = TRUE)
    
  )
}

sabi <- sabi %>% 
  mutate(nMinus2 = distance_by_element(lag(geometry, 2), geometry),
         nMinus1 = distance_by_element(lag(geometry, 1), geometry),
         nPlus1 = distance_by_element(geometry,lead(geometry,1)),
         nPlus2 = distance_by_element(geometry,lead(geometry,2)))                             )

sabi <- sabi %>%
  rowwise() %>%
  mutate(stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))) %>%
  ungroup()

sabi <- sabi %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

sabi_filter <- sabi %>% filter(!static)

sabi_filter %>% 
  ggplot(aes(E,N)) +
  geom_point(data = sabi, col = "red") +
  geom_path()+
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")


