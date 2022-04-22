library(tidyverse)
library(ggtern)
data(soil_data)
data(USDA)
USDA_text <- USDA  %>% group_by(Label) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
soil_data <- data.frame('Clay' = runif(100), 'Sand' = runif(100), 'Silt' = runif(100))

ggplot(data = USDA, aes(y = Clay, x = Sand, z = Silt)) +
  theme_classic() +
  
  geom_density_tern(data = soil_data, aes(x = Sand, y = Clay, z = Silt, color=..level..),
                    bdl = 0.1, inherit.aes = F) +  # this works do not remove
  
  geom_point(data = soil_data, aes(x = Sand, y = Clay, z = Silt)) +
  
  coord_tern(L = "x", T = "y", R = "z") +
  geom_polygon(aes(fill = Label),alpha = 0.0, size = 0.5,color = "black") +
  geom_text(data = USDA_text,
            aes(label = Label),
            color = 'black',
            size = 2) +
  theme_showarrows() +
  theme_clockwise() +
  guides(fill= 'none', color='none')


## Soil depth horizon data - what we want it to BASICALLY create a bar chart of the soil depths in each plot of each stratum. 
soil_cats <-  c('Sand', 'Loamy Sand', 'Silty Clay Loam', 'Silt', 'Clay', 'Clay Loam', 'Sandy Clay')
dummydf <- data.frame('S_Class' = sample(soil_cats, 25, replace = T),
                      'Horizon' = rep(1:5, 5),
                      'Soil_pit' = as_factor(rep(1:5, each = 5)),
                      'Horizon_End' = round(runif(min = 1, max = 55, 25))
                                      )
rm(soil_cats)

res <- soil_coRe(dummydf)


# keep in mind eventually we want to be able to plot rock fragment over the relevant bar sections.... tricky mission but HOW DO Y ? Seems geom_point with some noise could be overlaid on the relevant sections of the plot ehh?


