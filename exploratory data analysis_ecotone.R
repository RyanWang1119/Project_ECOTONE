
### EDA
library(tidyverse)
library(tidymodels)
library(dplyr)
library(here)


WI_images_1Jan2023 <- read.csv(here("project_ecotone", "data", "WI_images_1Jan2023.csv"))

## select the useful columns
WI_images <- WI_images_1Jan2023 %>% 
  dplyr::select("deployment_id","species", "common_name", "timestamp", "number_of_objects")

ggplot(WI_images, aes(x=reorder(deployment_id,deployment_id,
                      function(x)+length(x)))) + geom_bar()+ coord_flip() + labs(x = 'Locations')

ggplot(WI_images, aes(x=reorder(species,species,
                                function(x)+length(x)))) + geom_bar()+ coord_flip() + labs(x = 'Species')


## North Beach Fort Cam
north_beach_fort <- filter(WI_images, deployment_id == "North Beach Fort Cam")
ggplot(north_beach_fort, aes(common_name)) + geom_bar()+ coord_flip() + labs(x = 'Species')

nbf_1 <- north_beach_fort %>% group_by(timestamp) %>% 
  summarise(number_of_objects=sum(number_of_objects,na.rm=T))

nbf_day <- north_beach_fort %>% mutate(time=substring(timestamp,1,10)) %>%
 group_by(time) %>% summarise(number_of_objects=sum(number_of_objects,na.rm=T))

nbf_hour_incomplete <- north_beach_fort %>% mutate(time=substring(timestamp,1,13)) %>%
  group_by(time) %>% summarise(number_of_objects=sum(number_of_objects,na.rm=T))

d1 <- read.csv(here::here("data", "d.csv")) # a dataframe with complete time
d2 <- read.csv(here::here("data", "d_incomplete.csv"))

d3 <- merge( d2, d1, by = "time", all.y =T)

nbf_hour_complete <- d3 %>%  mutate(number_of_objects=number_of_objects.x) %>% 
  dplyr::select("time","number_of_objects") %>% replace(is.na(.), 0)

write.csv(nbf_hour_complete, file = "nbf_hour_complete.csv")


## Big Cojo Cam
big_cojo <- filter(WI_images, deployment_id == "Big Cojo Cam")
ggplot(big_cojo, aes(common_name)) + geom_bar()+ coord_flip() + labs(x = 'Species')

bc1 <- big_cojo %>% group_by(timestamp) %>% 
  summarise(number_of_objects=sum(number_of_objects,na.rm=T))

bc_day_incomplete <- big_cojo %>% mutate(time=substring(timestamp,1,10)) %>%
  group_by(time) %>% summarise(number_of_objects=sum(number_of_objects,na.rm=T))

bc_hour_incomplete <- big_cojo %>% mutate(time=substring(timestamp,1,13)) %>%
  group_by(time) %>% summarise(number_of_objects=sum(number_of_objects,na.rm=T))

dd1 <- read.csv(here::here("data", "bc_d.csv"))
dd2 <- read.csv(here::here("data", "bc_hour_incomplete.csv"))
dd3 <- merge(dd2,dd1, by = "time", all.y = T)

bc_hour_complete <- dd3 %>%  mutate(number_of_objects=number_of_objects.x) %>% 
  dplyr::select("time","number_of_objects") %>% replace(is.na(.), 0)

write.csv(bc_day_incomplete, file = "bc_day_incomplete.csv")


