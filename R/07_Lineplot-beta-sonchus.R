#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(ggplot2)
library(ggpubr)

#### Calling the other R scripts -------
#Comment this after getting the data
source("R/00_download-from-drive.R")
source("R/01_check-data.R")
rm(i, p, w, ws.legend, ws.observ)

#### Creating duplicate of the original data ------

dt1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root 

dt1 <- mutate(dt1, 
              Aerial_water_content = 
                (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
              Root_water_content = 
                (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on ---------

parameters <- c("Week",'Species', 'Treatment',
                "Leaf_number","Chlorophyll_content", "Root_water_content")

num.var <- c("Leaf_number","Chlorophyll_content", "Root_water_content")


dt2 <- dt1 %>% select(all_of(parameters))

#### Beta vulgaris ------------------------------------------

dt.beta0 <- dt2 %>% filter(Species == "Beta vulgaris") 

dt.beta1 <- dt.beta0 %>% filter(Week == "W3") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.beta1$Species <- c(rep("Beta vulgaris")) 
dt.beta1$Week <- c(rep("W3"))

dt.beta2 <- dt.beta0 %>% filter(Week == "W4") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.beta2$Species <- c(rep("Beta vulgaris")) 
dt.beta2$Week <- c(rep("W4"))

dt.beta3 <- dt.beta0 %>% filter(Week == "W5") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.beta3$Species <- c(rep("Beta vulgaris")) 
dt.beta3$Week <- c(rep("W5"))

dt.beta4 <- dt.beta0 %>% filter(Week == "W6") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.beta4$Species <- c(rep("Beta vulgaris")) 
dt.beta4$Week <- c(rep("W6"))

d1 <- bind_rows(dt.beta1, dt.beta2)
d2 <- bind_rows(dt.beta3, dt.beta4)
dt.beta <- bind_rows(d1, d2)

rm(dt.beta1, dt.beta2, dt.beta3, dt.beta4, d1, d2)

#### Sonchus oleraceus ------------------------------------------
dt.sonchus0 <- dt2 %>% filter(Species == "Sonchus oleraceus")

dt.sonchus1 <- dt.sonchus0 %>% filter(Week == "W3") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.sonchus1$Species <- c(rep("Sonchus oleraceus")) 
dt.sonchus1$Week <- c(rep("W3"))

dt.sonchus2 <- dt.sonchus0 %>% filter(Week == "W4") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.sonchus2$Species <- c(rep("Sonchus oleraceus")) 
dt.sonchus2$Week <- c(rep("W4"))

dt.sonchus3 <- dt.sonchus0 %>% filter(Week == "W5") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.sonchus3$Species <- c(rep("Sonchus oleraceus")) 
dt.sonchus3$Week <- c(rep("W5"))

dt.sonchus4 <- dt.sonchus0 %>% filter(Week == "W6") %>% group_by(Treatment)%>%
  select(Leaf_number:Chlorophyll_content) %>% summarise_each(funs(mean))
dt.sonchus4$Species <- c(rep("Sonchus oleraceus")) 
dt.sonchus4$Week <- c(rep("W6"))

d1 <- bind_rows(dt.sonchus1, dt.sonchus2)
d2 <- bind_rows(dt.sonchus3, dt.sonchus4)
dt.sonchus <- bind_rows(d1, d2)

rm(dt.sonchus1, dt.sonchus2, dt.sonchus3, dt.sonchus4, d1, d2, dt2)

#### Line plot

plot1 <-ggplot(dt.beta, aes(x=Week, y=Leaf_number, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))


plot2 <-ggplot(dt.sonchus, aes(x=Week, y=Leaf_number, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))


plot3 <-ggplot(dt.beta, aes(x=Week, y=Chlorophyll_content, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))


plot4 <-ggplot(dt.sonchus, aes(x=Week, y=Chlorophyll_content, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))

ggarrange(plot1, plot2, plot3, plot4, 
          labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
