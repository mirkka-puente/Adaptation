#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("factoextra")
#install.packages("psych")

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(agricolae)
library(factoextra)
library(psych)

#### Calling the other R scripts ---
#Comment this after getting the data
source("R/00_download-from-drive.R")
source("R/01_check-data.R")
rm(i, p, w)

#### Creating duplicate of the original data ------

dt1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root 

dt1 <- mutate(dt1, 
                Aerial_water_content = 
                  (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
                Root_water_content = 
                  (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on

num.var <- c("Plant_height", "Leaf_number", "Leaf_length",
                   "Leaf_width", "Leaf_area","Chlorophyll_content",
                   "Aerial_fresh_weight", "Aerial_dry_weight",     
                   "Root_length", "Roots_fresh_weight", 
                   "Roots_dry_weight", "Aerial_water_content",
                   "Root_water_content")

dt.beta <- dt1 %>% filter(Species == "Beta vulgaris") %>% 
  select(all_of(num.var)) 

dt.sonchus <- dt1 %>% filter(Species == "Sonchus oleraceus") %>% 
  select(all_of(num.var)) 

dt2 <- bind_rows(dt.beta, dt.sonchus) %>% drop_na()

#### PCA analysis (princomp) -----
pca.dt2 <- princomp(dt2, cor = TRUE)

# summary
summary(pca.dt2)


# correlation between components and data 
fviz_screeplot(pca.dt2)
round(cor(dt2, pca.dt2$scores), 3)

# COMMENTS
# Correlation between given data and principal component
# Determine: if there is relevancy between component and data
# The closer to 1  the closer related to the data set 

### Scree plot -----------
screeplot(pca.dt2, type = "l", main = "Screenplot for Water stress experiment")
abline(1, 0, col = "red", lty = 3)


### Plots with variables and points ------

# Correlated variables
fviz_pca_var(pca.dt2,
             col.var = "contrib",
             gradient.cols = c("indianred1", 
                               "lightgoldenrod1", 
                               "mediumseagreen"),
             repel = TRUE)# Avoid text overlapping

### Principal
pn <- principal(dt2, nfactors = 3, rotate = "none")
pca.table <- as.data.frame(round(cor(dt2, pn$scores), 3))

### Variables to work on 
c('leaf number', "root_water_content", "chlorophyll_content")

