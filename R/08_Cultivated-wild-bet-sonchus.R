#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("car")

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)

#### Calling the other R scripts ---
#Comment this after getting the data
source("R/00_download-from-drive.R")
source("R/01_check-data.R")
rm(i, p, w)


#### Creating duplicate of the original data ------

dt1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root ----

dt1 <- mutate(dt1, 
              Aerial_water_content = 
                (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
              Root_water_content = 
                (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on -------

w <- "W6"

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number","Chlorophyll_content", "Root_water_content", 
                "Roots_fresh_weight", "Roots_dry_weight")

num.var <- num.var <- c("Leaf_number","Chlorophyll_content", "Root_water_content",
                        "Roots_fresh_weight", "Roots_dry_weight")

dt2 <- dt1 %>% filter(Week == w) %>% select(one_of(parameters))

dt.beta <- dt2 %>% filter(Species == "Beta vulgaris")

dt.sonchus <- dt2 %>% filter(Species == "Sonchus oleraceus")

dt3 <- bind_rows(dt.beta, dt.sonchus)

rm(dt.beta, dt.sonchus)

### ANOVA with p-values to decide significant variables ---------- 

# Creating the table to keep the information 
species <- c("Beta vulgaris", "Sonchus oleraceus")
col_nam <- c("Species","Variables", "P_value", "Significant")
num_col <- length(col_nam)
num_row <- length(species)  * length(num.var)
anova_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(anova_table) <- col_nam

# Variables for the loop
sp <- species[1]
nv <- num.var[1]
i <- 1


for(sp in species){
  for(nv in num.var){
    # Data filtered by species
    dt4 <- dt3  %>% select(one_of(nv,"Use"))
    dt4 <- na.omit(dt4)
    
    # Anova test
    
    model <- aov(dt4[[1]] ~ dt4$Use, data = dt4)
    
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    
    
    # Allocating the values in the table
    
    if (p < 0.05){
      anova_table$Species[i] <- sp
      anova_table$Variables[i] <- nv
      anova_table$P_value[i] <- p
      anova_table$Significant[i] <- "Yes"
    } else {
      anova_table$Species[i] <- sp
      anova_table$Variables[i] <- nv
      anova_table$P_value[i] <- p
      anova_table$Significant[i] <- "No"
    }
    
    i <- i + 1
    rm(dt4)
  }
}

#anova_table <- anova_table %>% filter(Significant == "Yes")

# Remove variables that are not gonna be used
rm(sp, nv, i, p, w, parameters, col_nam, num_col,num_row, num.var,
   model, species)
