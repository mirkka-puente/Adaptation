#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("car")

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)

#### Calling the other R scripts -------
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

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number","Chlorophyll_content", "Root_water_content", 
                "Roots_fresh_weight", "Roots_dry_weight")

num.var <- num.var <- c("Leaf_number","Chlorophyll_content", "Root_water_content",
                        "Roots_fresh_weight", "Roots_dry_weight")


w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(all_of(parameters))

dt.beta <- dt2 %>% filter(Species == "Beta vulgaris")

dt.sonchus <- dt2 %>% filter(Species == "Sonchus oleraceus")

dt3 <- bind_rows(dt.beta, dt.sonchus)

rm(dt.beta, dt.sonchus, dt2)


### Bonferroni test-----

# Creating the table to keep the information 
groups <- c("Control.Intermediate", "Control.Stress", "Intermediate.Stress")
species <- c("Beta vulgaris", "Sonchus oleraceus")
col_nam <- c("Species","Variables", groups)
num_col <- length(col_nam)
num_row <- length(species)  * length(num.var)

#Bonferroni table
b_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(b_table) <- col_nam

#result table
res_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(res_table) <- col_nam

# Variables for the loop
sp <- species[1]
nv <- num.var[1]
i <- 1


for(sp in species){
  for(nv in num.var){
    # Data filtered by species
    dt4 <- dt3 %>% filter(Species == sp) %>% select(one_of(nv,"Treatment"))
    
    # Bonferroni test
    
    bt <- pairwise.t.test(dt4[[1]], dt4$Treatment, p.adjust.method="bonferroni")
    
    # Allocating the values in the table
    b_table$Species[i] <- sp
    b_table$Variables[i] <- nv
    n1 <- round(bt$p.value[1], 3)
    n2 <- round(bt$p.value[2], 3)
    n3 <- round(bt$p.value[4], 3)
    vec <- c(n1, n2, n3)
    b_table[i, groups] <- vec
    
    #Results
    sig <- c()
    
    for(n in vec){
      if(n < 0.05){
        sig <- c(sig, "significant")
      }else{
        sig <- c(sig, "not significant")
      }
    }
    
    res_table[i, groups] <- sig
      
    i <- i + 1
    
    # Remove the data just in case
    rm(dt4, n1, n2, n3, bt, vec, sig)
  }
}

# Remove variables for future
rm(i, nv, sp, n)


