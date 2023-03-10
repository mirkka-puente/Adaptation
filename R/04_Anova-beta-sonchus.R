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

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use',
                'Treatment',"Leaf_number","Chlorophyll_content",
                "Root_water_content")

dt.beta <- dt1 %>% filter(Species == "Beta vulgaris") %>% 
  select(all_of(parameters)) 

dt.sonchus <- dt1 %>% filter(Species == "Sonchus oleraceus") %>% 
  select(all_of(parameters)) 


### ANOVA with p-values to decide significant week: Beta vulgaris ---------- 

#Table for leaf number and Chlorophyll_content

# Creating the table to keep the information 
num.var <- c("Leaf_number","Chlorophyll_content")
weeks <- c("W1", "W3","W4","W5","W6")

col_nam <- c("Variable", weeks)
num_col <- length(col_nam)
num_row <- length(num.var)
betap.table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
btp.table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(betap.table) <- col_nam
names(btp.table) <- col_nam

# Variables for the loop
nv <- num.var[1]
w <- weeks[1]
i <- 1


for(nv in num.var){
  
  pvls <- c()
  sigs <- c()
  
  for(w in weeks){
    
    # Data filtered by species
    dt2 <- dt.beta %>% filter(Week == w) %>% select(nv,"Treatment")
    dt2 <- na.omit(dt2)
    
    # Anova test
    model <- aov(dt2[[nv]] ~ dt2$Treatment, data = dt2)
  
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    p <- round(p, 4)
    
    # significance table
    if(p < 0.05){
      g <- "significant"
      sigs <- c(sigs ,g)
    }else{
      g <- "not significant"
      sigs <- c(sigs ,g)
    }
    
    # p value table
    
    pvls <- c(pvls ,p)
    
    
    # Remove data just in case
    rm(dt2)
  }
  # Allocating the values in the table
  betap.table$Variable[i] <- nv
  btp.table$Variable[i] <- nv
  betap.table[i, weeks] <- pvls
  btp.table[i, weeks] <- sigs
  i <- i + 1
  rm(pvls, sigs)
}

# Remove variables that are not gonna be used
rm(nv, i, p, w, parameters, model, g)

### ANOVA with p-values to decide significant week: Sonchus Oleraceous ---------- 

#Table for leaf number and Chlorophyll_content

# Creating the table to keep the information 

sonchusp.table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
sonp.table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(sonchusp.table) <- col_nam
names(sonp.table) <- col_nam

# Variables for the loop
nv <- num.var[1]
w <- weeks[1]
i <- 1


for(nv in num.var){
  
  pvls <- c()
  sigs <- c()
  
  for(w in weeks){
    
    # Data filtered by species
    dt2 <- dt.sonchus %>% filter(Week == w) %>% select(nv,"Treatment")
    dt2 <- na.omit(dt2)
    
    # Anova test
    model <- aov(dt2[[nv]] ~ dt2$Treatment, data = dt2)
    
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    p <- round(p, 4)
    
    # significance table
    if(p < 0.05){
      g <- "significant"
      sigs <- c(sigs ,g)
    }else{
      g <- "not significant"
      sigs <- c(sigs ,g)
    }
    
    # p value table
    
    pvls <- c(pvls ,p)
    
    
    # Remove data just in case
    rm(dt2)
  }
  # Allocating the values in the table
  sonchusp.table$Variable[i] <- nv
  sonp.table$Variable[i] <- nv
  sonchusp.table[i, weeks] <- pvls
  sonp.table[i, weeks] <- sigs
  i <- i + 1
  rm(pvls, sigs)
}

# Remove variables that are not gonna be used
rm(nv, i, p, w, col_nam, num_col,num_row, model, g, 
   ws.legend, ws.observ)
rm(dt.beta, dt.sonchus, num.var)

### ANOVA with p-values to decide significant groups in W6 ---------- 

w <- "W6"

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use',
                'Treatment',"Leaf_number","Chlorophyll_content",
                "Root_water_content")

dt2 <- dt1 %>% filter(Week == w) %>% select(all_of(parameters))

dt.beta <- dt2 %>% filter(Species == "Beta vulgaris")

dt.sonchus <- dt2 %>% filter(Species == "Sonchus oleraceus")

dt3 <- bind_rows(dt.beta, dt.sonchus)

rm(dt.beta, dt.sonchus)

### ANOVA with p-values to decide significant variables ---------- 

# Creating the table to keep the information 
num.var <- c("Leaf_number","Chlorophyll_content", "Root_water_content")
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
    dt4 <- dt3 %>% filter(Species == sp) %>% select(nv,"Treatment")
    dt4 <- na.omit(dt4)
    
    # Anova test
    
    model <- aov(dt4[[1]] ~ dt4$Treatment, data = dt4)
    
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
   model, weeks, species)

