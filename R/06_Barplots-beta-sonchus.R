#install.packages("ggpubr")
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

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number","Chlorophyll_content", "Root_water_content", 
                "Roots_fresh_weight", "Roots_dry_weight")

num.var <- c("Leaf_number","Chlorophyll_content", "Root_water_content",
             "Roots_fresh_weight", "Roots_dry_weight")


w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(one_of(parameters))

dt.beta <- dt2 %>% filter(Species == "Beta vulgaris")

dt.sonchus <- dt2 %>% filter(Species == "Sonchus oleraceus")

# General data to be used through script
dt3 <- bind_rows(dt.beta, dt.sonchus)

rm(dt.beta, dt.sonchus, dt2)

#### Function to calculate the mean and the standard deviation 
#   to calculate error bars

# varname : the name of a column containing the variable to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#### Summary of data for plot 1: Leaf number --------------
dt2 <- data_summary(dt3, varname="Leaf_number", 
                    groupnames=c("Species", "Treatment"))

p.leaf <- c(" ", " ", "*"," ", '*', "*")


# Standard deviation of the mean as error bar
plot1 <- ggplot(dt2, aes(x=Treatment, y=Leaf_number, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Leaf_number-sd, ymax=Leaf_number+sd), width=0.3,
                position=position_dodge(0.9)) +
  geom_text(aes(y = Leaf_number/2 + 1.5,
                label = c(p.leaf)), vjust = 1.5, 
            colour = "black", size = 7,
            position = position_dodge(1))

# Finished bar plot
plot1+labs(title="Average Number of leaves per species in week 6", 
           x="Treatments", y = "Number of leaves")+
  scale_fill_manual(values=c('plum3','skyblue'))

rm(dt2, p.leaf, w)

#### Summary of data for plot 2: Chlorophyll_content --------------
dt2 <- data_summary(dt3, varname="Chlorophyll_content", 
                    groupnames=c("Species", "Treatment"))


# Standard deviation of the mean as error bar
plot2 <- ggplot(dt2, aes(x=Treatment, y=Chlorophyll_content, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Chlorophyll_content-sd, 
                    ymax=Chlorophyll_content+sd), width=0.3,
                position=position_dodge(0.9)) 
# Finished bar plot
plot2+labs(title="Average chlorophyll content per species in week 6", 
           x="Treatments", y = "Chlorophyll content (SPAD)")+
  scale_fill_manual(values=c('plum3','skyblue'))

rm(dt2)
#### Summary of data for plot 3: Root water content --------------
dt2 <- data_summary(dt3, varname="Root_water_content", 
                    groupnames=c("Species", "Treatment"))

p.rwc <- c(" ", " ", " "," ", '*', "*")


# Standard deviation of the mean as error bar
plot3 <- ggplot(dt2, aes(x=Treatment, y=Root_water_content, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Root_water_content-sd, 
                    ymax=Root_water_content+sd), width=0.3,
                position=position_dodge(0.9)) +
  geom_text(aes(y = Root_water_content/2 + 1.5,
                label = c(p.rwc)), vjust = 9, 
            colour = "black", size = 7,
            position = position_dodge(1))

# Finished bar plot
plot3+labs(title="Average Root water content per species in week 6", 
           x="Treatments", y = "Root water content")+
  scale_fill_manual(values=c('plum3','skyblue'))

rm(dt2, p.rwc)

#### Summary of data for plot 4: Root fresh weight --------------
dt2 <- data_summary(dt3, varname="Roots_fresh_weight", 
                    groupnames=c("Species", "Treatment"))


# Standard deviation of the mean as error bar
plot4 <- ggplot(dt2, aes(x=Treatment, y=Roots_fresh_weight, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Roots_fresh_weight-sd, 
                    ymax=Roots_fresh_weight+sd), width=0.3,
                position=position_dodge(0.9)) 

# Finished bar plot
plot4+labs(title="Average Root fresh weight per species in week 6", 
           x="Treatments", y = "Root fresh weight (grams)")+
  scale_fill_manual(values=c('plum3','skyblue'))

rm(dt2)

#### Summary of data for plot 5: Root dry weight --------------
dt2 <- data_summary(dt3, varname="Roots_dry_weight", 
                    groupnames=c("Species", "Treatment"))


# Standard deviation of the mean as error bar
plot5 <- ggplot(dt2, aes(x=Treatment, y=Roots_dry_weight, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Roots_dry_weight-sd, 
                    ymax=Roots_dry_weight+sd), width=0.3,
                position=position_dodge(0.9))

# Finished bar plot
plot5+labs(title="Average Root dry weight per species in week 6", 
           x="Treatments", y = "Root dry weight (grams)")+
  scale_fill_manual(values=c('plum3','skyblue'))

rm(dt2)

