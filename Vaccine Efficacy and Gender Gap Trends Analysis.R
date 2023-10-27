library(tidyverse)
#SECTION 1
#Read in data
BioNTech <- read.csv("biontech_adolescents.csv")

#Separate into separate groups#
Treatment_group <- filter(BioNTech, group == "vaccine")
Control_group <- filter(BioNTech, group == "placebo")

#Sum positive COVID tests
Vacc_Covid <- sum(Treatment_group$outcome == "COVID-19")
Unvacc_Covid <- sum(Control_group$outcome == "COVID-19")

#N for the sample size of each group.
#Size_treatment = nrow(Treatment_group)
#Size_control = nrow(Control_group)
#Total_size = Size_treatment + Size_control
#I wanted to find the value of the total sample size just in case.

#Perform Left-tailed Binomial Test
binom.test(0, 1131, 18/1129, alternative = "less")

library(ggplot2)
#data frame for Bar chart
results <- data.frame(Type = c("Vaccine", "Placebo"), Freq = c(Vacc_Covid, Unvacc_Covid))

ggplot(results, aes(x = "", y = Freq, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Freq, "%")), position = position_stack(vjust = 0.5))

## Bar graph of protest type. ##
ggplot(data = results) +
  geom_bar(mapping = aes(x = Type, y = Freq), stat = "identity")

#SECTION 2
##Load data##
GlobalGap <- read.csv("GGI2013.csv")

## Fill missing data: either use na.rm = TRUE ##
## 2013 histogram and smoothed density estimate##
##first create the objects for each year##
GGI2006 <- GlobalGap$X2006
GGI2013 <- GlobalGap$X2013
GGI2010 <- GlobalGap$X2010

#
library(ggplot2)

##graph for 2013##
ggplot()+
  geom_histogram(data = GlobalGap, na.rm = TRUE,
                 aes(x= GGI2013, y=..density..),
                 binwidth = 0.01,color="black",fill="lightblue")+
  geom_density(data = GlobalGap, na.rm = TRUE, aes(x = GGI2013),
               color="sienna1",size=1.5)

##graph for 2006##
ggplot()+
  geom_histogram(data = GlobalGap, na.rm = TRUE,
                 aes(x= GGI2006, y=..density..),
                 binwidth = 0.01,color="black",fill="lightblue")+
  geom_density(data = GlobalGap, na.rm = TRUE, aes(x = GGI2006),
               color="sienna1",size=1.5)

##graph for 2010##
ggplot()+
  geom_histogram(data = GlobalGap, na.rm = TRUE,
                 aes(x= GGI2010, y=..density..),
                 binwidth = 0.01,color="black",fill="lightblue")+
  geom_density(data = GlobalGap, na.rm = TRUE, aes(x = GGI2010),
               color="sienna1",size=1.5)

#Next create a box and whisker plot of the same data.
boxplot(GGI2006, na.rm = TRUE)
boxplot(GGI2010, na.rm = TRUE)
boxplot(GGI2013, na.rm = TRUE)

##Summary stats for each year##
summary(GGI2006, na.rm = TRUE)
summary(GGI2010, na.rm = TRUE)
summary(GGI2013, na.rm = TRUE)

#Find z-score for each data value#
z_scores2006 <- (GGI2006 - mean(GGI2006, na.rm = TRUE)) / sd(GGI2006, na.rm = TRUE)
z_scores2006
z_scores2010 <- (GGI2010-mean(GGI2010, na.rm = TRUE))/sd(GGI2010, na.rm = TRUE)
z_scores2010
z_scores2013 <- (GGI2013-mean(GGI2013, na.rm = TRUE))/sd(GGI2013, na.rm = TRUE)
z_scores2013