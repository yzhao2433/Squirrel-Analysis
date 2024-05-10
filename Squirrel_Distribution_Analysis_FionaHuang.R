
# load required packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)

# read in datasets
squirrel_df <- read.csv("C:/Users/xinya/Downloads/cleaned_squirrel.csv")
combined <- read.csv("C:/Users/Xinya/Downloads/combined.csv")
hectare_df <- read.csv("C:/Users/xinya/Downloads/cleaned_hectare.csv")


# clean the hectare conditions to change the "calm, busy" to "moderate"
hectare_df$Conditions <- rep(NA, nrow(hectare_df))
hectare_df$Conditions[(hectare_df$Hectare.Conditions == "Calm, Busy") | 
                        (hectare_df$Hectare.Conditions == "Medium") |
                        (hectare_df$Hectare.Conditions == "Moderate")] <- "Moderate"
hectare_df$Conditions[(hectare_df$Hectare.Conditions == "Calm")] <- "Calm"
hectare_df$Conditions[(hectare_df$Hectare.Conditions == "Busy")] <- "Busy"


new_df <- subset(hectare_df, select=c("Hectare", "Conditions", "Shift", "Number.of.Squirrels"))



# ANOVA test
# first compare the reduced model and full model
# reduced model
intercept.fit <- lm(Number.of.Squirrels ~ 1, data=new_df)
# full model
full.fit <- lm(Number.of.Squirrels ~ Conditions + Shift, data=new_df)
anova(intercept.fit, full.fit)


# Second ANOVA test
# number of squirrels based on conditions and shift
aov.fit <- aov(Number.of.Squirrels ~ Conditions + Shift, data=new_df)
summary(aov.fit)


# create a ggplot
ggboxplot(new_df, x = "Conditions", y = "Number.of.Squirrels", color = "Shift",
          palette = c("purple", "orange"))

