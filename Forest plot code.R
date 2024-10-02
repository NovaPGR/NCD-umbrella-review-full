
library(ggplot2)
library(tidyverse)
library(patchwork)
library(forcats)
library(grid)
library(meta)
library(dplyr)
library(readr)

## Reading in the data and removing unneeded data
rrfp <- read_csv("reveiwofreviewsforrestplot.csv")
rrfp_cleaned <- select(rrfp,c("outcome", "paper", "model_type", "period", "tblnum", "number_of_studies", "number_of_participants", "effect", "ci_lo" , "ci_hi", "i_squared", "p_value"))

## Creating 95% Ci Columns 
create_ci_column <- function(data) {
  data %>%
    mutate(ci_lo2 = sprintf("%.2f", ci_lo),
           ci_hi2 = sprintf("%.2f", ci_hi),
           `95_ci` = paste0("[", ci_lo2, " ; ", ci_hi2, "]"))
}

rrfp_cleaned <- create_ci_column(rrfp_cleaned)

## Creating subsets 
vteprev <- 
  subset(rrfp_cleaned,tblnum == 1)
vterisk <- 
  subset(rrfp_cleaned,tblnum == 2)  
periopvte <- 
  subset(rrfp_cleaned,tblnum == 3)
cve <- 
  subset(rrfp_cleaned,tblnum == 4)
deltabmd <- 
  subset(rrfp_cleaned,tblnum == 5)
kg <- 
  subset(rrfp_cleaned,tblnum == 6)
substanceuse <- 
  subset(rrfp_cleaned,tblnum == 7)

## VTE-prevelance Forest plot  
pdf(file='vteprev.pdf') # Open PDF device with specific file name
forest(vteprev_meta,
       common=F, random=F,overall=F,
       leftcols= c("outcome",
                   "number_of_studies",
                   "number_of_participants"),
       rightcols = c("effect",
                     "95_ci",
                     "i_squared"),
       leftlabs = c("Outcome",
                    "Studies (n)",
                    "Total Participants" ),
       rightlabs = c("Prevalence",
                     "95% CI",
                     "I Squared"
       ),
       xlim = c(0,0.06),
       fontsize = 5, spacing = .5,
       print.tau2 = FALSE,
       print.stat = FALSE,
       print.pval.Q = FALSE,
       print.I2 = FALSE,
       col.square = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.square.lines = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.study = 'black',
       col.inside = "black",
) 
# Plot the forest
dev.off() # Turn the PDF device off


## Peroperative VTE Forest Plot 
pdf(file='periopvte.pdf') # Open PDF device with specific file name
forest(periopvte_meta,
       common=F, random=F,overall=F,
       leftcols= c("outcome",
                   "number_of_studies",
                   "number_of_participants"),
       rightcols = c("effect",
                     "95_ci",
                     "i_squared"),
       leftlabs = c("Outcome",
                    "Studies (n)",
                    "Total Participants" ),
       rightlabs = c("Odds",
                     "95% CI",
                     "I Squared"
       ),
       xlim = c(-15,15),
       fontsize = 5, spacing = .5,
       print.tau2 = FALSE,
       print.stat = FALSE,
       print.pval.Q = FALSE,
       print.I2 = FALSE,
       col.square = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.square.lines = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.study = 'black',
       col.inside = "black",
) 
# Plot the forest
dev.off() # Turn the PDF device off

## CVE Forest plot
pdf(file='cve.pdf') # Open PDF device with specific file name
forest(cve_meta,
       common=F, random=F,overall=F,
       leftcols= c("outcome",
                   "number_of_studies",
                   "number_of_participants"),
       rightcols = c("effect",
                     "95_ci",
                     "i_squared"),
       leftlabs = c("Outcome",
                    "Studies (n)",
                    "Total Participants" ),
       rightlabs = c("proportion",
                     "95% CI",
                     "I Squared"
       ),
       xlim = c(0,0.04),
       fontsize = 5, spacing = .5,
       print.tau2 = FALSE,
       print.stat = FALSE,
       print.pval.Q = FALSE,
       print.I2 = FALSE,
       col.square = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.square.lines = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.study = 'black',
       col.inside = "black",
) 
# Plot the forest
dev.off() # Turn the PDF device off

## BMD Δ Forest plot 
pdf(file='bmd.pdf') # Open PDF device with specific file name
forest(bmd_meta,
       common=F, random=F,overall=F,
       at=seq(from=-0.8, to =0.8, by=0.2),
       leftcols= c("outcome",
                   "period",
                   "number_of_studies",
                   "number_of_participants"),
       rightcols = c("effect",
                     "95_ci",
                     "i_squared"),
       leftlabs = c("Outcome",
                    "Time",
                    "Studies (n)",
                    "Total Participants" ),
       rightlabs = c("BMD Change",
                     "95% CI",
                     "I Squared"
       ),
       fontsize = 5, spacing = .5,
       print.tau2 = FALSE,
       print.stat = FALSE,
       print.pval.Q = FALSE,
       print.I2 = FALSE,
       col.square = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.square.lines = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.study = 'black',
       col.inside = "black",
) 
# Plot the forest
dev.off() # Turn the PDF device off

## Weight Change forest plot 
pdf(file='kg.pdf') # Open PDF device with specific file name
forest(kg_meta,
       common=F, random=F,overall=F,
       leftcols= c("outcome",
                   "number_of_studies",
                   "number_of_participants"),
       rightcols = c("effect",
                     "95_ci",
                     "i_squared"),
       leftlabs = c("Outcome",
                    "Studies (n)",
                    "Total Participants" ),
       rightlabs = c("Weight Change (kg)",
                     "95% CI",
                     "I Squared"
       ),
       xlim = c(-4,4),
       fontsize = 5, spacing = .5,
       print.tau2 = FALSE,
       print.stat = FALSE,
       print.pval.Q = FALSE,
       print.I2 = FALSE,
       col.square = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.square.lines = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.study = 'black',
       col.inside = "black",
) 
# Plot the forest
dev.off() # Turn the PDF device off

## Substance use forest plot 
pdf(file='substance.pdf') # Open PDF device with specific file name
forest(substance_meta,
       common=F, random=F,overall=F,
       leftcols= c("outcome",
                   "number_of_studies",
                   "number_of_participants"),
       rightcols = c("effect",
                     "95_ci",
                     "i_squared"),
       leftlabs = c("Outcome",
                    "Studies (n)",
                    "Total Participants" ),
       rightlabs = c("Odds",
                     "95% CI",
                     "I Squared"
       ),
       xlim = c(-1,3),
       fontsize = 5, spacing = 0.75,
       print.tau2 = FALSE,
       print.stat = FALSE,
       print.pval.Q = FALSE,
       print.I2 = FALSE,
       col.square = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.square.lines = rgb(red = 0, blue = 0, green = 0, alpha = 0 ),
       col.study = 'black',
       col.inside = "black",
) 
# Plot the forest
dev.off() # Turn the PDF device off
