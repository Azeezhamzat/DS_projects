## In this script we will take a set of variables characterizing texts and run a Factor Analysis
## This will check whether different proxies of a certain core concept cluster together
## In this script, we will explore the selected proxies of climate vs selected proxies of change

## We first install the packages
#install.packages("psych", dependencies=TRUE)
library(psych)
library(FactoMineR)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
select <- dplyr::select
rename <- dplyr::rename
library(car)
library(GPArotation)
library(nFactors)
library(stargazer)
library(extrafont)

## We set the base DIR
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Open the datafile
my_data <-read.csv("data_for_internal_validity_.csv", header=T)


## Clean the data
dat1 <- my_data %>%
  mutate(author= as.factor(author),
         year = as.numeric(year),
         #climate.damage= as.numeric(scale(log(climate.damage+3))),
         weather.damage= as.numeric(scale(log(weather.damage+3))),
         climate.change= as.numeric(scale(log(climate.change+3))),
         weather.change= as.numeric(scale(log(weather.change+3))),
         mitigation.damage= as.numeric(scale(log(mitigation.damage+3))),
         climate.threat= as.numeric(scale(log(climate.threat+3))),
         adaptation.threat= as.numeric(scale(log(adaptation.threat+3))),
         adaptation.conflict= as.numeric(scale(log(adaptation.conflict+3))),
         vader_zscore= as.numeric(scale(log(vader_zscore+3))),
         big.small.ratio= as.numeric(scale(log(big.small.ratio+3))),
         bio.loss= as.numeric(scale(log(bio.loss+3))),
         climate.conflict= as.numeric(scale(log(climate.conflict+3)))
         ) %>%
  select(weather.damage,climate.change,weather.change,mitigation.damage,adaptation.conflict, bio.loss)


## Here we select the variables which will be part of the factor analysis, and eliminate rows with missing values
data_subset <- dat1[ , c("weather.damage","climate.change","weather.change","mitigation.damage","adaptation.conflict","bio.loss")]  
dat2 <- dat1[complete.cases(data_subset), ]
dat3 = dat2[,1:6]


# Determine Number of Factors to Extract
ev <- eigen(cor(dat3)) # get eigenvalues
ap <- parallel(subject=nrow(dat3),var=ncol(dat3),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 2 factors,
# with oblimin rotation

fit <- factanal(dat3, 2, rotation="oblimin",scores="regression")
print(fit, digits=2, cutoff=.29, sort=TRUE)

load <- fit$loadings[,1:2] 
df_out <- as.data.frame(load)

## plot the Factor Analysis
theme<-theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.title=element_text(size=14), panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black", size=12),axis.text.y=element_text(colour="black",size=12),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))
ggplot(df_out,aes(x=Factor1,y=Factor2,color=row.names(df_out),label=row.names(df_out)))+
  theme + geom_text(size=5, nudge_y =0.03, nudge_x =0.16) + xlim(-0.2,1.25)+
  geom_segment(df_out,mapping=aes(x=0,y=0,xend = Factor1, yend =Factor2 ), size  =1.2,arrow = arrow())+
  ggtitle('Exploratory Factor Analysis Climate')

## Save the plot
ggsave('_Exploratory Factor Analysis Climate.pdf',  width = 5.5,
       height = 5,
       units = c("in"))








