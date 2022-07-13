#################
### LIBRARIES ###
#################

library(tseries)
library(robustbase)
library(lmtest)
library(astsa)
library(robust)
library(rr2)
library(sjPlot)
library(chron)
library(smooth)
library(timeSeries)
library(Mcomp)
library(export)
library(lme4)
library(glmulti)
library(LMERConvenienceFunctions)
library(MASS)
library(psych)
library(Hmisc)
library(ggplot2)
library(rcompanion)
require(cowplot)
library(blme)
library(emmeans)
library(lsmeans)

library(ggplot2)
library(ggpubr)

library(dplyr)
select <- dplyr::select
rename <- dplyr::rename
library(stargazer)
library(car)
library(reshape2)
library(nlme)
library(lmerTest)


#################
### LOAD FILE ###
#################


## We set the base DIR
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Open the datafile
my_data <-read.csv("Global_Climate_Sentiment&OWID.csv", header=T)

######################
###  TRIM THE DATA ###
######################

## Note: The ratios ranged from -1 to 1, but for statistics we made greater or equal to 0, 
## so that we could apply power transformations as suggested by box cox

dat1 <- my_data %>%
  mutate(author= as.factor(author),
         year=as.numeric(year),
         year2=as.numeric(scale(year)),
         blob = as.numeric(scale(blob_zscore_outliers)),
         vader = as.numeric(scale(vader_zscore_outliers)),
         afinn = as.numeric(scale(afinn_zscore_outliers)),
         co2 = as.numeric(scale(co2)),
         co2_growth_prct = as.numeric(scale(co2_growth_prct)),
         co2_growth_abs = as.numeric(scale(co2_growth_abs)),
         co2_per_capita = as.numeric(scale(co2_per_capita)),
         cumulative_co2 = as.numeric(scale(cumulative_co2)),
         coal_co2 = as.numeric(scale(coal_co2)),
         coal_co2_per_capita = as.numeric(scale(coal_co2_per_capita)),
         cumulative_coal_co2 = as.numeric(scale(cumulative_coal_co2))
         
  ) %>%
  select(blob, vader, afinn, co2, author, year,year2,co2_growth_prct,co2_growth_abs,co2_per_capita,cumulative_co2,coal_co2,coal_co2_per_capita,cumulative_coal_co2)



########################
###  REMOVE OUTLIERS ###
########################

dat2 <- dat1

hist(dat2$vader)

###############################
### SIMPLE PARTY DIFERENCES ###
###############################

###################################
### transform data using BoxCox ###
###################################

dat2$vader2=dat2$vader+3

Box = boxcox(vader2 ~ co2 + year2 + co2_growth_prct + co2_growth_abs + co2_per_capita + cumulative_co2 + coal_co2 + coal_co2_per_capita + cumulative_coal_co2,
             data =dat2,
             lambda = seq(-6,6,0.1))

Cox = data.frame(Box$x, Box$y)

Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]

Cox2[1,]

lambda = Cox2[1, "Box.x"]
lambda ## optimal lambda 1

dat2$vader3 = scale(((dat2$vader2**lambda)-0.7)/lambda) 

#########################
## socioeconomic model ##
#########################

econom_mixed_model = lmer(vader3 ~ co2 + year2 + co2_growth_prct + co2_growth_abs + co2_per_capita + cumulative_co2 + coal_co2 + coal_co2_per_capita + cumulative_coal_co2+(1|author), data = dat2)
#econom_linear_model = lm(vader3 ~ co2 + year2 + co2_growth_prct + co2_growth_abs + co2_per_capita + cumulative_co2 + coal_co2 + coal_co2_per_capita + cumulative_coal_co2, data = dat2)

summary(econom_mixed_model)
#summary(econom_linear_model)
confint(econom_mixed_model)
#confint(econom_linear_model)


res_econom_mixed_model = residuals(econom_mixed_model)
shapiro.test(res_econom_mixed_model) 
plotNormalHistogram(res_econom_mixed_model)

acf(res_econom_mixed_model)
BIC(econom_mixed_model)

## Export model##
class(econom_mixed_model) <- "lmerMod"
stargazer(econom_mixed_model,   title="Climate_Sentiment_model", align=TRUE,type = 'html', out = 'sentiment_model.jpg', flip=TRUE)


#################
### cumulative_co2 model ##
#################

## we focus on the period comprising the years since the first Paris Climate Accords
dat3 <- subset(dat2, year > 1995 & year < 2021) 

##cumulative_co2 model
ggplot(dat3, aes(x=co2, y=cumulative_co2)) +
  geom_boxplot()+scale_fill_brewer(palette="RdBu") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  theme_minimal()


theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)

b <- ggplot(dat3, aes(x = co2, y = year))
# Scatter plot with regression line
b + geom_point(shape=18)+
  geom_smooth(method = "lm") 
ggsave("co2_in_years.png", width = 5, height = 5)


c <- ggplot(dat3, aes(x = cumulative_co2, y = year))
# Scatter plot with regression line
c + geom_point(shape=18)+
  geom_smooth(method = "lm")
ggsave("cumulative_co2_in_years.png", width = 5, height = 5)

b <- ggplot(dat3, aes(co2))
b + geom_bar(stat = "bin", fill="orange")
ggsave("co2_bar.png", width = 5, height = 5)

c <- ggplot(dat3, aes(cumulative_co2))
c + geom_bar(stat = "bin", fill="indianred")
ggsave("cumulative_co2_bar.png", width = 5, height = 5)


econom_model2 = lmer(vader3 ~ co2 + year2 + co2_growth_prct + co2_growth_abs + co2_per_capita + cumulative_co2 + coal_co2 + coal_co2_per_capita + cumulative_coal_co2+(1|author), data = dat3)
summary(econom_model2)

res_econom_model2 = residuals(econom_model2)
shapiro.test(res_econom_model2) 
plotNormalHistogram(res_econom_model2)

acf(res_econom_model2)
BIC(econom_model2)

## Export model##
class(econom_model2) <- "lmerMod"
stargazer(econom_model2,   title="Sentiment_model co2", align=TRUE,type = 'html', out = 'sentiment_model_co2.htm', flip=TRUE)



######################
### Compare slopes ###
######################

period_year_model3 = lmer(vader3 ~ year2*cumulative_co2+(1|author), data = dat3)
summary(period_year_model3)

# Obtain slopes
m.lst3 <- emtrends(period_year_model3, 'cumulative_co2',var ='year2',options = list())
m.lst3
s
