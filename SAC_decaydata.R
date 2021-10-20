#tidyverse has ggplot2, dplyr, tidyr, and readr packages
library(tidyverse)

#VIDEO FROM STAT course: https://youtu.be/r1mZp7owmf0

#Import SAC data
rdata_sac <- read_excel("Research/SARS-Cov-2 persistance/rdata_sac.xlsx")
View(rdata_sac)

#remove unnecessary columns
sac <- rdata_sac %>% select(-Plant,-Ave_conc,-upperlncco,-lowerlncco)
head(rdata_sac)

#subset N1 4C data
N14C <- subset(sac,Target=='N1' & Temp_C==4)
View(N14C)

#plot lnc/c0 vs time
plot(N14C$t_day,N14C$ln_cc0, col="red", pch=16, cex=2)
lines(loess.smooth(N14C$t_day,N14C$ln_cc0, span=1),col="red")
#it doesn't look linear (monophasic) - check recovery???

#Pearson's product-moment correlation
cor.test(N14C$t_day,N14C$ln_cc0)
#p-value =0.4986 <0.5 significant?
#cor = -0.348 negative (inverse) correlation

#Calculate first order decay (k) - linear regression model forced through zero  
lm_N14C <- lm(formula =  ln_cc0~0+t_day, data = N14C)

#Statistical analysis (YT video: https://www.youtube.com/watch?v=u1cc1r_Y7M0)
summary(lm_N14C) #null hypothesis - slope = 0 
#p-value = 0.0297 <0.05 - reject null hypothesis (slope is statistically significant)
#r2=0.6448

#NOTES:How to interpret r2 values (https://blog.minitab.com/en/adventures-in-statistics-2/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit)
#R-squared does not indicate whether a regression model is adequate. 
#You can have a low R-squared value for a good model, or a high R-squared value for a model that does not fit the data!
#The F-test of overall significance determines whether this relationship is statistically significant.

#check for residuals and normality
plot (lm_N14C)
shapiro.test(residuals(lm_N14C))
#p-value = 0.409 > 0.05 thus normal distributed

#### REPEAT ####

#subset data by target and temperature
N122C <- subset(sac,Target=='N1' & Temp_C==22)
N137C <- subset(sac,Target=='N1' & Temp_C==37)
N24C <- subset(sac,Target=='N2' & Temp_C==4)
N222C <- subset(sac,Target=='N2' & Temp_C==22)
N237C <- subset(sac,Target=='N2' & Temp_C==37)
P4C <- subset(sac,Target=='PMMoV' & Temp_C==4)
P22C <- subset(sac,Target=='PMMoV' & Temp_C==22)
P37C <- subset(sac,Target=='PMMoV' & Temp_C==37)

#run linear regression
lm_N122C <- lm(formula =  ln_cc0~0+t_day, data = N122C)
lm_N137C <- lm(formula =  ln_cc0~0+t_day, data = N137C)
lm_N24C <- lm(formula =  ln_cc0~0+t_day, data = N24C)
lm_N222C <- lm(formula =  ln_cc0~0+t_day, data = N222C)
lm_N237C <- lm(formula =  ln_cc0~0+t_day, data = N237C)
lm_P4C <- lm(formula =  ln_cc0~0+t_day, data = P4C)
lm_P22C <- lm(formula =  ln_cc0~0+t_day, data = P22C)
lm_P37C <- lm(formula =  ln_cc0~0+t_day, data = P37C)

#get coefficient (k), r2, and p-value
summary(lm_N14C)
summary(lm_N122C)
summary(lm_N137C)
summary(lm_N24C)
summary(lm_N222C)
summary(lm_N237C)
summary(lm_P4C)
summary(lm_P22C)
summary(lm_P37C)
