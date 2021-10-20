#tidyverse has ggplot2, dplyr, tidyr, and readr packages
library(tidyverse)

#VIDEO FROM STAT course: https://youtu.be/r1mZp7owmf0

#Import sj data
rdata_sj <- read_excel("Research/SARS-Cov-2 persistance/rdata_sj.xlsx")
View(rdata_sj)

#remove unnecessary columns
sj <- rdata_sj %>% select(-Plant,-Ave_conc,-upperlncco,-lowerlncco)
head(rdata_sj)

#subset N1 4C data
N14C <- subset(sj,Target=='N1' & Temp_C==4)
View(N14C)

#plot lnc/c0 vs time
plot(N14C$t_day,N14C$ln_cc0, col="red", pch=16, cex=2)
lines(loess.smooth(N14C$t_day,N14C$ln_cc0, span=1),col="red")

#Pearson's product-moment correlation
cor.test(N14C$t_day,N14C$ln_cc0)

#Calculate first order decay (k) - linear regression model forced through zero  
lm_N14C <- lm(formula =  ln_cc0~0+t_day, data = N14C)

#Statistical analysis (YT video: https://www.youtube.com/watch?v=u1cc1r_Y7M0)
summary(lm_N14C) #null hypothesis - slope = 0 

#NOTES:How to interpret r2 values (https://blog.minitab.com/en/adventures-in-statistics-2/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit)
#R-squared does not indicate whether a regression model is adequate. 
#You can have a low R-squared value for a good model, or a high R-squared value for a model that does not fit the data!
#The F-test of overall significance determines whether this relationship is statistically significant.

#check for residuals and normality
plot (lm_N14C)
shapiro.test(residuals(lm_N14C))

#### REPEAT ####

#subset data by target and temperature
N122C <- subset(sj,Target=='N1' & Temp_C==22)
N137C <- subset(sj,Target=='N1' & Temp_C==37)
N24C <- subset(sj,Target=='N2' & Temp_C==4)
N222C <- subset(sj,Target=='N2' & Temp_C==22)
N237C <- subset(sj,Target=='N2' & Temp_C==37)
P4C <- subset(sj,Target=='PMMoV' & Temp_C==4)
P22C <- subset(sj,Target=='PMMoV' & Temp_C==22)
P37C <- subset(sj,Target=='PMMoV' & Temp_C==37)

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
