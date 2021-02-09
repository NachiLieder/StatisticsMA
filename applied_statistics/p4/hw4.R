## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------------------------------------------------------
# import libraries
library(rmarkdown)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pivottabler)
library(gtsummary)
library(ggpubr)
library(ggfortify)
library(cluster)
library(MASS)
library(lmtest)
library(fBasics)
library(rcompanion)
library(gridExtra)
library(cowplot)
library(kableExtra)
library(haven)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(lme4)
library(reshape2)
library(kableExtra)
library(pander)
library(performance)
library(pROC)
library(sqldf)
library(nlme)
library(ggeffects)
library(doBy)
library(tseries)
library(forecast)




## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------------------------------------------------------
# data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = # "", fileEncoding = "UTF-8-BOM")

#setwd("School/courses/applied_stats/p4")
#write.csv( data, 'data.csv')
data <- read.csv('download')


## ----initial_plot , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------------------------------------------
#convert dates columns
# plot the weekly cases in france
data$dateRep<-as.Date(data$dateRep, '%d/%m/%Y')
france <-  data[ data$countriesAndTerritories=='France' ,]
p <- ggplot(france, aes(x=dateRep, y=cases_weekly)) +
  geom_line() + 
  xlab("")+ ggtitle("Plot of weekly cases in France")
grid.arrange(p)

par(mfrow=c(1,1))
# dickey fuller test
options(warn=-1)
pander(adf.test(france$cases_weekly), caption = "Dickey Fuller Test")



## ----exlplore_data , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------------------------------------------

par(mfrow=c(1,2))
# dickey fuller test
options(warn=-1)
#pander(adf.test(france$cases_weekly), caption = "Dickey Fuller Test")

#par(mfrow=c(1,2))
p1<- acf(france$cases_weekly, plot=FALSE)
plot(p1,main = "ACF")

p2<- pacf(france$cases_weekly, plot=FALSE)
plot(p2,main = "PACF")

# box cox transofrmation
tseries_h<- france$cases_weekly
#bx<- BoxCox(tseries_h, lambda = 0.5)
#plot.ts(bx)
#lambda <- BoxCox.lambda(tseries_h)
#adf.test(bx)



## ----auto_arima1 , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------------------------------------
#Aproach 1
par(mfrow=c(1,1))
options(warn = -1)
f<- auto.arima(tseries_h)
plot(forecast(f,h=20))
#pander(summary(f) , caption ='Summary Stepwise ARIMA')
#pander(f$coef , caption ='Coefficients non Stepwise ARIMA')
#pander(f$aic , caption ='AIC non Stepwise ARIMA')
#pandoc.table(f$aic, keep.line.breaks = FALSE,caption ='AIC Stepwise ARIMA',style = 'rmarkdown')


par(mfrow=c(1,3))
# lewts check auto coreelation since we are looking at the diff (0,1,0)
plot(diff(tseries_h),main = "Scatter Differences")
p1<- acf(diff(tseries_h),plot = FALSE)
plot(p1,main = "ACF")
p2<- pacf(diff(tseries_h),plot = FALSE)
plot(p2,main = "PACF")


## ----auto_arima2 , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------------------------------------

#Aproach 2
options(warn = -1)
f<- auto.arima(tseries_h, stepwise = FALSE,seasonal=FALSE)
pander(f$coef , caption ='Coefficients non Stepwise ARIMA')
#pandoc.table(f$aic, keep.line.breaks = FALSE,caption ='AIC non Stepwise ARIMA',style = #'rmarkdown')



#pander(f$aic , caption ='AIC non Stepwise ARIMA')
# lewts check auto coreelation since we are looking at the diff (0,1,0)
#resid<- checkresiduals(f, plot=FALSE,test=FALSE)
#par(mfrow=c(1,1))
#plot(forecast(f,h=20))

#checkresiduals(f,test = FALSE)


## ----function , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------------------------------------------------------------------

# define plotting function
plot_predictions <- function(france, m , title){
  pred <- data.frame(week = france$week,
                         cases_weekly = france$cases_weekly,
                         predicted_values = predict(m, newdata = france))
  
 ggplot(pred, aes(x = week)) +
  geom_point(aes(y = cases_weekly), size = 1, alpha = 0.5) + geom_line(aes(y = predicted_values), colour = "red")+ ggtitle(title)
}



## ----gam , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------------------------------------------------------



library(mgcv)
library(stringr)
# split the column to use the week as an input
france$week <- str_split_fixed(france$year_week,'-',2)[,2]
france$week <- as.numeric(france$week)


p1 <- ggplot(france, aes(week, cases_weekly)) + geom_point()
p1



## ----s_week , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------------------------------------------------
###########################################################
# s(week)
m1 <- gam(cases_weekly ~ s(week), data = france , method = "REML")
# plot diagnostics
par(mfrow = c(2,2))
gam.check(m1)

#the larger the number, the more wiggly the fitted model.
summary(m1)

#model of s(week)
#p1<- ggplot(france, aes(week, cases_weekly)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x))

p1 <- plot_predictions(france,m1,"cases_weekly ~ s(week)")
grid.arrange(p1)


## ----ti_week , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------------------------------------------------
###########################################################
# ti(week) 

m2 <- gam(cases_weekly ~ ti(week), data = france)
# plot diagnostics
par(mfrow = c(2,2))
gam.check(m2)

#the larger the number, the more wiggly the fitted model.
#summary(m2)

#model of s(week)
#ggplot(france, aes(week, cases_weekly)) + geom_point() + geom_smooth(method = "gam", formula = #y ~ti(x))
#grid.arrange(p1)
p2<- plot_predictions(france,m2,"cases_weekly ~ ti(week)")



###########################################################
# ti(week) +s(week)

m3 <- gam(cases_weekly ~ ti(week) +s(week), data = france)
# plot diagnostics
par(mfrow = c(2,2))
gam.check(m3)

#the larger the number, the more wiggly the fitted model.
#summary(m3)

#model of s(week)
#p2<- ggplot(france, aes(week, cases_weekly)) + geom_point() + geom_smooth(method = "gam", #formula = y ~ti(x) + s(x))
#grid.arrange(p2)

p3<- plot_predictions(france,m3,"cases_weekly ~ ti(week) +s(week)")


###########################################################
m11 <- gam(cases_weekly ~ te(week), data = france)

#the larger the number, the more wiggly the fitted model.
#summary(m11)

#model of te(week)
#ggplot(france, aes(week, cases_weekly)) + geom_point() + geom_smooth(method = "gam", formula = y ~te(x))

p11<- plot_predictions(france,m11,"cases_weekly ~ te(week) ")
###########################################################


grid.arrange(p2,p3,p11)



## ----k , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------------------------------------------------------
# change the number of basis functions

m4 <- gam(cases_weekly ~ s(week, bs = 'cc', k = 52), data = france , method = "REML")
m5 <- gam(cases_weekly ~ s(week, bs = 'cc', k = 1), data = france , method = "REML")
m6 <- gam(cases_weekly ~ s(week, bs = 'cc', k = 10), data = france , method = "REML")
m7 <- gam(cases_weekly ~ s(week, bs = 'cc', k = 20), data = france , method = "REML")



#model of s(week)
#ggplot(france, aes(week, cases_weekly)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x, bs = 'cc', k = 52))


p4<- plot_predictions(france,m4,"Cyclic ~ 52 knots")
p5<-plot_predictions(france,m5,"Cyclic ~ 1 knot")
p6<-plot_predictions(france,m6,"Cyclic ~ 10 knots")
p7<-plot_predictions(france,m7,"Cyclic ~ 20 knots")

grid.arrange(p4,p5,p6,p7)



## ----gamma , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------------------------------------------

###########################################################
# change the smoothing parameter - gamma


m8 <- gam(cases_weekly ~ s(week), gamma=1 ,data = france , method = "REML")
p8 <-plot_predictions(france,m8, 'gamma = 1')

m9 <- gam(cases_weekly ~ s(week), gamma=20 ,data = france , method = "REML")
p9 <-plot_predictions(france,m9, 'gamma = 20')

m10 <- gam(cases_weekly ~ s(week), gamma=0.1 ,data = france , method = "REML")
p10 <- plot_predictions(france,m10, 'gamma = 0.1')


grid.arrange(p8,p9,p10)


## ----anova , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------------------------------------------

m1_an <- gamm(cases_weekly ~ ti(week), data = france)
m2_an <- gamm(cases_weekly ~ s(week), data = france)
m3_an <- gamm(cases_weekly ~ s(week) + ti(week), data = france)
m4_an <- gamm(cases_weekly ~ s(week, bs = 'cc', k = 52), data = france , method = "REML")
m5_an <- gamm(cases_weekly ~ s(week, bs = 'cc', k = 1), data = france , method = "REML")
m6_an <- gamm(cases_weekly ~ s(week, bs = 'cc', k = 10), data = france , method = "REML")
m7_an <- gamm(cases_weekly ~ s(week, bs = 'cc', k = 20), data = france , method = "REML")
m8_an <- gamm(cases_weekly ~ s(week), gamma=1 ,data = france , method = "REML")
m9_an <- gamm(cases_weekly ~ s(week), gamma=20 ,data = france , method = "REML")
m10_an <- gamm(cases_weekly ~ s(week), gamma=0.1 ,data = france , method = "REML")
m11_an <- gamm(cases_weekly ~ te(week) ,data = france , method = "REML")

anova(m1_an$lme,
      m2_an$lme,
      m3_an$lme)

anova(m4_an$lme,
      m5_an$lme,
      m6_an$lme,
      m7_an$lme)

anova(m7_an$lme,
      m8_an$lme,
      m9_an$lme)


## ----bam , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------------------------------------------------------

# check where the drop of the rho is
#once we hit a high rho , we see that the trend is random  - and they are due to auto correlated errors

aicvec<-remlvec<-c()
rovec<- seq(.01,1,0.05)
for (k in 1:length(rovec)){
  tmp<- bam( cases_weekly ~ s(week) , rho = rovec[k],data = france)
  aicvec[k] <- AIC(tmp)
  remlvec[k] <- tmp$gcv.ubre
  #cat(rovec[k], aicvec[k], remlvec[k],"\n")
  
  
}

#matplot(rovec, cbind(aicvec,remlvec))
q1<- qplot(rovec, aicvec)+ ggtitle('Rho value vs AIC ')
q2<- qplot(rovec, remlvec)+ ggtitle('Rho values vs REML' )

# check certain rhos

m13 <- bam(cases_weekly ~ s(week) , rho =0.7,data = france)
p13 <- plot_predictions(france,m13, 'BAM with rho 0.7')

m14 <- bam(cases_weekly ~ s(week) , rho =0.5,data = france)
p14 <- plot_predictions(france,m14, 'BAM with rho 0.5')
grid.arrange(q1,q2)
grid.arrange(p13,p14)



## ----vc , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------------------------------------------------------------------------
data$cases_per_capita <- data$cases_weekly / data$popData2019 * 100000

data$week <- str_split_fixed(data$year_week,'-',2)[,2]
data$week <- as.numeric(data$week)


# crete set of data aggregated by continent and sum weekly cases and population (per week)
agg_data <- data %>% 
  group_by(continentExp,week) %>% 
  summarise(cases_weekly = sum(cases_weekly),
            popData2019 = sum(popData2019))

agg_data$cases_per_capita <- agg_data$cases_weekly / agg_data$popData2019 * 100000

agg_data <- agg_data[(agg_data$continentExp == 'Asia')|((agg_data$continentExp == 'Europe')),]


agg_data$cont <- factor(agg_data$continentExp)
agg_data<- na.omit(agg_data)

cases_per_capita<- as.vector(agg_data$cases_per_capita)
dateRep <- agg_data$dateRep
cont <- agg_data$cont

cont_plot <- ggplot(agg_data, aes(x = week, y = cases_per_capita, colour = factor(cont)))+ 
  geom_point(size=2.5)+ ggtitle('Plot of Avg Adjusted cases per 100K weekly')
cont_plot$labels$colour <- "Continent"
grid.arrange(cont_plot)


## ----vc_gam , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------------------------------------------------
m3_1 <- gam(cases_per_capita ~ s(week)+ s(week,by=cont,bs="cc"), data = agg_data , method = "REML")
m3_2 <- gam(cases_per_capita ~ s(week)+ te(week,by=cont), data = agg_data , method = "REML")
m3_3 <- gam(cases_per_capita ~ s(week)+ s(week,by=cont,bs="cc")+ te(week,by=cont), data = agg_data , method = "REML")
summary(m3_4 <- gam(cases_per_capita ~ cont + s(week)+ s(week,by=cont,bs="cc")+ te(week,by=cont), data = agg_data ,
method = "REML"))

par(mfrow=c(3,3))
plot(m3_4)

pander(anova(m3_1,m3_2,m3_3,m3_4),caption = 'Anova 4 models')




## ----tstat , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------------------------------------------

##### -------- aggregating all countries in same continent a- sum all weekly cases and adjust
asia <-  agg_data[(agg_data$continentExp == 'Asia'),]$cases_per_capita
europe <-  agg_data[(agg_data$continentExp == 'Europe'),]$cases_per_capita

pander(adf.test(europe), caption = "Dickey Fuller Test - Europe Lag Differences")
pander(adf.test(asia), caption = "Dickey Fuller Test- Asia Lag Differences")

# low p val -> not equal
pander(t.test(diff(asia,1), y = diff(europe,1), alternative = c("two.sided"), paired = FALSE, var.equal = FALSE, conf.level = 0.95))



## ----ts_clustering , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------------------------------------------
cols <- c('countriesAndTerritories','week','cases_weekly')
pv <- data[cols]
library(reshape2)

#pv <- pv[pv$week > 10 , ]
rr <- recast(pv, countriesAndTerritories  ~ week, id.var = c( "week",'countriesAndTerritories'))
rr[is.na(rr)] <- 0

require(dtw)
jj <- dist(rr[,-1], method="dtw")
mds <- cmdscale(jj, eig=TRUE)
plot(mds$points[,1:2], pch=16, cex=.8, xlab="Principal coordinate 1", ylab="Principal coordinate 2")

require(cluster)
p3 <- pam(jj,3)      # k-medoids clustering
t_table <- table(p3$clust,rr[,1])

#merge with continent and compare
tr_table <- t(t_table)

plot(mds$points[,1:2], pch=16, cex=.8, xlab="Principal coordinate 1", ylab="Principal coordinate 2",col=p3$cluster)


continents_and_countries <- as.data.frame(unique(data[c('countriesAndTerritories','continentExp')]))
rownames(continents_and_countries) <- continents_and_countries$countriesAndTerritories

df_tr_table <- as.data.frame.matrix(tr_table)
df_tr_table$countriesAndTerritories <- rownames(df_tr_table)

m<- merge(x = df_tr_table,y = continents_and_countries, by.x='countriesAndTerritories', by.y='countriesAndTerritories', all.x=FALSE, all.y=FALSE)



## ----summaries , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------------------------------------------------
summary(m2)
summary(m3)
summary(m11)


## ----p3_summaries , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------------------------------------------
summary(m3_1)
summary(m3_2)
summary(m3_3)



## ----cluster , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------------------------------------------------
# all cluster 1 
tr_table[tr_table[,1] ==1,]
# all cluster 2
tr_table[tr_table[,2] ==1,]
# all cluster 3 
tr_table[tr_table[,3] ==1,]

m %>% 
  group_by(continentExp) %>% 
  summarise(across(c(2,3,4), list(mean)))


