## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------------------
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




## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------------------
# reading the data and storing in df
#setwd("School/courses/applied_stats/p2_2")

path = file.path( "ETT_ESM_Study1.sav")
df = read_sav(path)


## ----define_df , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------------
# define columns for analysis and filter only those.
cols_for_analysis<- c('Subject','religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
  'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control',
  'meaning')

df <- df[cols_for_analysis]

#summary(df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls')])
#summary(df[c('valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')])

# plot histogram of observation counts per value of Meaning
p<-ggplot(df, aes(x=meaning)) + geom_histogram() +ggtitle('Histogram of # Obs per Meaning') 
grid.arrange(p,ncol = 1, nrow = 1)

df$ls <- as.numeric(df$ls)
# remove null vals
df_na <- na.omit(df) 



df_cpy <- df_na
df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
  'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')] <- scale(df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
  'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')] )




## ----df_run_models , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------

personality_c <- c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls')
feelings_c <- c('valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')

# define columns for constructed dataframe
all_results_columns <- c("formula","Intercept_estimate"  , "Personality_estimate" ,"Feeling_estimate"  ,   "Interaction_estimate", "Intercept_st_error"  ,
"Personality_st_error" ,"Feeling_st_error"   ,  "Interaction_st_error" ,"Intercept_Tvalue" ,    "Personality_Tvalue"  ,
"Feeling_Tvalue"   ,    "Interaction_Tvalue" )

# initiate df
all_results_df <- data.frame(matrix(ncol = 13, nrow = 0))
all_results_df_p_vals <- data.frame(matrix(ncol = 7, nrow = 0))
all_results_df_na <- data.frame(matrix(ncol = 13, nrow = 0))
all_results_df_p_vals_na <- data.frame(matrix(ncol = 7, nrow = 0))

colnames(all_results_df) <- all_results_columns

# loop through both sets of predictors and run model per pair.
# save information in constructed df
for (c_p in personality_c){
  for (c_f in feelings_c){
  single_mixed = lmer(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s+ (1 | Subject)",c_p,c_f,c_p,c_f)) , data = df)
  
  # get the formula
  formula <-  single_mixed@call[["formula"]]
  
  # get the sumary stats
  summary_single <- summary(single_mixed)$coef
  ss<- as.data.frame(summary_single)
  estimates <- t(ss["Estimate"])
  colnames(estimates) <- c("Intercept_estimate","Personality_estimate","Feeling_estimate","Interaction_estimate")

  st_error  <- t(ss["Std. Error"])
  colnames(st_error) <- c("Intercept_st_error","Personality_st_error","Feeling_st_error","Interaction_st_error")
  
  t_vals    <- t(ss["t value"])
  colnames(t_vals) <- c("Intercept_Tvalue","Personality_Tvalue","Feeling_Tvalue","Interaction_Tvalue")
  
  combined_res <- data.frame(estimates,st_error,t_vals)
  combined_res$formula <- as.character(formula)[3]
  
  all_results_df <- rbind(all_results_df, combined_res)
  
  # run model and get p val
  m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s",c_p,c_f,c_p,c_f))
          ,random=~1|Subject, na.action=na.omit,data=df)
  anov <- anova(m1)
  p_val <- anov$`p-value`[4]
  l_pvals <- length(anov$`p-value`)
  
  # append data from single model to large df with all results  
  combined_res_p_val <- data.frame(c_p,c_f,p_val)
  all_results_df_p_vals <- rbind(all_results_df_p_vals, combined_res_p_val)

}
}

############################
# Lets run this again for the equal sample sized DF (the one where we globally ommited the nulls prior to all analysis)
for (c_p in personality_c){
  for (c_f in feelings_c){
  single_mixed = lmer(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s+ (1 | Subject)",c_p,c_f,c_p,c_f)) , data = df_na)
  
  # get the formula
  formula <-  single_mixed@call[["formula"]]
  
  # get the sumary stats
  summary_single <- summary(single_mixed)$coef
  ss<- as.data.frame(summary_single)
  estimates <- t(ss["Estimate"])
  colnames(estimates) <- c("Intercept_estimate","Personality_estimate","Feeling_estimate","Interaction_estimate")

  st_error  <- t(ss["Std. Error"])
  colnames(st_error) <- c("Intercept_st_error","Personality_st_error","Feeling_st_error","Interaction_st_error")
  
  t_vals    <- t(ss["t value"])
  colnames(t_vals) <- c("Intercept_Tvalue","Personality_Tvalue","Feeling_Tvalue","Interaction_Tvalue")
  
  combined_res <- data.frame(estimates,st_error,t_vals)
  combined_res$formula <- as.character(formula)[3]
  
  all_results_df_na <- rbind(all_results_df_na, combined_res)
  
  # run model and get p val
  m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s",c_p,c_f,c_p,c_f))
          ,random=~1|Subject, na.action=na.omit,data=df)
  anov <- anova(m1)
  p_val <- anov$`p-value`[4]
  l_pvals <- length(anov$`p-value`)
  
  # append data from single model to large df with all results  
  combined_res_p_val_na <- data.frame(c_p,c_f,p_val)
  all_results_df_p_vals_na <- rbind(all_results_df_p_vals_na, combined_res_p_val_na)

}
}



## ----display results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------

# histogram of tvalues of interactions
p1<- ggplot() + aes(all_results_df$Interaction_Tvalue)+ geom_histogram( colour="black", fill="white")+ ggtitle("Histogram of T Value \n of Interactions\n Different sample sizes") +
  xlab("T value") + ylab("# of occurences")


# histogram of tvalues of interactions
p2<- ggplot() + aes(all_results_df_na$Interaction_Tvalue)+ geom_histogram( colour="black", fill="white")+ ggtitle("Histogram of T Value \n of Interactions \n Same sample size") +
  xlab("T value") + ylab("# of occurences")

grid.arrange(p1,p2, ncol = 2, nrow = 1)



# shapiro.test for both sets of T stats
#print(shapiro.test(all_results_df_na$Interaction_Tvalue))
#print(shapiro.test(all_results_df$Interaction_Tvalue))

#cor(all_results_df_na$Interaction_Tvalue,all_results_df$Interaction_Tvalue)

tvals <- cbind(equal_sized = all_results_df_na$Interaction_Tvalue,unequal_sized=all_results_df$Interaction_Tvalue)

p3 <- ggplot(tvals, aes(x=equal_sized, y=unequal_sized)) + geom_point()+ ggtitle("Scatter plot of T statistics - equal sample sized models vs unequal sample sized models") 

# histogram of T differences
x <- all_results_df_na$Interaction_Tvalue - all_results_df$Interaction_Tvalue  
p4<- ggplot() + aes(x)+ geom_histogram( colour="black", fill="white")+ ggtitle("Histogram of T Value differences " ) +
  xlab("T value diff") + ylab("# of occurences")

grid.arrange(p3,p4, ncol = 1, nrow = 2)



## ----tables , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------------

# calculate the adj. p vals
all_results_df_p_vals$hochberg_p_val <-  p.adjust(all_results_df_p_vals$p_val, method = "hochberg")
all_results_df_p_vals$holm_p_val <-  p.adjust(all_results_df_p_vals$p_val, method = "holm")
all_results_df_p_vals$bonferroni_p_val <-  p.adjust(all_results_df_p_vals$p_val, method = "bonferroni")


# filter models which t val camoe out more than 3 absolute val
tmp <- all_results_df[abs(all_results_df$Interaction_Tvalue) > 3, c('formula','Interaction_estimate','Interaction_st_error','Interaction_Tvalue')]

# set formula as index for visualization 
rownames(tmp) <- tmp$formula

tmp<- tmp[c('Interaction_estimate','Interaction_st_error','Interaction_Tvalue')]
pander(tmp)

# filter set of all mdoels with hoch p val less than 0.05
tmp2 <- all_results_df_p_vals[all_results_df_p_vals$hochberg_p_val < 0.05,]

#c('c_p','c_f','p_val','holm_p_val', 'hochberg_p_val', 'bonferroni_p_val')]

# rename columns for visualization
tmp2 <- tmp2 %>% 
  rename(
    Personality = c_p,
    Feeling = c_f
    )

pander(tmp2)



## ----display_results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------
# dive into specific model
c_p <-  'religious'
c_f <- 'selfother'
m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s",c_p,c_f,c_p,c_f))
          ,random=~1|Subject,data=df_na)

mydf<- ggpredict(m1, terms=c('religious[1,2,3,4,5,6,7]','selfother'))

# plot interaction between two predictors
p <- ggplot(mydf, aes(x, predicted, colour = group)) +
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)
  ) +
  scale_x_discrete(breaks = 1:3, labels = get_x_labels(mydf))+
xlab("Religious Score") +
ylab("Meaningfulness")+
 labs(color='Self-vs-Other\n Score') 

grid.arrange(p, ncol = 1, nrow = 1)


# marginal effects of interaction terms
#ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()



## ----Bonferroni , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------


# hist of p vals -raw
p1 <- ggplot() + aes(all_results_df_p_vals$p_val)+ geom_histogram( colour="black", fill="white")+  geom_vline(xintercept = 0.05,linetype = "dashed", colour="#BB0000",size=2)+   ggtitle("Histogram of raw P values") +
  xlab("P value") + ylab("# of occurences")

# hist of p vals bonferroni
p2 <- ggplot() + aes(all_results_df_p_vals$bonferroni_p_val, method = "bonferroni")+ geom_histogram( colour="black", fill="white")+   ggtitle("Bonferroni adj P vals ") +
  xlab("P value") + ylab("# of occurences")

# hist of p vals - holm
p3 <- ggplot() + aes(p.adjust(all_results_df_p_vals$p_val, method = "holm"))+ geom_histogram( colour="black", fill="white")+   ggtitle("Holm adj P vals ") +
  xlab("P value") + ylab("# of occurences")

# hist of p vals - hoch
p4 <- ggplot() + aes(p.adjust(all_results_df_p_vals$p_val, method = "hochberg"))+ geom_histogram( colour="black", fill="white")+   ggtitle("Benjamini-Hochberg P vals") +
  xlab("P value") + ylab("# of occurences")

grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)




## ----pvals_per_group , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------
# box plot of p vals pooled per predictor in personality
p1 <- ggplot(all_results_df_p_vals, aes(x=c_p, y=p_val)) + 
  geom_boxplot() + geom_hline(aes(yintercept = 0.05), colour="#BB0000",size=1)+ ggtitle("P values of personality\n Non equal sample sizes")+ theme(axis.text.x=element_text(angle=45, hjust=1))

# box plot of p vals pooled per predictor in feelings
p2 <- ggplot(all_results_df_p_vals, aes(x=c_f, y=p_val)) + 
  geom_boxplot()+ geom_hline(aes(yintercept = 0.05), colour="#BB0000",size=1)+ ggtitle("P values of feelings\n Non equal sample sizes")+ theme(axis.text.x=element_text(angle=45, hjust=1))



grid.arrange(p1,p2,ncol = 1, nrow = 2)


## ----heatmap , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------------------
# heatmap of p vals between pairs of predictors
ggplot(data = all_results_df_p_vals, aes(x=c_p, y=c_f, fill=p_val)) +geom_tile(color = "white")+
 scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
   midpoint = 0.5, limit = c(0,1), space = "Lab", 
   name="P values") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
    size = 12, hjust = 1))+
 coord_fixed()+ ggtitle("Heatmap of P values  feelings ~ personality interaction")+xlab("Personality") + ylab("Feelings")

#all_results_df[order(all_results_df$Interaction_Tvalue),]


## ----part2 , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------

# find specific test from large df 
formula_<- 'depression + selfother + depression * selfother + (1 | Subject)'

# filter that given test
single_test <- filter(all_results_df, formula == formula_)
single_test <- t(single_test)
colnames(single_test) <- c('test results')

pander(single_test)

# rerun the test for full display of stats
c_p <- "depression"
c_f <- "selfother"
m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s:%s",c_p,c_f,c_p,c_f))
          ,random=~1|Subject,data=df, na.action=na.omit)

s<-summary(m1)
pander(s$tTable,caption = "Summary of meaning ~ depression * self-vs-other ")





## ----test_effect_grouped , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------

# create variable which rounds the depression score to nearest 1
df_cpy$depression_groups <- round_any(df_cpy$depression, 1)  



## ----test_effect_grouped_results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------

#lmer(meaning ~ depression+ selfother + depression*selfother+  (1 | Subject) ,data=df_cpy)

# fit models per depression group

x<- lapply(split(df_cpy,df_cpy$depression_groups),lmer, formula = meaning ~ depression+ selfother + depression*selfother+  (1 | Subject) )


# look at each model independantly
x1 <- x[1]$`1`
s_x_1<- summary(x1)
coefs<- s_x_1$coefficients
#pander(coefs,caption = "Group 1")


x2 <- x[2]$`2`
s_x_2<- summary(x2)
coefs<- s_x_2$coefficients
#pander(coefs,caption = "Group 2")

#anov <- anova(x4)
#p_val <- anov$`p-value`[4]
#l_pvals <- length(anov$`p-value`)
  


sub_group_df <-  df_cpy[df_cpy$depression_groups==2,]
m1 <- lme(as.formula(sprintf( "meaning ~ %s  ",c_f))
          ,random=~1|Subject,data=df_cpy)
anov <- anova(m1)
p_val <- anov$`p-value`[4]




# look at effects
m1<-lmer(meaning ~ depression+ selfother + depression:selfother+  (1 | Subject) ,data=df_na)

mydf<- ggpredict(m1, terms=c('depression[1,2,3]','selfother'))
pander(mydf)


# marginal effects of interaction terms
p<- ggplot(mydf, aes(x, predicted, colour = group)) + 
  geom_line() +
xlab("Depression Score") +
ylab("Meaningfulness")+
 labs(color='Self-vs-Other\n Score')
grid.arrange(p, ncol = 1, nrow = 1)





## ----do_by_effect , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------
# create the representation of the three models with fixed depression scores
Depression_1 <- c(0,0,1,1)
Depression_2 <- c(0,0,1,2)
Depression_3 <- c(0,0,1,3)
# assess the 3 models we created
z<- esticon(m1, rbind(Depression_1,Depression_2,Depression_3),conf.int = FALSE)

pander(z, caption ="Effect of Self-vs-Other as a function of Depression")




## ----appendix , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------------------------------
# full results of all 108 models
colnames(all_results_df)

col_order<- c(,  
"formula","Intercept_estimate"  , "Personality_estimate", "Feeling_estimate"    , "Interaction_estimate",
 "Intercept_st_error"  , "Personality_st_error", "Feeling_st_error"  ,   "Interaction_st_error",
 "Intercept_Tvalue"   ,  "Personality_Tvalue" ,  "Feeling_Tvalue"   ,    "Interaction_Tvalue")
my_data2 <- all_results_df[, c(5, 4, 1, 2, 3)]
pander(all_results_df)




## ----doc,eval=FALSE, echo=TRUE--------------------------------------------------------------------
## ## ----setup, include=FALSE------------------------------------------------------------
## knitr::opts_chunk$set(echo = FALSE)
## 
## 
## ## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------
## # import libraries
## library(rmarkdown)
## library(plyr)
## library(dplyr)
## library(ggplot2)
## library(tidyr)
## library(pivottabler)
## library(gtsummary)
## library(ggpubr)
## library(ggfortify)
## library(cluster)
## library(MASS)
## library(lmtest)
## library(fBasics)
## library(rcompanion)
## library(gridExtra)
## library(cowplot)
## library(kableExtra)
## library(haven)
## library(tidyverse)
## library(rstatix)
## library(ggpubr)
## library(lme4)
## library(reshape2)
## library(kableExtra)
## library(pander)
## library(performance)
## library(pROC)
## library(sqldf)
## library(nlme)
## library(ggeffects)
## library(doBy)
## 
## 
## 
## 
## ## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------
## # reading the data and storing in df
## #setwd("School/courses/applied_stats/p2_2")
## 
## path = file.path( "ETT_ESM_Study1.sav")
## df = read_sav(path)
## 
## 
## ## ----define_df , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------
## # define columns for analysis and filter only those.
## cols_for_analysis<- c('Subject','religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
##   'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control',
##   'meaning')
## 
## df <- df[cols_for_analysis]
## 
## #summary(df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls')])
## #summary(df[c('valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')])
## 
## # plot histogram of observation counts per value of Meaning
## p<-ggplot(df, aes(x=meaning)) + geom_histogram() +ggtitle('Histogram of # Obs per Meaning')
## grid.arrange(p,ncol = 1, nrow = 1)
## 
## # remove null vals
## df_na <- na.omit(df)
## 
## # scale the datasets predictors
## df_na$ls <- as.numeric(df_na$ls)
## df_cpy <- df_na
## df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
##   'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')] <- scale(df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
##   'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')] )
## 
## 
## 
## 
## ## ----df_run_models , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------------
## 
## personality_c <- c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls')
## feelings_c <- c('valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')
## 
## # define columns for constructed dataframe
## all_results_columns <- c("formula","Intercept_estimate"  , "Personality_estimate" ,"Feeling_estimate"  ,   "Interaction_estimate", "Intercept_st_error"  ,
## "Personality_st_error" ,"Feeling_st_error"   ,  "Interaction_st_error" ,"Intercept_Tvalue" ,    "Personality_Tvalue"  ,
## "Feeling_Tvalue"   ,    "Interaction_Tvalue" )
## 
## # initiate df
## all_results_df <- data.frame(matrix(ncol = 13, nrow = 0))
## all_results_df_p_vals <- data.frame(matrix(ncol = 7, nrow = 0))
## list_of_raw_pvals <- c()
## colnames(all_results_df) <- all_results_columns
## 
## # loop through both sets of predictors and run model per pair.
## # save information in constructed df
## for (c_p in personality_c){
##   for (c_f in feelings_c){
##   single_mixed = lmer(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s+ (1 | Subject)",c_p,c_f,c_p,c_f)) , data = df)
## 
##   # get the formula
##   formula <-  single_mixed@call[["formula"]]
## 
##   # get the sumary stats
##   summary_single <- summary(single_mixed)$coef
##   ss<- as.data.frame(summary_single)
##   estimates <- t(ss["Estimate"])
##   colnames(estimates) <- c("Intercept_estimate","Personality_estimate","Feeling_estimate","Interaction_estimate")
## 
##   st_error  <- t(ss["Std. Error"])
##   colnames(st_error) <- c("Intercept_st_error","Personality_st_error","Feeling_st_error","Interaction_st_error")
## 
##   t_vals    <- t(ss["t value"])
##   colnames(t_vals) <- c("Intercept_Tvalue","Personality_Tvalue","Feeling_Tvalue","Interaction_Tvalue")
## 
##   combined_res <- data.frame(estimates,st_error,t_vals)
##   combined_res$formula <- as.character(formula)[3]
## 
##   all_results_df <- rbind(all_results_df, combined_res)
## 
##   # run model and get p val
##   m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s",c_p,c_f,c_p,c_f))
##           ,random=~1|Subject,data=df_na)
##   anov <- anova(m1)
##   p_val <- anov$`p-value`[4]
##   l_pvals <- length(anov$`p-value`)
## 
##   # append data from single model to large df with all results
##   combined_res_p_val <- data.frame(c_p,c_f,p_val)
##   all_results_df_p_vals <- rbind(all_results_df_p_vals, combined_res_p_val)
## 
## }
## }
## 
## 
## 
## 
## 
## ## ----display results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------
## 
## # histogram of tvalues of interactions
## p<- ggplot() + aes(all_results_df$Interaction_Tvalue)+ geom_histogram( colour="black", fill="white")+ ggtitle("Histogram of T Value \n of Interactions") +
##   xlab("T value") + ylab("# of occurences")
## grid.arrange(p, ncol = 1, nrow = 1)
## 
## #summary(all_results_df)
## 
## 
## 
## ## ----tables , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------
## 
## # calculate the adj. p vals
## all_results_df_p_vals$hochberg_p_val <-  p.adjust(all_results_df_p_vals$p_val, method = "hochberg")
## all_results_df_p_vals$holm_p_val <-  p.adjust(all_results_df_p_vals$p_val, method = "holm")
## all_results_df_p_vals$bonferroni_p_val <-  p.adjust(all_results_df_p_vals$p_val, method = "bonferroni")
## 
## 
## # filter models which t val camoe out more than 3 absolute val
## tmp <- all_results_df[abs(all_results_df$Interaction_Tvalue) > 3, c('formula','Interaction_estimate','Interaction_st_error','Interaction_Tvalue')]
## 
## # set formula as index for visualization
## rownames(tmp) <- tmp$formula
## 
## tmp<- tmp[c('Interaction_estimate','Interaction_st_error','Interaction_Tvalue')]
## pander(tmp)
## 
## # filter set of all mdoels with hoch p val less than 0.05
## tmp2 <- all_results_df_p_vals[all_results_df_p_vals$hochberg_p_val < 0.05,]
## 
## #c('c_p','c_f','p_val','holm_p_val', 'hochberg_p_val', 'bonferroni_p_val')]
## 
## # rename columns for visualization
## tmp2 <- tmp2 %>%
##   rename(
##     Personality = c_p,
##     Feeling = c_f
##     )
## 
## pander(tmp2)
## 
## 
## 
## ## ----display_results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------
## # dive into specific model
## c_p <-  'religious'
## c_f <- 'selfother'
## m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s*%s",c_p,c_f,c_p,c_f))
##           ,random=~1|Subject,data=df_na)
## 
## mydf<- ggpredict(m1, terms=c('religious[1,2,3,4,5,6,7]','selfother'))
## 
## # plot interaction between two predictors
## p <- ggplot(mydf, aes(x, predicted, colour = group)) +
##   geom_point(position = position_dodge(.1)) +
##   geom_errorbar(
##     aes(ymin = conf.low, ymax = conf.high),
##     position = position_dodge(.1)
##   ) +
##   scale_x_discrete(breaks = 1:3, labels = get_x_labels(mydf))+
## xlab("Religious Score") +
## ylab("Meaningfulness")+
##  labs(color='Self-vs-Other\n Score')
## 
## grid.arrange(p, ncol = 1, nrow = 1)
## 
## 
## # marginal effects of interaction terms
## #ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()
## 
## 
## 
## ## ----Bonferroni , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------
## 
## 
## # hist of p vals -raw
## p1 <- ggplot() + aes(all_results_df_p_vals$p_val)+ geom_histogram( colour="black", fill="white")+  geom_vline(xintercept = 0.05,linetype = "dashed", colour="#BB0000",size=2)+   ggtitle("Histogram of raw P values") +
##   xlab("P value") + ylab("# of occurences")
## 
## # hist of p vals bonferroni
## p2 <- ggplot() + aes(all_results_df_p_vals$bonferroni_p_val, method = "bonferroni")+ geom_histogram( colour="black", fill="white")+   ggtitle("Bonferroni adj P vals ") +
##   xlab("P value") + ylab("# of occurences")
## 
## # hist of p vals - holm
## p3 <- ggplot() + aes(p.adjust(all_results_df_p_vals$p_val, method = "holm"))+ geom_histogram( colour="black", fill="white")+   ggtitle("Holm adj P vals ") +
##   xlab("P value") + ylab("# of occurences")
## 
## # hist of p vals - hoch
## p4 <- ggplot() + aes(p.adjust(all_results_df_p_vals$p_val, method = "hochberg"))+ geom_histogram( colour="black", fill="white")+   ggtitle("Benjamini-Hochberg P vals") +
##   xlab("P value") + ylab("# of occurences")
## 
## grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
## 
## 
## 
## 
## ## ----pvals_per_group , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------
## # box plot of p vals pooled per predictor in personality
## p1 <- ggplot(all_results_df_p_vals, aes(x=c_p, y=p_val)) +
##   geom_boxplot() + geom_hline(aes(yintercept = 0.05), colour="#BB0000",size=1)
## #+  geom_hline(xintercept = 0.05,linetype = "dashed", colour="#BB0000",size=2)
## 
## # box plot of p vals pooled per predictor in feelings
## p2 <- ggplot(all_results_df_p_vals, aes(x=c_f, y=p_val)) +
##   geom_boxplot()+ geom_hline(aes(yintercept = 0.05), colour="#BB0000",size=1)
## 
## grid.arrange(p1,p2,ncol = 1, nrow = 2)
## 
## 
## ## ----heatmap , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------------------
## # heatmap of p vals between pairs of predictors
## ggplot(data = all_results_df_p_vals, aes(x=c_p, y=c_f, fill=p_val)) +geom_tile(color = "white")+
##  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
##    midpoint = 0.5, limit = c(0,1), space = "Lab",
##    name="P values") +
##   theme_minimal()+
##  theme(axis.text.x = element_text(angle = 45, vjust = 1,
##     size = 12, hjust = 1))+
##  coord_fixed()+ ggtitle("Heatmap of P values  feelings ~ personality interaction")+xlab("Personality") + ylab("Feelings")
## 
## #all_results_df[order(all_results_df$Interaction_Tvalue),]
## 
## 
## ## ----part2 , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------
## 
## # find specific test from large df
## formula_<- 'depression + selfother + depression * selfother + (1 | Subject)'
## 
## # filter that given test
## single_test <- filter(all_results_df, formula == formula_)
## pander(t(single_test))
## 
## # rerun the test for full display of stats
## c_p <- "depression"
## c_f <- "selfother"
## m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s:%s",c_p,c_f,c_p,c_f))
##           ,random=~1|Subject,data=df_na)
## 
## s<-summary(m1)
## pander(s$tTable,caption = "Summary of meaning ~ depression * self-vs-other ")
## 
## 
## 
## 
## 
## ## ----test_effect_grouped , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------
## 
## # create variable which rounds the depression score to nearest 1
## df_cpy$depression_groups <- round_any(df_cpy$depression, 1)
## 
## 
## 
## ## ----test_effect_grouped_results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----
## 
## #lmer(meaning ~ depression+ selfother + depression*selfother+  (1 | Subject) ,data=df_cpy)
## 
## # fit models per depression group
## 
## x<- lapply(split(df_cpy,df_cpy$depression_groups),lmer, formula = meaning ~ depression+ selfother + depression*selfother+  (1 | Subject) )
## 
## 
## # look at each model independantly
## x1 <- x[1]$`1`
## s_x_1<- summary(x1)
## coefs<- s_x_1$coefficients
## #pander(coefs,caption = "Group 1")
## 
## 
## x2 <- x[2]$`2`
## s_x_2<- summary(x2)
## coefs<- s_x_2$coefficients
## #pander(coefs,caption = "Group 2")
## 
## #anov <- anova(x4)
## #p_val <- anov$`p-value`[4]
## #l_pvals <- length(anov$`p-value`)
## 
## 
## 
## sub_group_df <-  df_cpy[df_cpy$depression_groups==2,]
## m1 <- lme(as.formula(sprintf( "meaning ~ %s  ",c_f))
##           ,random=~1|Subject,data=df_cpy)
## anov <- anova(m1)
## p_val <- anov$`p-value`[4]
## 
## 
## 
## 
## # look at effects
## m1<-lmer(meaning ~ depression+ selfother + depression:selfother+  (1 | Subject) ,data=df_na)
## 
## mydf<- ggpredict(m1, terms=c('depression[1,2,3]','selfother'))
## pander(mydf)
## 
## 
## # marginal effects of interaction terms
## p<- ggplot(mydf, aes(x, predicted, colour = group)) +
##   geom_line() +
## xlab("Depression Score") +
## ylab("Meaningfulness")+
##  labs(color='Self-vs-Other\n Score')
## grid.arrange(p, ncol = 1, nrow = 1)
## 
## 
## 
## 
## 
## ## ----do_by_effect , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------
## # create the representation of the three models with fixed depression scores
## 
## Depression_1 <- c(0,0,1,1)
## Depression_2 <- c(0,0,1,2)
## Depression_3 <- c(0,0,1,3)
## # assess the 3 models we created
## 
## z<- esticon(m1, rbind(Depression_1,Depression_2,Depression_3),conf.int = FALSE)
## 
## pander(z, caption ="Effect of Self-vs-Other as a function of Depression")
## 
## 
## 
## 
## ## ----appendix , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------
## # full results of all 108 models
## pander(all_results_df)
## 
## 
## 
## 
## ## ----doc,eval=FALSE, echo=TRUE-------------------------------------------------------
## ## NA
## 
## 

