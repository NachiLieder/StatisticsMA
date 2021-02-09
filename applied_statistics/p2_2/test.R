#' 
#' ---
#' output:
#'   pdf_document:
#'     toc: no
#' pagetitle: HW2
#' title: "HW2 - Nachi Lieder 314399114"
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

#' 
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



#' 
#' 
#' 
#' # File reading
#' 
## ---- echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------------------
# reading the data and storing in df
#setwd("School/courses/applied_stats/p2_2")

path = file.path( "ETT_ESM_Study1.sav")
df = read_sav(path)

#' 
#' First off we will observe the given dataset and undertstand the different values we have within each column. We can see that there is a difference in the scaling of the different parameters. Lets first address the personality features.
#' We have seen from the previous project the difference in the distributions of the columns of the A,E,C,N,O (big 5) though we knew that the range was similar [ints from 1-7]. In this project we will address additional personality traits and metrics and evaluate them.
#' Need for closure (NFC), Trait self-control (tsc)  , and Life satisfaction (ls) are also ranged from 1-7 though in a float variable. 
#' Self-esteem (se) , Rumination and Depression are have a different range , and therefor we will consider scaling all features in the same range.
#' 
#' Next we will address the variables which indicate current feelings.
#' Here too , we can see different scales per feature , where some have ranges of [-3,3] and others [0,4]. It is worth noting that the features that have a range of [0,4] have many missing values (~3300 which is ~50% of the dataset) and therefor we will consider ignoring these features and / or fitting the dataset by removing all null values.
#' 
#' Last, we will explore the target variable - Meaning (meaningfulness) . This too ranges from [-3,3] and below we can observe the following distribution.
#' Below is the histogram of the values of our target , where we can see that the two most answered scores were values 1 and 2.
#' 
#' We will consider removing the null values at first , reducing the dataframe from 6686 rows to 3211. Though another possible analysis would be to run with the full set, and per pairwise interaction test we perform, only remove the null for that set. This way we can have an optimal sized set per paired interaction dataset.
#' 
#' 
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

# remove null vals
df_na <- na.omit(df) 

# scale the datasets predictors
df_na$ls <- as.numeric(df_na$ls)
df_cpy <- df_na
df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
  'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')] <- scale(df[c('religious','E','A','C','N','O','NFC','tsc','se','rumination','depression','ls',
  'valence','arousal','angry','anxious','disapp','surprise','wanting','selfother','control')] )



#' 
#' # Model Fitting
#' 
#' Next we will fit 108 models in the form of Meaninig ~ Personality + Feeling + Personality x Feeling. This represents both the individual affect each trait (personality and feeling) perform , as well as their interaction.
#' 
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
list_of_raw_pvals <- c()
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
          ,random=~1|Subject,data=df_na)
  anov <- anova(m1)
  p_val <- anov$`p-value`[4]
  l_pvals <- length(anov$`p-value`)
  
  # append data from single model to large df with all results  
  combined_res_p_val <- data.frame(c_p,c_f,p_val)
  all_results_df_p_vals <- rbind(all_results_df_p_vals, combined_res_p_val)

}
}




#' 
#' Lets look a the results. I will address the most interesting findings from the 108 moedls, and append to the end of the document the entire representation of all results.
#' We can see that there are only a very small set of models which contain an interaction effect that can be considered significant with a confidence level of 0.05. In addition to the P val , I decicded to look at the absolute T value of larger than 3.
#' We find that there are 3 models which answer the constraint of the T value , and only 1 of them also answers the threshold of the P value - including the P value constrint for adjusted P values , meaning that also the holm , hochberg and bonferroni are below 0.05.
#' The model which answers this constraint is the interaction between religious and self-vs-other traits.  Below we can see the tables of the following models which answer the given constraints of T values and adjust P values.
#' we can see that there are 2 more models which have quite significantly absolute high T vals , being the interactions between extrovision and being under control , and between life satisfaction and self vs other. 
#' 
#' 
#' In addition we can look at the histogram of the T values of the interactions between the feelings predictors and the personality predictors.
#' 
#' First off lets address the histogram. We can see a normal behavior to the values. Our median is 0.05 which is close enough to 0 , and ranges go from -3 to 3.5. 
#' 
#' We can see that in terms of positive relationships the strongest relationships are the interactions between religious and focusing on other people (self vs others) , and life satifaction along with focusing on other people people. One could intuitively explain  people that thinking of other people gives meaning to others, which is complimented by religious and life satisfaction.
#' 
#' When viewing the two lowest T vals which indicate on negative relationships we see Extraversion along with control and depression along with focusing on others. this would suggest that people with great depression which dont think of others might not score the meaninigfullness as high (which may be due to self centered traits) 
#' 
## ----display results , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------

# histogram of tvalues of interactions
p<- ggplot() + aes(all_results_df$Interaction_Tvalue)+ geom_histogram( colour="black", fill="white")+ ggtitle("Histogram of T Value \n of Interactions") +
  xlab("T value") + ylab("# of occurences")
grid.arrange(p, ncol = 1, nrow = 1)

#summary(all_results_df)


#' 
#' 
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


#' 
#' Lets review the singe significant model that answers all criterias : The model which tests the interaction between religious and self-vs-other traits.
#' The plot below describes the interactions of the two spoken traits , along with the ranges of the given CIs of 95% , where we can see the different plots of self-vs-other groups , vs the progress of the religious score. We can see per group of self-vs-other (a groupoed being a certain score) , that as the score of the religoius increases , we see an increase in the predicted target (meaningfulness).
#' We can see here that for an instance, per the lowest score of the religous ,  the differentiation between the predictions is a bit lower between the different scores of the self-vs-other score. Though per the highest score of religious we can see larger differentiations .
#' Also worth noting that the prediction target for the lower groups is much lower than the higher.
#' 
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


#' 
#' As stated, the t-statistics for fixed effects in the above models have approximately a standard normal distribution. We will now look at the aprox P values  , as well as the Bonferroni-adjusted p-values, Holm-adjusted p-values and Benjamini-Hochberg adjusted p-values for the interaction terms.
#' 
#' 
#' Now lets address the Bonferroni-adjusted p-values as well as the Holm-adjusted p-values and Benjamini-Hochberg p vals.
#' We perform adjustments to the Interaction predictor and view the histograms of the adjusted observations.
#' We can see that most adjustments adjust the pvals to 1 in all 3 different types of adjustments. This is possibly mostly due to the high pvalues and the sample of 108 which we sampled from as well as the size of our sample set.
#' 
#' 
#' Lets address the single lowest P val from the Benjamini-Hochberg adjustment. This individual can easily be observed.The raw P val here is 0.004 and represents the predictor of the interaction between religious and selfother. 
#' Lets assume independence between tests for the sake of the analysis.
#' According to this adjustment we can assume an alpha of 0.05 , giving us only 1 model without rejection vs several models using the original P vals (such as Extraversion:control and religous:arousal) . We can see the same phenomenon with the bonferroni and holm adjustments.
#' 
#' Note: As we know from the definition , each Pval_i_ (BH adjustment ) <= Pval_i_(bonferroni) for each i-th test perfromed .
#' 
## ----Bonferroni , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE----------------------------


# hist of p vals -raw
p1 <- ggplot() + aes(all_results_df_p_vals$p_val)+ geom_histogram( colour="black", fill="white")+   ggtitle("Histogram of raw P values") +
  xlab("P value") + ylab("# of occurences")

# hist of p vals bonferroni
p2 <- ggplot() + aes(all_results_df_p_vals$bonferroni_p_val, method = "bonferroni")+ geom_histogram( colour="black", fill="white")+   ggtitle("Histogram of Bonferroni adjusted P values ") +
  xlab("P value") + ylab("# of occurences")

# hist of p vals - holm
p3 <- ggplot() + aes(p.adjust(all_results_df_p_vals$p_val, method = "holm"))+ geom_histogram( colour="black", fill="white")+   ggtitle("Histogram of Holm adjusted P values ") +
  xlab("P value") + ylab("# of occurences")

# hist of p vals - hoch
p4 <- ggplot() + aes(p.adjust(all_results_df_p_vals$p_val, method = "hochberg"))+ geom_histogram( colour="black", fill="white")+   ggtitle("Histogram of Benjamini-Hochberg P values") +
  xlab("P value") + ylab("# of occurences")

grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)



#' 
#' 
#' 
#' Lets dive in to  the P values per group , and first look at the main distribution of the pooled set of P values. Below we can see the histogram of the P values , along with our designated threshold of 0.05. the majority of P values are very high , and probably indicate on weak interaction predictors in terms of prediction. As we saw above , with the adjustment , most of the non significant p vals get adjusted to 1.
#' 
## ----pvals , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------
# larger plot of pvals - raw with threshold
ggplot() + aes(all_results_df_p_vals$p_val)+ geom_histogram( colour="black", fill="white")+  geom_vline(xintercept = 0.05,linetype = "dashed", colour="#BB0000",size=2)+ ggtitle("Histogram of raw P values - Interactions") +
  xlab("P value") + ylab("# of occurences")





#' 
#' The plot above describes the distribution of P values per personality trait.
#' 
#'  - First off we can see that the "self-control" trait (tsc) has a very low median and low variance, other than a couple outliers. This may indicate on a significant conscensus in the P values and interaction between this trait and all predictor feeilings.
#'  
#'  - Second , lets look at the depression predictor. We can see here a very high median, suggesting that there is no significant phenomenon of an interaction effect between depression vs any  predictor feelings. Its worth noting that there are some P values under depression that might be low enough to suit our confidense level , but the general phenomenon of the interaction between depression and feelings is suggesting no significance.
#'  
#' For now we will focus on those two personality traits since they seem to have the overall lowest P vlaues. These would be the self control predictor (tsc) and the Openness ("O").
#' 
#' Now lets look at the second box plot , of the p values grouped by feelings predictors.
#' 
#' - Here we can see that the feeling of "focus on yourself/others" (selfother) seems to have the most significance across all personalities towards predicting the meaningfullness of current thoughts. The median is the lowest and closest to 0.05 which was set as our benchmark threshold.
#' 
#' - Arousal seems to have the largest variance, which indicates some sort of disagreement between the different interactions of personalities as a predictor towards the meaningfullness of current thoughts.
#' 
#' Most of these predictors have medians close to 0.25 which isnt so confident in its significance.
#' 
## ----pvals_per_group , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-----------------------
# box plot of p vals pooled per predictor in personality
p1 <- ggplot(all_results_df_p_vals, aes(x=c_p, y=p_val)) + 
  geom_boxplot() + geom_hline(aes(yintercept = 0.05), colour="#BB0000",size=1)
#+  geom_hline(xintercept = 0.05,linetype = "dashed", colour="#BB0000",size=2)

# box plot of p vals pooled per predictor in feelings
p2 <- ggplot(all_results_df_p_vals, aes(x=c_f, y=p_val)) + 
  geom_boxplot()+ geom_hline(aes(yintercept = 0.05), colour="#BB0000",size=1)

grid.arrange(p1,p2,ncol = 1, nrow = 2)

#' 
#' To look at these p values more in depth and in individual terms , lets look at the heatmap matrix below which can describe the pvalues more individually and the sole interaction effect between each 2 pairs of personality and feeling predictors.
#' Lets address some of the elements here.
#' 
#' - As we stated before ,  TSC has pretty significant pvalues accross all Feeling predictors. It is has a weaker confidense interacting with the surprise feeling and valence. so generally speaking we can understand that the interaction of self control and surpise doesnt predict significantly the meaniningfullness.
#' 
#' - Lets look at the control feeling. Here we can see a wide range of values , though control interacted with Extraversion has a strong confidence. this makes sense since some traits under extraversion are assertaveness and talkative which compliments the feeling of control. This interaction has the most negative T value indicating a negative relationship between the given interaction and the targeted feature of meaningfullness. In ther words this would suggest that people that have strong extraversion and control traits and personalities have low meaningfullness scores. this is quite interesting since personality as itself and the feeling standalone have positive estimates and positive T values, though the interaction between them is low!
#' 
#' - Anger and Neuroticism interacted have a significantly low P value , which may indicate that may suggest angry neurotic people may be predictive towards meaniningfullness . The combination of the P value and the positive significant T value indicates a positive relationship (angry + neuroticism -> meaningfullness)
#' 
#' - Self other and depression have a very low T value and P value, also indicating a singificant negative relation. We would assume that people that are focused on themselves and feel depressed tend to score meaningfullness with low scores. This is quite interesting. Diving into this interaction , we see that standalone the personality trait is with a negative T val (depression) though the Feeling trait is positive. This may indicate the strong negative relationship between depression and meaniningfullness.
#' 
#'  
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

#' 
#' 
#' 
#' 
#' 
#' # Part 2
#' 
#' Lets assess the specific model of Meaning ~ Depression + Self-vs-Other + Depression:Self-vs-Other.
#' Lets look at first at the raw P val , and notice that it has a value of 0.02 , which relative to our benchmark of alpha 0.05 is low. This indicates on significance of our model. Though the p value of the depression predictor is slightly higher with a value of 0.06.
#' 
#' Next lets observe the T values: 
#' 
#' - We can see that the Personality predictor - Depression. This value is equal to  -1.8 which is acceptably high in its absolute value , with a negative relation to our target variable. This indicates that significantly as the observants rated low scores in their depression question , they rated high in the meaningfulness question , and vice versa. 
#' 
#' - Next lets observe the Feeling T val - Self vs Other. This is equal to 4.39 which is more significant (given the equal scale between the 2) with a positive relationship. So the higher the Self vs Other score - the higher the meaningfullness score.
#' 
#' This is interesting that the two individual predictors have opposite relations with the target.
#' 
#' - Last lets review the interaction T val , where we see a negative value of -2.1 , which too is slightly significant. This would indicate that the interaction of the two traits would have a negative relation.
#' 
#' 
#' Its worth noting the standard error of the personality trait being the highest of the three predictors.
#' Also there is a high negative correlation between the interaction predictor and the feeling predictor (self vs other)
#' 
## ----part2 , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE---------------------------------

# find specific test from large df 
formula_<- 'depression + selfother + depression * selfother + (1 | Subject)'

# filter that given test
single_test <- filter(all_results_df, formula == formula_)
pander(t(single_test))

# rerun the test for full display of stats
c_p <- "depression"
c_f <- "selfother"
m1 <- lme(as.formula(sprintf( "meaning ~ %s + %s  + %s:%s",c_p,c_f,c_p,c_f))
          ,random=~1|Subject,data=df_na)

s<-summary(m1)
pander(s$tTable,caption = "Summary of meaning ~ depression * self-vs-other ")




#' 
#' Now lets look a little deeper into the effect of self-vs-other on meaningfulnessat different depression levels. Below we can read the table of the different confidence intervals relative to the X (depression Score) and our group ( Self vs other). We can also look at the std error per set.
#' Below the table we can see a graphical representation of the trends above, demonstrating the larger negative slopes as the self-vs-other score increases.
#' 
#' 
## ----test_effect_grouped , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE-------------------

# create variable which rounds the depression score to nearest 1
df_cpy$depression_groups <- round_any(df_cpy$depression, 1)  


#' 
#' 
#' Generally speaking we can first address the slopes. On our X axis we can see the different scores of the depression , and the graphs represent the different scores of "self-vs-other" which would be 7 groups representing the disscrete values of [-3,3].
#' We can see from the slopes that as the values are larger for the self-vs-other trait score , we can get a larger negative slope.
#' 
#' We can also see the large variance in the predicted target varaible under depression scores of 1, vs score 3 which is much lower. This 
#' 
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
m1<-lmer(meaning ~ depression+ selfother + depression*selfother+  (1 | Subject) ,data=df_na)

mydf<- ggpredict(m1, terms=c('depression[1,2,3]','selfother'))
pander(mydf)


# marginal effects of interaction terms
p<- ggplot(mydf, aes(x, predicted, colour = group)) + 
  geom_line() +
xlab("Depression Score") +
ylab("Meaningfulness")+
 labs(color='Self-vs-Other\n Score')
grid.arrange(p, ncol = 1, nrow = 1)




#' 
#' Lets review some of the following results we see that running the 
#' 
## ----do_by_effect , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE--------------------------
K <- LE_matrix(m1, at=list(depression=c(1,2,3)))
K<- LE_matrix(m1)
z<- esticon(m1, K)
pander(summary(z))

lambda1 <- c(1, 1, 1, 1)
z<- esticon(m1, lambda1)
z



#' 
#' 
#' # Appendix
#' 
#' Here we have the former representation of all the 108 models.
#' 
#' 
## ----appendix , echo=FALSE , echo=FALSE, warning=FALSE,message=FALSE------------------------------
# full results of all 108 models
pander(all_results_df)



#' 
#' 
## ----doc,eval=FALSE, echo=TRUE--------------------------------------------------------------------
## NA

