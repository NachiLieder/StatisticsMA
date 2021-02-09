library(dplyr)
library(ggplot2)
library(tidyr)
library(pivottabler)
library(scales)
library(gtsummary)
library(ggpubr)
library(ggfortify)
library(cluster)
library(MASS)
library(lmtest)
library(fBasics)
library(car)

#############################
#
# Preparing the data
#
###############################
# setting my working directory
setwd("School/courses/applied_stats/p1")

#read csvs
main_m <- read.csv(file = 'main_m.csv')
main_f <- read.csv(file = 'main_f.csv')
scale_m <- read.csv(file = 'scale_m.csv')
scale_f <- read.csv(file = 'scale_f.csv')
colnames(main_m)[1]<- "hhid"
colnames(main_f)[1]<- "hhid"
colnames(scale_m)[1]<- "hhid"
colnames(scale_f)[1]<- "hhid"

# join the data of the males
m_data = inner_join(main_m, scale_m, by = "hhid", copy = FALSE, suffix = c(".x", ".y"))
# join the data of the females
f_data = inner_join(main_f, scale_f, by = "hhid", copy = FALSE, suffix = c(".x", ".y"))           

cat("Dimension for Male DF: ", dim(m_data))
cat("Dimension for Female DF: ", dim(f_data))

df <- rbind(m_data, f_data)
cat("Dimension for General DF: ", dim(df))




############################
# Data Exploration
############################
head(df,10)

cat_data <- factor(c('region','urb','race','origin','povcat','sex.x','emp_status','salt_typ','vt_freq','health','exercise','smk_100','smk_now'))
cols = c('region','urb','race','origin','povcat','sex.x','emp_status','salt_typ','vt_freq','health','exercise','smk_100','smk_now')
df[cols] <- lapply(df[cols], factor)  ## as.factor() could also be used



bool_cat <- factor(c('plan_yn','shop_yn','wic_yn','dt01','dt02','dt03','dt06','dt07'
                     ,'doctor1','doctor2','doctor3','doctor4','doctor5','doctor6','doctor7'))
cols = c('plan_yn','shop_yn','wic_yn','dt01','dt02','dt03','dt06','dt07'
         ,'doctor1','doctor2','doctor3','doctor4','doctor5','doctor6','doctor7')
df[cols] <- lapply(df[cols], factor)  ## as.factor() could also be used



numeric_cat <- factor(c('hhsize','income','pctpov','fs_rcv12','age','grade','d1_tv','bmi_sp'))

not_sure <- factor(c('salt_frq'))

scale_vars <- factor(c('kq2_b','kq2_c','kq2_d','kq2_e','kq2_f','kq2_g',   'kq7','kq33_a','kq33_b','kq34','kq37','kq38','kq39','kq40','kq41','kq42'))           

#count unique values per field
df %>% summarise_all(n_distinct)



# let view region 
# mostly from the South
df$region = factor(df$region)
p <- ggplot(df, aes(x=factor(region))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("Northeast","Midwest","South","West"))


#view urb
# mostly Suburban
df$urb = factor(df$urb)
p <- ggplot(df, aes(x=factor(urb))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("Central","Suburban","Non Metroolitan"))

#view income
# some outliers >100000
ggplot(df, aes(x=income)) + geom_histogram(color="black", fill="lightblue", linetype="dashed")

#view age
# mostly between 40-60
ggplot(df, aes(x=age)) + geom_histogram(color="black", fill="lightblue", linetype="dashed")

#view sex.x
#equal amount of M/F
df$sex.x = factor(df$sex.x)
p <- ggplot(df, aes(x=factor(sex.x))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("Central","Suburban","Non Metroolitan"))

#view race
# vast majority as White
df$race = factor(df$race)
p <- ggplot(df, aes(x=factor(race))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("White","Black","Asian/Pacific","American Indian","Other"))

#view grade
# outlier around 100 , after that avg around 12
ggplot(df, aes(x=grade)) + stat_count(color="black", fill="lightblue", linetype="dashed")
summary(df$grade)


#view exercise
# majority rarely or never
df$exercise = factor(df$exercise)
p <- ggplot(df, aes(x=factor(exercise))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("Daily","5-6_Times_a_week","2-4_times_a_week","Once_a_week","1-3_times_a_week","Rarely/Never"))


#view kq7
df$kq7 = factor(df$kq7)
p <- ggplot(df, aes(x=factor(kq7))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("Overweight","Underweight","About Right","Dont Know","Not Ascertained"))



# 5 diet variables
cols <- c('dt01','dt02','dt03','dt06','dt07')
df[cols] <- lapply(df[cols], factor)  ## as.factor() could also be used


summary(df %>% dplyr::select('dt01','dt02','dt03','dt06','dt07'))
diet_questions <- df %>% dplyr::select('dt01','dt02','dt03','dt06','dt07')
myfreq <- sapply(pv, function(x) table(factor(x, levels=unique(unlist(diet_questions)), ordered=TRUE)))
sweep(myfreq,2,colSums(myfreq),`/`)


#view emp_status
df$emp_status = factor(df$emp_status)
p <- ggplot(df, aes(x=factor(emp_status))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p + scale_x_discrete(labels=c("Employed -full time","Employed - part time","Employed, not at work last week",
                              "employed"))






df_cat_data    <- df %>% select(cat_data)
df_bool_cat    <- df %>% select(bool_cat)
df_numeric_cat <- df %>% select(numeric_cat)
df_not_sure    <- df %>% select(not_sure)


# relationship between sex and region
counts <- table(df$sex.x, df$region)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = c("Male", "Female"),
        names.arg=c("Northeast", "Midwest", "South","West"))


# relationship between sex and urb
counts <- table(df$sex.x, df$urb)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = c("Male", "Female"),
        names.arg=c("Central", "Suburban", "NonMetropolan"))


attach(df)
plot(age, income, main="Age ~ Income",
     xlab="Age ", ylab="Weight", pch=1,
     col=rgb(0,100,0,50,maxColorValue=255))
abline(lm(income~age), col="red") # regression line (y~x)


pairs(~age+income+grade,data=df,
      main="Simple Scatterplot Matrix")

# Correlation amtrix between Boolean survey Questions
cor <-cor(df_bool_cat, method = "pearson", use = "complete.obs")
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor, col=col, symm=TRUE)



####################################
# Deal with missing data
####################################

# df$region[is.na(df$region)]<- "NA"

# df$urb[is.na(df$urb)]<- "NA"

# df$race[is.na(df$race)]<- "NA"

# df$exercise[is.na(df$exercise)]<- "NA"
index <- df$exercise == '9'
df = df[!index,]



# df[df$kq7 %in%  c("4","5","8","9"),] <- "NA"
index <- df$kq7 %in%  c("4","5","8","9")
df = df[!index,]
index <- is.na(df$kq7)
df = df[!index,]

index <- df$grade > 18
df = df[!index,]

# Loop through columns and remove null values
primes_list <- list('dt01','dt02','dt03','dt06','dt07')
for (col in primes_list) {
  df<- df[!is.na(df[col]),]
}

#remove 9 value (NAN) for emp status
index <- df$emp_status %in%  c("9")
df = df[!index,]

# remove for scale vars the not ascertained observations
for (col in scale_vars) {
  df[,col] <- as.factor(as.character(df[,col]))
  index <- c("9") ==  df[col] 
  df = df[!index,]
}

# verify dimension of DF
dim(df)

# df[is.na(df)] <- "NA" 

####################################
# Look at the response parameter
####################################
print(summary(df$bmi_sp))
# distribution of BMI
p <- ggplot(df, aes(x=factor(bmi_sp))) + stat_count(color="black", fill="lightblue", linetype="dashed")
p+geom_density(alpha = .2, fill = "#FF6666")

# QQ plot of BMI
qqnorm(df$bmi_sp, pch = 1, frame = FALSE)
qqline(df$bmi_sp, col = "steelblue", lwd = 2)

# filter tail and look again
filtered_bmi <-  df$bmi_sp[df$bmi_sp < 90]
summary(filtered_bmi)
qqnorm(filtered_bmi, pch = 1, frame = FALSE)
qqline(filtered_bmi, col = "steelblue", lwd = 2)

# filter tail 
df <- df[df$bmi_sp < 90,]

#what do we want to do with the outliers?
# Median and Mean are somewhat close
# what is a normal range for BMI? usually 16-40


y <- df$bmi_sp
ggdensity(y, 
          main = "Density plot of BMI",
          xlab = "BMI Value")



## look at the box cox for the BMI (mayvbe vs the intercept)
Box <- boxcox(y ~ 1)
qqnorm(bc$x, pch = 1, frame = FALSE)
qqline(bc$x, col = "steelblue", lwd = 2)
Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y
Cox2[1,]  
library(rcompanion)
lambda = Cox2[1, "Box.x"]                 # Extract that lambda
T_box = (y ^ lambda - 1)/lambda   # Transform the original data
df$T_box = T_box
plotNormalHistogram(T_box)


#####################################
#
# BMI vs other columns
#
#####################################

y <- df$T_box
# boxplot per region
# deosnt seem to have any differences, ADDED: after removing outlier Y, seems like cat 2 ranges a bit more, cat 4 probably has lower variance.
ggplot(df, aes(x=region, y=T_box)) + 
  geom_boxplot()

# boxplot per race
# 2 and 4 potentially have higher BMI by definition, 1 has long tail (WHITE)
ggplot(df, aes(x=race, y=T_box)) + 
  geom_boxplot()

# boxplot per urb
# no change, ADDED: 1 seems a little more subtle
ggplot(df, aes(x=urb, y=T_box)) + 
  geom_boxplot()

# boxplot per sex
# larger variance with 2
ggplot(df, aes(x=factor(sex.x), y=T_box)) + 
  geom_boxplot()

# scatter plot vs grade
#pretty straight regression line , ADDED: declining a bit as the grade progresses
ggplot(df, aes(x=grade, y=T_box)) + 
  geom_point(shape=18, color="blue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")


# scatter plot vs age
#pretty straight regression line
# this should be similar to grade ...need to verify why not.
ggplot(df, aes(x=age, y=T_box)) + 
  geom_point(shape=18, color="blue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")


# scatter plot vs income
#BMI with slight decreasing relationship
ggplot(df, aes(x=income, y=T_box)) + 
  geom_point(shape=18, color="blue") + 
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")


# boxplot per exercise
# larger variance for NA values
ggplot(df, aes(x=exercise, y=T_box)) + 
  geom_boxplot()

# boxplot per kq7
# Overweight(1) has a higher BMI as expected , with a long right tail , 2 is lower as expected.
ggplot(df, aes(x=factor(kq7), y=T_box)) + 
  geom_boxplot()


# Scale Y data and correlate with binary survey questions
scaled_y <- (y-min(y))/(max(y)-min(y))
scaled_y
apply( df_bool_cat , 2 , cor , y = T_box )


dim(df)

selected_cols = c('region','urb','income','age','sex.x','race','grade','exercise','kq7','dt01',
                  'dt02','dt03','dt06','dt07','emp_status')
selected_df <- df %>% dplyr::select(selected_cols)
selected_df


# plot the kmeans score per different sets of clusters to attempt to charecterize set,
rng<-2:20 #K from 2 to 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) 
  for(i in 1:tries){
    k.temp <-kmeans(selected_df,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

# k <-kmeans(selected_df, centers=5) #Create 5 clusters

#plot pca after kmeans
autoplot(kmeans(selected_df, 5), data = selected_df)

# another method to plot clusters
autoplot(clara(selected_df, 5),frame=TRUE)



##########################################
# Regrress Y vs each column seperately
###########################################


tbl_uv_ex1 <-
  tbl_uvregression(
    selected_df,
    method = glm,
    y = T_box,
    # exponentiate = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 2)
    
  ) %>%
  add_global_p()
tbl_uv_ex1

# Choose columns to filter after univariate runs
columns_filtered_after_uvariate_regs = c('region','income','age','sex.x','race','grade','exercise','dt01','dt02','dt03','dt07',
                                         'emp_status')
uvariate_filtered_df <- selected_df %>% dplyr::select(columns_filtered_after_uvariate_regs)
uvariate_filtered_df


################
# STEPWISE AIC #
################
# without interactions
full.model <- lm(T_box ~., data = uvariate_filtered_df  )

# with interactions
full.model <- lm(T_box ~.^2, data = uvariate_filtered_df  )

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = TRUE)
summary(step.model)

# Looks like there is a bias towards the size of the BMI

model_res = resid(step.model)
fitted <- fitted(step.model)


summary(model_res)
# first view loks fine in terms of normality
hist(model_res, main="Histogram of Residuals",
     ylab="Residuals")

# plot(T_box, model_res, ylab="Residuals", xlab="BMI Score", main="BMI Prediction") 
plot(fitted, model_res, ylab="Residuals", xlab="BMI Score", main="BMI Prediction") 
  

#Q-Q Plot of residuals
qqnorm(model_res)
qqline(model_res)


# additional tests - 
dwtest(step.model) #Test for independence of residuals


# Influence Plot
influencePlot(step.model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# cooks distance
plot(cooks.distance(step.model))


# kmeans on diet questions (plot pca) -- done
# consider kmeans on everything -- done
# do i need to convert each column to factor? -- done
# univariate LR - find top ones -- done
# take top ones and perform stepwise 
# include interactions
# include polinomial
# look at leverage 
# look at resiuals -- done
#


sv = df %>% dplyr::select(scale_vars)
