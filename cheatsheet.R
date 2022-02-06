################################################################################
################################################################################
#----------------------CHEAT SHEET---------------------------------------------#
#*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*#
#####################################LIBRARIES
library(tidyverse) # just import it
library(haven) #to play with SPSS files
library(epiDisplay) #nice frequency tables
library(Hmisc) # useful for data analysis rcorr
library(pastecs) #desc statistics
options(scipen=100) #options to alter desc stats in pastecs
options(digits=3) #options to alter desc stats in pastecs
library(nortest) #Anderson-Darling normality test
library(moments) #skewness
library(reshape2) #transform data
library(qqplotr) #qqplots
library(ggpubr)  #more fancy graphs
library(xlsx) #excel
library(janitor) #adds a total sum row
library(corrplot) #correlation plots
library(rstatix) #cramer's V
library(foreign) #mport data file
library(fBasics) # Basic Statistics
library(lmtest) #dwtest
#---------------------------FUNCTIONs------------------------------------------#
#---------------------------------> Functions to flag Outliers for 1s, 2s and 3s
outlier_flag_3s<-function(x)
{
  hist(x)
  summary(x)
  # using here the z values 
  flag<- ifelse(test = scale(x) > 3 | scale(x)< -3, yes = 1, no = 0)
  # number of outlier
  table(flag)
  # return the vector representing the flag variable
  flag  
}

outlier_flag_2s<-function(x)
{
  hist(x)
  summary(x)
  # using here the z values 
  flag<- ifelse(test = scale(x) > 2 | scale(x)< -2, yes = 1, no = 0)
  # number of outlier
  table(flag)
  # return the vector representing the flag variable
  flag  
}
outlier_flag_1s<-function(x)
{
  hist(x)
  summary(x)
  # using here the z values 
  flag<- ifelse(test = scale(x) > 1 | scale(x)< -1, yes = 1, no = 0)
  # number of outlier
  table(flag)
  # return the vector representing the flag variable
  flag  
}
#recommended application
#x: dataframe
#y: variable
#n: # of standard deviations
#x$y_nsflag<-outlier_flag_ns(x$y) #add flag variable to the dataframe
#head(x)#view new column within the df (optional)
#table(x$y_nsflag) #counts how many variables are outliers and how many not
#length(x$y) #count of rows with outliers
#length(x$y[x$y_nsflag==0]) #count of rows without outliers
#--------------------------------------------------->SIMPLE FUNCTION TO GET MODE
getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#just pass the vector or variable inside
#--------------------------------------------------->DESSCRIBE VARIABLE FUNCTION
describeme <- function(v, breaks = 5, norm = FALSE)
{ #-------------------3S RULE---------------------------------------------------
  #normal distribution 68.27 1s, 95.45 within 2s and 99.73 within 3s
  #unimodal symetric distribution 55.56 1s, 88.90 within 2s and 95.06 within 3s
  #distribution of any shape 0 1s, 75 within 2s and 88.89 within 3s
  
  up_2s <- mean(v, na.rm= TRUE)+2*sd(v, na.rm= TRUE)
  lo_2s <- mean(v, na.rm= TRUE)-2*sd(v, na.rm= TRUE)
  up_3s <- mean(v, na.rm= TRUE)+3*sd(v, na.rm= TRUE)
  lo_3s <- mean(v, na.rm= TRUE)-3*sd(v, na.rm= TRUE)
  print(stat.desc(v,basic=TRUE, desc=TRUE,norm = norm, p=0.95))
  
  cat("\n")
  print(paste("Influentia values: At least 90-95% of the values lie between ",
              signif(lo_2s,4), " and ", signif(up_2s,4)))
  print(paste("95-99% or more of the values lie between ",signif(lo_3s,4),
              " and ", signif(up_3s,4)))
  cat("\n")
  print("Outliers for 2s Rule")
  print("0 = Within 2s, 1 = Outlier")
  flag<- ifelse(test = scale(v) > 2 | scale(v)< -2, yes = 1, no = 0)
  print(table(flag))
  
  cat("\n")
  print("Outliers for 3s Rule")
  print("0 = Within 3s, 1 = Outlier")
  flag<- ifelse(test = scale(v) > 3 | scale(v)< -3, yes = 1, no = 0)
  print(table(flag))
  n <- length(v)
  min <- min(v)
  max <- max(v)
  m<-mean(v, na.rm = TRUE)
  med <- median(v, na.rm = TRUE)
  std<-sqrt(var(v, na.rm = TRUE))
  #varaince-squared sum of the deviation of the values of the mean
  hist(v, density=20, breaks=breaks, prob=TRUE, 
       xlab="Variable", 
       main="normal curve over histogram")
  curve(dnorm(x, mean=m, sd=std), 
        col="darkblue", add=TRUE)
  legend("topright", legend = c(paste("Mean: ",signif(m,5)),
                                paste("Median: ",signif(med,5)),
                                paste("Min: ",signif(min,5)),
                                paste("Max: ",signif(max,5)),
                                paste("Std Dev: ",signif(std,5)),
                                paste("n: ",signif(n,5))),
         title = "Descriptive Stats", title.adj = 0.75,
         lty = 1, lwd = 2, box.lty = 0, bg="transparent")
}
#recommended use
#describeme(x$y, breaks = 10, norm = FALSE)
#-------------------------------------------------------------------------------
#----------------------------USEFUL FUNCTIONS FROM LIBRARIES--------------------
# Before running the code below make sure to replace variables for your own.
#-------------------------------------------------------------------import data
#### import SPSS files
employee <- read_sav("employee_survey.sav")# used for examples
dice <- read_sav("perfect_dice.sav")
#### import Excel files
job <- read.xlsx("job_profile_examples.xlsx", sheetIndex = 1)# used for examples
#### Import CSVs with german decimal marks
healthy <- read_delim("healthy_breakfast_cereals.txt", delim = "\t",
                      locale = locale(decimal_mark = ","))# used for examples
sales <- read_sav("sales.sav")
#using SPSS labels as another column
employee  <- employee  %>% mutate(gender_lbl = as_factor(gender))
employee  <- employee  %>% mutate(motivation_lbl = as_factor(motivation))
#view df in another window
view(job)
#-------------------------------------------------------------recoding & binning
job$gender_dic <- ifelse(job$gender == "m", 1, 0)
x$y <- ifelse(x$y == -1, NA, x$y)
#replace missing values (-1) & transform factor into character for numeric lbls
x$y_label <- ifelse(x$y_label == "-1", NA,as.character(x$y_label))
#Other ecoding and NA identification
x <- x %>% mutate(z = case_when(is.na(y) ~ NA_real_, y=="X" ~ 1  ))
x <- x %>% mutate(y = case_when(is.na(z ) ~ "NA"  , z == 1  ~ "X"))
x$y <- ifelse(x$y == "NA", NA, x$y)
#Most effective for many variables in conjunction with unique
job$type_job_rec <- recode(job$type, "Team Manager"=1,
                           "Office Worker"=2,
                           "blue-collar Worker"=3,
                           "unskilled Worker"=4,
                           "Trainee"=5)
#get uniquevalues of columns
unique(job$type_job)
#Number of distinct values in a column
length(unique(job[["type_job"]]))

#binning and determine categories
#from 3 to 5, from 6 to 7, and from 8 to 10. Use with min and max functions!
y_thresholds<-c(3,5,7,10)
y_labels<-c('3-5','6-7','8-10')
x$y_categories<-cut(x$y,breaks=y_thresholds,labels=y_labels,right=T)

salary.thresholds<-c(0,1500,2500,4000)
salary.labels<-c('low','middle','high')
employee_survey$salary.categories<-cut(employee_survey$salary,
                                       breaks=salary.thresholds,
                                       labels=salary.labels,right=T)
#---------------------------------------------------------Descriptive statistics
#method 1
summary(job) #works better for the whole df
#method 2 describe(x$y) 
#method 3 # My favorite. norm=False for more than 5000 records
stat.desc(job$salary,basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)#use describeme()
#value of quantile until that percentage
quantile(job$salary, 0.95, na.rm = TRUE)
# a quantile defines a particular part of a dataset, e.g. how many values in a
# distribution are above or below a certain limit.
# descriptive statistics for grouped variables.
job %>% group_by(gender) %>% summarise(mean(salary),
                                       median(salary),
                                       getmode(salary))

#Creates frequency table and bar plot for categorical or discrete variables
tab1(job$type_job, cum.percent = TRUE,horiz=FALSE, main='Distrib of variable Y')

#looking at the dispersion of values in a univariable boxplot
boxplot(job$salary)
histogram(job$salary)#use describeme()
#The ultimate function to describe a variable with descriptive stats
describeme(job$salary, breaks = 10, norm = TRUE)

#Flag Outliers and remove them
job$salary_2sflag<-outlier_flag_2s(job$salary) #add flag variable to the dframe
head(job)#view new column within the df (optional)
table(job$salary_2sflag)#counts how many variables are outliers and how many not
length(job$salary) #count of rows with outliers (optional)
length(job$salary[job$salary_2sflag==0]) #count of rows without outliers (opt)
# Create Z score variable and gives you % of observations within n Sts dvs 
# zscore = (variable - mean)/stdev same as scale
job <- job %>% mutate(salary_z = scale(salary))
paste((job %>% dplyr::filter(salary_z > -2 & salary_z < 2) %>%
         nrow())/(job %>% nrow())*100,"% of Salaries are inside the 2s rule")
#without Outliers
describeme(job$salary[job$salary_2sflag==0], breaks = 10, norm = TRUE)
#create another df without the outliers, don't overwrite
#----------------------------------------------Assessing behaviour of a variable
describeme(job$salary, breaks = 10, norm = TRUE)

# left skewed median is larger than the mean
# right skewed Left steep median is smaller than the mean
# if mean and median are not close your are in trouble
skewness(job$salary) #-0.0205 negative skew/leftskewed
#negative skew/leftskewed long tails to the left_join
#positive skew/right skewed right tail longer

kurtosis(job$salary) #-1.12 platykurtic
# < 3 platykurtic less sharper & with thiner tails, neg kurtosis "platy" "broad,
#=~ 3 mesokurtic normal distribution
# > 3 leptokurtic sharper and wide tails, pos Kurtosis   lepto-" means "skinny,
#------------------------------------------------------
# p-value is the probability of having observations at least as extreme as the 
# one we measured (via the samples) if the null hypothesis were true.
# how likely your null hypothesis is

# If the p-value is smaller than the predetermined significance lvl α (uslly 5%)
# so if p-value < 0.05 →H0 is unlikely → we reject the null hypothesis
# If the p-value is greater than or equal to the predetrmned significance lvl α
# (usually 5%) so if p-value ≥ 0.05 →H0 is likely→we do not reject the null hypt

#p-value < 0.05 →H0 is unlikely → we reject the null hypothesis
#p-value ≥ 0.05 →H0 is likely→we fail to reject the null hypt

# How to interprete the results of a statistical test?
# first you need to understand the null hypothesis of the statistical test
# 1.what is the null hypothesis of the statistical test 
# (in this case the chi distribution test)
# in this case it will tell us if the random variable is equally distributed
# 
# 2. you compare the empirical significance that you find in the test result 
# against the given significance. In 99% of the cases -> failure level of 5%
#   
# 3. Compare what you got from the test with the 5% and then you ask to yourself
# "my failure to reject the null hypothesis is 74.9 %"
# it is larger than 5% so i cannot reject it
# the null hypothesis is the the distribution is equally distributed
# so we accept that the dice is probably perfect
# the smaller the significance is the harder is to reject the null hypothesis
#--------------------Unifromity test
####perfect dice
tab1(dice$points, cum.percent = TRUE,
     main='Distribution of times per points in a dice')
dice_chi <- tribble(~value,~observed,~expected,1,54,50,2,45,50,
                    3,52,50,4,48,50,5,44,50,6,57,50)
dice_chi <- dice_chi %>% mutate(chi = ((observed - expected)^2)/expected)
sum(dice_chi$chi)# empirical chi square value is 2.68
#degrees of freedom is 5 =n-1 = 6-1
chisq.test(dice)
#allowing a failure of 5%
#the critical value with a confidence level of 95% is 11.1 
qchisq(.95, df=5)
#we cannot reject the hypothesis and uniformly distribution is assumed
#since 2.68 is lower than 11.1
#H0 : sample was drawn from a uniform population
#Ha : sample was NOT drawn from a uniform population
pchisq(2.68, 5) #0.251 probability of correct decision if we do not reject H0
#Empirical significance
1-pchisq(2.68, 5 ) #0.749 probab of failure is 74.9% if we reject the null hyp
#We cannot reject the null hyphotesis
#The distribution is uniform
# 74.9% is the probability of a failure, therefore i do not reject the hypoth
# If i do not reject the hypothesis i can assume the distrib follows a uniform
# distribution model and the dice is perfect

#--------------------Normality Distribution tests
#QQ Plots
qqnorm(job$salary, pch = 1, frame = FALSE)
qqline(job$salary, col = "steelblue", lwd = 2)
# Detrended Exponential Q-Q plot
ggplot(data = job, mapping = aes(sample = salary)) +
  stat_qq_line( detrend = TRUE) +
  stat_qq_point( detrend = TRUE) +
  labs(x = "Price Theoretical Quantiles", y = "Sample Quantiles")

#Shapiro function in R only works up to 5000 observations
#The null-hypothesis of this test is that the population is normally distributed
#if the p value is less than the chosen alpha level, then the null hypothesis is
# rejected and there is evidence that the data is not normally distributed
shapiro.test(job$salary)
#shapiro.test(x$y[0:5000]) #(not recommended)
ad.test(job$salary)$p.value # Anderson-Darling

# Lilliefors (Kolmogorov-Smirnov) Test for Normality
# KS-tesT (distribution test) null hypothesis is:
# Data is normally distributed
# KS does not work well with small samples
lillie.test(job$salary) #D = 0.08, p-value = 0.00005
ks.test(job$salary, "pnorm") #D = 1, p-value <0.0000000000000002 #not vry recmnd
# This means, there is a 0% of failure if we reject the null hypothesis,
# hence, we reject it. Data does not follow a normally distributed  model.

# Difference between Chi-Square test and KS-Test
# Chi-Square is used to asses if the data is uniformly distributed based on 
# certain paramtrs,while K-STest asses if the data is normlly distributed or not

type_gen <- job %>% dplyr::select(type_job, gender) %>% 
  group_by(type_job, gender) %>% summarise(count = n())
type_gen_piv <- type_gen %>% spread( key = gender, value = count)
type_gen_piv <- type_gen_piv %>% mutate(sum = f+m)
type_gen_piv %>%
  adorn_totals("row")
#Marginal Distribution
# We can work out what is the chance that a randomly selected person in our 
# company will be a Trainee 24 people hold this type of job, and since the 
# total number of employees is 300, we can calculate that 24 / 300 = 0.08,
# so 8% of the employees are Trainees.

#Conditional Distribution
#chance that a man will be a Trainee 10/159 = 6.2%
#-------------------------------------------------------------------Correlations
################################################################################
################################################################################
#          Nominal                  |   Ordinal        |    Metric            #|
#---------------------------------------------------------------------------- #|
#Nominal | Phi and Cramer's         |                  |                      #|
#---------------------------------------------------------------------------- #|
#Ordinal | Phi and Cramer's V       | Spearmen’s Rho   |                      #|
#        | Mann-Whitney test or     |                  |                      #|
#        | Wilcoxon test (only two  |       Gamma      |                      #|
#        | groups or "levels" of X);| Kendall's tau-b  |                      #|
#        |                          | take ties into   |                      #|
#        | Kruskal Wallis test (X   | account          |                      #|
#        | can have more than two   | Kendall's tau-c  |                      #|
#        | groups)                  | ignores ties     |                      #|
#---------------------------------------------------------------------------- #|
#Metric  |        Eta                                  |                      #|
#        | T-test (only two groups or ‘variable levels’| Pearson correlation  #|
#        |    ONEWAY-ANOVA                             | coefficient          #|
#        |(variable can have more than two groups)     |                      #|
#---------------------------------------------------------------------------- #| 
################################################################################
################################################################################
################################################################################
ggplot(employee, aes(x = as_factor(gender), y = salary)) +
  geom_boxplot(aes(fill=as_factor(gender))) +
  geom_point(aes(col=salary))+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Salary by Gender",
       subtitle = "1=female, 2=male",
       caption = "Salary by Gender",
       x = 'Gender',
       y = 'Salary')
# Correlation matrix with significance levels (p-value)
#The funct rcorr() [in Hmisc package] can be used to compute the signf. lvls for 
# pearson and spearman correlations. It returns both the correlation coeff.s and 
# the p-value of the corr for all possible pairs of columns in the data table.
employeenm <-employee %>% dplyr::select(gender,motivation,age,salary)
res2 <- rcorr(as.matrix(employeenm), type = c("pearson","spearman"))
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
corrplot(cor(employeenm), type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Another way
corrplot(cor(employeenm),method = "number",type = "upper") #show only upper side
pairs(employeenm, panel = panel.smooth)
#a bit of data preparation to bin numeric variables
#-------------------------------------------------------------------------------
describeme(employee$salary)
salary_thresholds<-c(0,1500,2500,4000)
salary_labels<-c('low','middle', 'high')
employee$salary_categories<-cut(employee$salary,breaks=salary_thresholds,
                                labels=salary_labels,right=T)
tab1(employee$salary_categories, cum.percent = TRUE,
     main='Distribution of Types of salary')
describeme(employee$age)
age_thresholds<-c(0,27,40,65)
age_labels<-c('young','middle', 'older')
employee$age_categories<-cut(employee$age,breaks=age_thresholds,
                             labels=age_labels,right=T)
tab1(employee$age_categories, cum.percent = TRUE,
     main='Distribution of Age')
#-------------------------------------------------------------------------------
################################################################################
#1) Nominal - Nominal 
# 1.1) Phi: this is a chi-square-based measure of association
# 1.2) Cramer's V
# 2) Nominal-Ordinal# Sames tools as Nominal - Nominal
# Chi-square test of independence
# H0: The two variables are independent.
# If the outcome of the test validates the null hypothesis, we accept that
# there is no dependency. Otherwise, we reject the null hypothesis and say: 
# “variables are dependent”.
#----------------------------------------CONTINGENCY MADE EASY----------------#|
f <- table(employee$gender,employee$salary_categories)                        #|
chis <-  chisq.test(f)                                                        #|
# Degrees of freedom (#rows-1) x (#columns-1)                                 #|
# ‘gender’ 2 and ‘salary cat’ 3 (2-1) * (3-1) = 2                             #|
qchisq(.95, df=2) #critical chi square value  5.99                            #|
chis$statistic    #empirical chi square value 1.08                            #|
chis$p.value      #empirical significance     0.583                           #|
# The probability of a failure is 58.3% if we reject the                      #|
# null hypothesis. The allowed probability of a failure is 5%                 #|
# empirical chi-square value was 1.08, much lower than 5.99.                  #|        
# Hence, the null hypothesis must be accepted.                                #|
#-----------------------------------------------------------------------------#|
# Therefore, I do NOT reject the hypothesis. If I do not reject the           #|
# hypothesis then I can assume that there isn`t a dependency.                 #|
#-----------------------------------------------------------------------------#|
# If chi-square is large, then we can expect that both                        #|
# variables are independent                                                   #|
#-----------------------------------------------------------------------------#|
cramer_v(employee$gender,employee$salary_categories) #0.0326                  #|
# The rule of thumb to interpret Cramer’s V is                                #|
# 0.1 – 0.2 weak dependency                                                   #|
# 0.2 – 0.3 moderate dependency                                               #|
# > 0.3 strong dependency                                                     #|
#-----dependency between variables is nonexistent.                            #|
#-----------------------------------------------------------------------------#|
#------------------------------------------EXTRA------------------------------#|
# Observed values                                                             #|
chis$observed                                                                 #|
# Expected values                                                             #|
round(chis$expected,2)                                                        #|
# Pearson resideuals                                                          #|
round(chis$residuals, 3)                                                      #|
corrplot(chis$residuals, is.cor = FALSE)                                      #|
# Positive residuals are in blue. Positive values in cells specify            #| 
# an attraction (positive association) between the corresponding              #|
# row and column variables.                                                   #|
# Negative residuals are in red. This implies a repulsion (negative           #|
# association) between the corresponding row and column variables.            #|
contrib <- 100*chis$residuals^2/chis$statistic                                #|
round(contrib, 3)                                                             #|
#-----------------------------------------------------------------------------#|
# 3) ordinal-ordinal
# 3.1) Spearman's Rho
employee$age_cat_rec <- recode(employee$age_categories,
                               "young"=1,"middle"=2,"older"=3)
corr <- cor.test(x=employee$age_cat_rec, y=employee$motivation,
                 method = 'spearman') #rho 0.368
#Variables are dependent. Moderate Dependency

# 4) metric and nominal or ordinal
# 4.1) T-test
# An independent 2-group t-test / two sample assuming equal variances.
# The command will be:
t.test(y~x,paired=FALSE,var.equal=TRUE) # where y numeric & x is a binary factor
t.test(y1,y2,paired=FALSE,var.equal=TRUE) # where y1 and y2 are numeric
# Independent 2-group t-test / Two sample assuming unequal variances.
# The command will be:
t.test(y~x,paired=FALSE,var.equal=FALSE) # where y numeric & x is binary factor
t.test(y1,y2,paired=FALSE,var.equal=FALSE) # where y1 and y2 are numeric
#Paired t-test. The command will be:
t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric

t.test(employee$age~employee_survey$gender,
       paired=FALSE,var.equal=FALSE) # where y is numeric & x is a binary factor
# t is the t-test statistic value (t = -0.8),
# df is the degrees of freedom (df= 1005),
# p-value is the significance level of the t-test (p-value = 0.4).
# conf.int is the confidence interval of the correlation coefficient at 95%
# (conf.int = [-2.29  1.02]);
# The difference between the means for the two groups is significantly different
# with higher P value
t.test(employee$salary~employee$gender,paired=FALSE,
       var.equal=FALSE) # where y is numeric & x is a binary factor
#t = -2, df = 996, p-value = 0.08
# The difference betwn the means for the two groups is not significantly diffrnt
# with lesser P value

#Age and motivation don't work bc Motivation has more than two levels, use anova
anova(employee$motivation~employee$age)
# Null hypothesis: the means of the different groups are the same
# Alternative hypothesis: At least one sample mean is not equal to the others.
one.way <- aov(motivation ~ age, employee)
summary(one.way)
# The larger the F value, the more likely it is that the variation caused by 
# the independent variable is real and not due to chance.
# The Pr(>F) column is the p-value of the F-statistic. This shows how likely it 
# is that the F-value calculated from the test would have occurred if the 
# null hypothesis of no difference among group means were true.
# The p-value of the age variable is very low (p < 0.001), so it appears that 
# the age has a real impact on motivation

# 5) Metric-Metric
# 5.1) Pearson's correlation
# Pearson Correlation between 2 variables (Age vs Salary)
cor.test(employee$age, employee$salary, method = "pearson") #cor 0.565 
qplot(x = age, y = salary, data =employee,
      color = gender_lbl,main="Age vs Salary")
#Variables are dependent. Strong Dependency 
#The probability of failure is 0% if we reject the null hypothesis, 
# therefore we assume the variables are dependent
# The critical value 0.565 indicates a strong dependency.
ggscatter(employee, x = "age", y = "salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "age", ylab = "salary")

#------------------------------------------------------------------REGRESSION---
#Ste Useful steps to identify useful models
# a. Identify all scale variables.
# b. Execute Shapiro Test for normal distribution.
# c. Optional: identify and remove outliers
# d. If pass, execute Pearson correlation test.
# e. Rank the stronger correlations in the Pearson test.
# f. Execute linear regression based on the Pearson ranking, select those who 
#    reject the F and T test null hypothesis.
# g. After filtering, rank by R square in descending order to obtain the better 
#    models.
# h. Proof OLS assumption
#-------------------------------------------------------------------------------
# More Useful steps to build a valuable regression model to predict
# a. Analyse frequency distribution (Descriptive statistics).
# b. Analyse outliers.
# c. Create a scatter plot matrix to find interesting patterns.
# d. Measure dependency between variables.
# e. Mark the larger correlations avoiding autocorrelation when using more than 
#    one predictor.
# f. Exclude spurious correlations
# ---------------------------------------------------OLS: ordinary least squares
# This method uses a linear function, and is the sum of the squared distances, 
# parallel to the axis of the dependent variable, between each data point in
# the set and the corresponding point on the regression line
#-----------------------------------------------------------Proof OLS assumption
# Residuals must be normally distributed. QQ plot.
# Non-relation between the std. residuals and independent variable. QQ plot
# Homoscedasticity must exist btwn the residuals & the dependnt variable.QQ plot
#-------------------------------------------------------------------------------
#                       steps to assess a regression model fitted using OLS
#-------------------------------------------------------------------------------
# Problem                                 Steps to assess a model
#-------------------------------------------------------------------------------
# Multicollinearity                   inspection of the correlation matrix
#                                     advanced methods: VIF (Variance Inflation 
#                                     Factor)
#-------------------------------------------------------------------------------
# Outliers in residuals               boxplots
#                                     scatterplot of standardized predicted vs. 
#                                     standardized residuals -> even if a 
#                                     residual is not an outlier, it could be an
#                                     influential case and it could improve the
#                                     model to remove the corresponding case. 
# Normal distribution of residuals    Histogram of the residuals including 
#                                     normal curve
#                                     Q-Q-plot of residuals 
#                                     Kolmogorov-Smirnov (K-S test) or 
#                                     Shapiro-Wilk. 
#                                     if residuals are not normally distributed,
#                                     the quality of the model cannot be 
#                                     assessed. See rule of thumb of 
#                                     RMSE -> RMSE intervals around a prediction
#                                     does not cover 68% or 95 %
#-------------------------------------------------------------------------------
# homoscedasticity of residuals      plot to check for homoscedasticity: 
#                                    standardized residuals ~ standardized 
#                                    predicted values
# linearity of dependency            residual plots
#                                    partial regression plots
#-------------------------------------------------------------------------------
# autocorrelation – especially       casewise plots: residuals ~ each single 
# in case of time series regression  predictor!
#                                    advanced: Durbin-Watson statistics
#                                    See Janssen/Laatz p. 426
#                                    use another model e.g. polynomial instead 
#                                    of linear
#                                    transform variables, e.g. 1st or 2nd 
#                                    differences or log-transformation
#-------------------------------------------------------------------------------
# Describe what is meant by homo-/heteroscedasticity.
# Homoscedasticity refers to equal distribution of the error term (noise) 
# across all values of independent variables. 
# Heteroscedasticity refers to a varying size of the error
# term across values of independent variables
#-------------------------------------------------------------------------------
res2 <- rcorr(as.matrix(sales), type = c("pearson","spearman"))
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
corrplot(cor(sales), type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)
# Another way
corrplot(cor(sales),method = "number",type = "upper") #show only upper side
describeme(sales$price)
describeme(sales$sales)
# for an appropriate regression according to the OLS method it is necessary to 
# have intercept or constant, a slope and an independent variable to get the 
# estimate value of the dependent value
# 𝑦= b0 + b1*X
lm_x <- lm( sales ~ price, sales)
summary(lm_x)
anova(lm_x)
#Ho: all the coeffts are zero. F test fails.You have to start from beginning
lm_x$coefficients#----------------------slope*indep------------------constant
# The regression coefficient for Sales(tb) is -3,532 for “sales” and 254,549 for 
# the constant “price”. This means that for each additional 
# dollar the price will dencrease 3,532 . 
# The parameters are: sales:y=254,549 -3,532 x(price to estimate sales)
ggplot(sales, aes(x = price, y = sales)) + 
  geom_point(alpha = 0.75) + 
  geom_smooth(method="lm", color = 'red')

ggplot(sales, aes(x = price, y = sales)) + 
  geom_point(alpha=0.7) +                           # observed data
  geom_point(aes(x = price, y = lm_x$fitted.values),  # predicted data
             color='red', alpha=0.5) +
  geom_segment(aes(xend = price, yend = lm_x$fitted.values),
               color='red', linetype='dashed', alpha=0.25)

ggplot(sales, aes(x = price, y = sales)) + 
  geom_point(alpha=0.7) +   
  geom_abline(slope = lm_x$coefficients[[2]],
              intercept = lm_x$coefficients[[1]],
              color='red', alpha=0.5)
#Assesing quality of the regression model for Sales
# R-squared is the proportion of variance in the dependent variable that can be
# explained by the independent variable.
#R Square:  0.447. The model predicts 44.7% of volatility of the input variables
# or % of original values that can be reproduced by using the model
#F-statistic: 11.3 on 1 and 14 DF,  p-value: 0.00466  0.5%
# T test  5.01  0.00019 ***
#         -3.36  0.00466 ** .5% 
# the F-test’s null hypothesis was that ALL coefficients are zero. 
# The t-test allows us to check if a SPECIFIC coefficient is zero

#  The F-test asks if all coefficients are zero and the t-test asks if this 
# specific coefficient is zero. Here both questions come together.
# Because we have only one coefficient

# looking at the F Test and T Test Sig. there is a probability of 0,5% 
# failure if we reject the null hypothesis, so, we reject it meaning 
# the correlations coefficients are not zero

#analyzing the residuals!
#Residuals: Is the distance of an observed point from the expected point
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Price x Residuals Plot
plot(lm_x$resid~sales$price[order(sales$price)],
     main="Price x Residuals\nfor Simple Regression",
     xlab="Price", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(lm_x$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(lm_x$resid)
qqline(lm_x$resid)
# Normality tests
shapiro.test(lm_x$resid) #p-value = 0.5
# The null hypothesis is not rejected, hence the data is normally distributed

# Check if residuals are independent
# The Null hypothesis of theDurbin-Watson test:errors are serially UNcorrelated
dwtest(lm_x)$p.value #Test for independence of residuals
# Null Hypothesis: Errors are serially Uncorrelated
# Results: 0.72
# Based on the results, we can not reject the null hypothesis that the errors 
# are serially uncorrelated.

# Distribution of residuals seems to be normally distributed and and errors are
# serially uncorrelated

# predicted values vs standardize residuals: no heteroskedasticity
# Use model to predict values of a df
p <- tribble(~price,53)
predict(lm_x, newdata =p) #67,376 

#Multivariable Regression
healthy
summary(healthy)
healthy$potass[is.na(healthy$potass)] <- mean(healthy$potass, na.rm = TRUE)
# Set the name column as index 
rownames(healthy) <- healthy$name
# Remove the name column from columns list
cereals <- subset(healthy, select = -c(name,mfr,type))
# Create a Correlation Matrix
rcorr(as.matrix(cereals))
# Create only a correlation matrix(without p-values) and show the plot
corrplot(cor(cereals),method = "number",type = "upper") # show only upper side
#Most correlated to build a model to predict rating
# sugars calories fiber protein fat
# Create the multivariate regression model
lm_x <- lm(rating ~ sugars+calories+fiber+protein,cereals)
summary(lm_x) #0.851
lm_x <- lm(rating ~ sugars+fiber+fat,cereals)
summary(lm_x) # 0.861
lm.model<- lm(rating~sugars+calories+fiber+protein ,data=cereals)
# Print the R2 value to check rough goodness of fit
summary(lm.model)

###############-----------------------------------------------PDFs CDFs and IDFs
# The probability density function (PDF) is the probability that a random 
# variable, say X,will take a value exactly equal to x 
# not effective for continuos variables

#Excel functions
#binomial distribution means only two possible outcomes
#BINOM.DIST(number_s,trials,probability_s,cumulative)
#BINOM.DIST(1;10;D8=0.0875;FALSE)
#=1-BINOMDIST(1;10;D8=0.0875;TRUE)
#=NORM.DIST(value,mean,sdev,FALSE) #PDF
#=NORM.DIST(value,mean,sdev,TRUE) #CDF
# =CHISQ.INV(1-significance level, degrees of freedom) 
# =CHISQ.TEST(observed frequency array, expected frequency array)

#------------------------probability Density Function (PDF) 
# dnorm(x=vector, mean, sd) 
dnorm(1500, 3000, 500) #0.00000886

#------------------------Cumulative Distribution Function (CDF) 
# pnorm(q, mean, sd)  
# Gives the area under the standard normal curve to the left of 1.96
pnorm(1.96, 0, 1) # 0.975
pnorm(1.96, 0, 1, lower.tail=FALSE ) #0.025

#------------------------Quantile Function – (IDF)
#helps to obtain the quantile once you know the probability
# pnorm qnorm(p, mean, sd)  
# Gives the value at which the CDF of the standard normal is .975
qnorm(0.975, 0, 1) #i.e. ~1.96
qnorm(0.95,3000,500) #lifetime that will not be exceeded with  a p of 95% :3822
#VaR: With a probability of 95% the expected loss will not exceed -13.51 %

#----------------student t-distributions
#---------------dt(), pt(), qt() and rt()
#pdf_t
plot(dt(seq(-4, 4, by = 2), df = 5), type = "l",
     main = "t-distribution density function example", las=1)
#cdf_t
ji <- pt(c(-2,0,2), df = 5, lower.tail = TRUE)
#idf_t
qt(ji, df = 5, lower.tail = TRUE)

# T-Distribution is normally use when the deviation is unknown. T-Distribution 
# is an estimation of the Normal distribution, and as it is evidenced in the 
# example, the greater the degrees of freedom the closer to a normal 
# distribution shape.

#parameters needed to draw the PDF of a random variable following
# a. normal distribution: Quantile, Mean and Std. Deviation.
# b. student t-distribution: Quantile and Degrees of freedom
# c. Chi-Square distribution: Quantile and Degrees of freedom

#### poisson probability dist
##Poisson Probability only applies to discrete variables
# lambda λ (the average number of events per time interval)
# dpois(x, lambda, log = FALSE) prob density function
# ppois(q, lambda, lower.tail = TRUE, log.p = FALSE) cumulative
# qpois(p, lambda, lower.tail = TRUE, log.p = FALSE) quantil IDF
# rpois(n, lambda)

#---------------------------------------Probability analysis
dpois(c(0:4), lambda=0.55, log = FALSE)
#57.7% probability that students don’t repeat the examination (0)
ppois(c(0:4), lambda=0.55, log = FALSE)
#89.4% probability that students retake at most one examination (0 and 1)
# at leat one examination 100-57.7 = 42.3% (everything but 0
plot(dpois(c(0:4), lambda=0.55), type = "h", lwd = 2, main = "Poisson PDF",
     ylab = "P(X = x)", xlab = "Number of events")
#0.55 is the mean of the whole population and not from the sample

#################################EXTRA##########################################
#----------time series
varsk %>% 
  ggplot( aes(x=Date, y=PxLast)) +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y")+
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2") +
  ggtitle("Evolution of Price Shares")+
  theme(axis.text.x=element_text(angle=60, hjust=1))
#-------------------------------------useful diplyrs and rs
arrange(students, by = desc(grade_statistics))
attributes(organic$visits)
job_profile %>% dplyr::filter(Age < 45) %>% nrow()
job_profile %>% dplyr::filter(Gender == 'm' &
                                Jobtype %in% c("Team Manager","Office Worker"))%>% nrow()
par(mfrow=c(1,2)) # for rbase plots
ggarrange(A, B, labels = c("A", "B"), ncol = 2, nrow = 1)# for 2 ggplots


scaled <- sapply (it_project[,-1],scale)
#rename the z columns
colnames(scaled) <-  paste("z", colnames(scaled),sep = "_")
scaled