help(plot)
?plot
??plot
?plot
a <- c(1,2,10, 11:14)
is.array(a)
is.vector(a)
is.numeric(a)
is.character(a)
min(a)
c<-as.factor("Bangalore")
c
c+1
b<-as.factor(a+1)
b
d<-as.factor(log(a))
d

# easy sequences of numbers
seq1 <- 1:10 ; seq2 <- 1:10-10 ; seq3 <- seq(1,10,1.1)
seq1 ; seq2 ; seq3

# Issues with loading the dataset so we skip te first 2 rows and tell R
# that the resulting first row contains variable names
help(read.csv)
df <- read.csv(file = "API_EN.ATM.CO2E.PC_DS2_en_csv_v2_3470453.csv", skip=3, header=TRUE)
View(df)
# Use df$ to create new variables in the dataframe
df$X2022 <- df$X2018 + 1
df$X2022
is.numeric(df$X1960)
View(df)

# Indexing: viewing elements of vectors or dataframes
df[1]
df[,1]
df[1,]
df[df$Country.Name = "Aruba",]
subset(df, Country.Name == "Aruba")
df<-subset(df, Country.Name == "Aruba", X1987)
df$One <- 1
df$One
df$Country.Name[df$X1980 > 10]
df[df[,1]<10,]
head(df)

#can change variable names
names(df)
names(df)[1] <- "Country"
names(df$X1960) <- "1960"
names(df)

# Listing and deleting objects
ls() # list all objects in memory, but not the variables inside the dataframes
ls.str() # *describes* all objects in memory including all variables in dataframes
ls(pat = "c") # lists all objects in mem whose names contain c
ls(pat="^m") #lists all objects in mem whose names begin with c
test <- 10
rm(test) # rm = remove any object stored in the environment
rm(list = ls(pat="^test")) # remove the listed objects - those whose names begin with test
mode(model3)
length(crimenomiss)

# Now we're going to import datasets from Stata. Need to use one of the following packages
library(foreign)
library(haven)
??haven
# If you want to access help about commands within an installed package, you need
# to use this syntax: 
??foreign::read.dta
# Haven allows for more functionality - e.g. variable labels from stata are retained (not
# a native functionality in R) whereas foreign does not do so. But it stores datasets as
# tibbles rather than dataframes. Ref. tidyverse package and dplyr.

install.packages('wooldridge')
library(wooldridge)
data(wage1)
head(wage1)
summary(wage1) #summarises ALL the variables in the df
mean(wage1$wage) #summarises the chosen variable
sd(wage1$wage)

# stargazer is better than the base summary command at producing sumary stats and
# regression tables. But it requires a bit more care.
install.packages("stargazer")
library(stargazer)
stargazer(wage1$wage) #this gives you a disgusting list of numbers

# the input needs to be a dataframe, not just a variable - so subset the original dataframe. 
# but unless you specify type='text' stargazer's output will be Latex code.
# This is beautiful, but it needs to be exported to a file using out="filepathname.tex".
# This can the be viewed in Latex.
stargazer(subset(wage1,select=c(wage))) 
stargazer(subset(wage1,select=c(wage)), out='test2.tex') 
# to view the summary stats in R itself, need to specify type='text'
stargazer(subset(wage1,select=c(wage)),type='text')

# you can summarise multiple variables
stargazer(subset(wage1,select=c(wage,exper)),type='text') 

# or you can summarise variables for a specified subset of observations, eg.
# only for men or only for women
stargazer(subset(wage1, female==0,select=c(wage)),type='text') 

# You can also export the summary table as html, export it as out='filename.html')
stargazer(subset(wage1, female==1,select=c(wage, exper)),out="Test.html")
stargazer(subset(wage1, female==1,select=c(wage, exper)),out="Test.tex")

# Frequency distribution of a chosen variable
table(wage1$exper)

# can calculate t-test for mean, diff in mean; or correlations bw variables
t.test(wage1$exper)
cor(wage1$educ, wage1$nonwhite)
cor.test(wage1$educ, wage1$nonwhite)
table(wage1$married, wage1$nonwhite,dnn = c("Married","Non-White?"))
# This could be made into a percentage table. Not explained here.

# can get a chosen summary stat, separately for different categories - e.g. avg wage
# for married and unmarried separately:
aggregate(wage~married,data = wage1,FUN = mean)
aggregate(wage~married,data = wage1,FUN = median)
aggregate(wage~married,data = wage1,FUN = var)
aggregate(married~nonwhite,data = wage1,FUN = mean)
aggregate(wage~nonwhite,data = wage1,FUN = mean)

# Plot command allows for a lot of customisation.
plot(wage1$educ, wage1$wage, xlab = "Education", ylab = "Wage", main="Edu-Wage")
# add a line: abline(interept,slope)
abline(mean(wage1$wage),0)

# Histograms are simple - x-axis bins vs frequency
hist(wage1$exper)
# Can either specify the number of x-axis bins or the specific x-axis bins using breaks:
hist(wage1$exper, breaks = c(0,5,25,40,150), col = "lightblue")
hist(wage1$exper, breaks = 15, col = "pink")

# A bar graph needs a frequency table as an input
barplot(table(wage1$exper))
barplot(table(wage1$nonwhite))

plot(density(wage1$exper))

# ggplot allows prettier and more customisable graphs. Not covered.

# Regressions
# Install estimatr for robust s.e.'s
install.packages("estimatr")
library(estimatr)
model1 <- lm_robust(wage~educ+nonwhite, data=wage1)
model2 <- lm(wage~educ+nonwhite, data=wage1)
summary(model1)
# Annoyingly you can't directly use stargazer to get summary of lm_robust - only regular lm
# command without robust s.e.'s. There is a workaround code - see below.
stargazer(model2, type='text')
stargazer(model2, style='aer',out='Reg.tex')

model3 <- lm(wage~educ+I(educ^2)+tenure+female+married, data=wage1)
stargazer(model3,type='text')

model4 <- lm(wage~educ*female+I(educ^3)+tenure+married, data=wage1)
stargazer(model4,type='text')

model5 <- lm(wage~educ+exper+factor(tenure),data=wage1)
stargazer(model5,type='text')

model6 <- lm(wage~educ*female+.,data=wage1)
summary(model6)

model1 <- lm_robust(wage~educ+nonwhite, data=wage1)
summary(model1)
model2 <- lm(wage~educ+nonwhite, data=wage1)
summary(model2)
stargazer(model2,se = starprep(model2),p = starprep(model2),type='text') # But this doesn't allow for F-stats with robust S.E.'s.

install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
install.packages("AER")
library(AER)

stargazer(model2,coeftest(model2,vcovHC),type='text') # Still non-robust F-stat
bptest(model2) # Test for heterogeneity (Breusch-Pagan) rejects null of homo. So we should
# using robust s.e.'s 

# F-test for joint significance (or any joint hypothesis testing) - use linearHypothesis
# Unsure how to impose robust s.e.'s here
linearHypothesis(model2,c("educ=0","nonwhite=0"),type=c("F"))

predictedvalues <- predict(model2)
res <- resid(model2)
mean(res)
plot(res)
plot(wage1$educ,predictedvalues)

predvals2 <- predict(model3)
plot(wage1$educ,predvals2,xlab="Education",ylab="Predicted wage",main='title')
abline(model3)
abline(model2)
summary(model3)

IVstage2 <- ivreg(wage~educ+exper|educ+tenure,data=wage1)
IVstage1 <- lm(exper~educ+tenure,data=wage1)
stargazer(IVstage1,type='text')
stargazer(IVstage2,type='text')
linearHypothesis(IVstage2,"exper=0",type=c("F"))

# Now binary outcome models. Install mfx for marginal effects in probit/logit.
install.packages("mfx")
library(mfx)

# To run a probit or logit command, use the glm function, specifying family=binomial(link="probit"/"logit") for binary data.
probit1 <- glm(married~educ+nonwhite,data=wage1,family = binomial(link="probit"))
# stargazer works as before
stargazer(probit1, type='text')

logit1 <- glm(married~educ+nonwhite,data=wage1,family = binomial(link="logit"))
stargazer(logit1, type='text')

# This is the syntax for computing mfx for a regression which you've modelled and saved
probitmfx(probit1,data=wage1,atmean=TRUE) #atmean=TRUE computes the marginal effects at the sample mean of each covariate
probitmfx(probit1,data=wage1,atmean=FALSE) #atmean=FALSE computes the marginal effects of a covariate for ALL observed values in the sample, and then takes the sample average of those computed marginal effects
logitmfx(logit1,data=wage1,atmean=FALSE)
logitmfx(logit1,data=wage1,atmean=TRUE)

# Moving on to a new dataset to explore panel data methods
# Need plm package to work with BG, WG, FD models
install.packages("plm")
library(plm)

data('crime4') # This is in long form rather than wide form. Nice.
# We need to tell R that this is a panel dataset. in particular we need to specify which variable is the 'individual' and which is the time variable:
crimepanel<- pdata.frame(crime4,index=c("county","year"))

# We can then run different regressions.
# Fixed effects / Within groups toy model - specify model="":
WG <- plm(crmrte~polpc,data=crimepanel,model='within') 
stargazer(WG,type='text')
# Random effects / between groups:
BG <- plm(crmrte~polpc, data=crimepanel,model='random')
stargazer(BG,type='text')  
# First-differences model:
FD <- plm(crmrte~polpc, data=crimepanel, model='fd')
stargazer(FD, type='text')

# Hausman test checks whether the WG and BG models are different (do the assumptions for BG seem valid?)
phtest(WG,BG) # I have forgotten how this test actually works

# You can run WG with lagged dependent variable by putting lag(var) inside the plm model:
WG_lag <- plm(crmrte~lag(polpc),data=crimepanel,model='within')
stargazer(WG_lag,type='text')

# Missing observations:
# Stargazer is useful because it gives you summary stats of all (or a subset of) the variables 
# in a dataset. You can see whether any variables have missing observations.
stargazer(wage1, type='text')
stargazer(crimepanel, type='text')
stargazer(subset(crimepanel, select=c("crmrte")),type='text')

# You could create a new dataset which includes only those observations that have non-missing entries
# for some chosen variable (or all variables)
crimenomiss <- subset(crimepanel,!is.na(clmix))
crimenomiss2 <- subset(crimepanel,complete.cases(crimepanel))

# Simulations and for loops: we could run a MC simulation, generating random data from a chosen
# distribution as many times as we choose. We could then run a regression on each iteration
# of the simulated data, store the N regression coeffs in a dataframe, and then plot their distribution.

results <- data.frame(xcoef = NA, xs.e.= NA)

for (i in 1:1000) {
  epsilon1 <- rnorm(500)
  mu1 <- runif(500, min = -1, max = 1)
  x = epsilon1 + mu1
  y = 1.5*x + epsilon1 + rnorm(500)
  
  ols_sim <- lm(y~x)
  
  results[i,] <- c(coef(ols_sim)[['x']],summary(ols_sim)$coef[2,2])
}
plot(density(results$xcoef))


