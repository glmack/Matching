# matching example PennMed
setwd('/Users/lee/Documents/code/Matching')

# install.packages('tableone')
# install.packages('Matching')

library(tableone)
library(Matching)

# load data
load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))

View(rhc)

?tableone

# subset data set with set of vars
ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1

treatment

data<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,
              age,female,meanbp1,treatment,died)
df <- data.frame(data)

# covariates
xvars <- c("ARF","CHF","Cirr","colcan","Coma","lungcan","MOSF","sepsis",
         "age","female","meanbp1")

# Create "Table 1" to describe baseline characteristics
table1 <- CreateTableOne(vars=xvars, strata="treatment", data=df, test=FALSE)
print(table1, smd=TRUE)

# greedy matching on Mahalanabois distance
greedymatch <- Match(Tr=treatment, M=1, X=df[xvars], replace=FALSE)
matched = df[unlist(greedymatch[c("index.treated, index.control")]), ]


#propensity score matching logistic regression

# fit generalized linear model 
model1 <- glm(treatment~ARF+CHF+Cirr+colcan+Coma+lungcan+MOSF+sepsis+age+female+meanbp1, 
              family=binomial, data=df)
summary(model1)

p_score <- model1$fitted.values
p_score
