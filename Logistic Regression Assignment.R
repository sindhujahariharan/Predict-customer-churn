#Six steps of logistic regression
mydata <- read.table(file.choose(), sep = ",", header = T)

attach(mydata)
names(mydata)
#----------------------- Preliminary steps - Data Split

library(caret)
?createDataPartition
mydata.rows<- createDataPartition(y= mydata$Churn, p=0.7, list = FALSE)
mydata.dev<- mydata[mydata.rows,] # 70% data goes in here
table(mydata.dev$Churn)

mydata.Holdout<- mydata[-mydata.rows,] # 30% data goes in here
table(mydata.Holdout$Churn)

#generalized linear model - glm
?glm
logit=glm(Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins,data=mydata.Holdout,family=binomial)
summary(logit)


#-------------------------Step 1 - Overall validity of the model
library(lmtest)
#likelihood ratio test
lrtest(logit)

#-------------------------Step2 - McFadden R2
library(pscl)
pR2(logit)

#-------------------------Step 3 - Individual coeff's significance
summary(logit)


#-------------------------Step 4  - Explanatory power of odds and probability
exp(coef(logit))
exp(coef(logit))/(1+exp(coef(logit)))

#-------------------------Step 5 - confusion matrix
?predict
prediction=predict(logit,type="response")
prediction
#'+' indicates '>='
cutoff=floor(prediction+0.5)
cutoff
table(Actual=mydata.Holdout$Churn,predicted=cutoff)

#------------------------- Step 6 - ROC curve - receier operating charateristic curve

library(Deducer)
rocplot(logit)
