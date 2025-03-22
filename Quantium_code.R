set.seed(69)

install.packages('readr')
install.packages('dplyr')
install.packages("readxl")
install.packages('tidyselect')
install.packages('tidyverse')
install.packages('writexl')
install.packages('VGAM')
install.packages('caret')
install.packages("glmnet")
install.packages('pROC')
install.packages('ROCR')
install.packages('knitr')
install.packages('moments')
install.packages('randomForest')
install.packages('class')
library(readr)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(readxl)
library(writexl)
library(VGAM)
library(caret)
library(glmnet)
library(pROC)
library(ROCR)
library(knitr)
library(moments)
library(randomForest)
library(class)


rate<-0.04
clawback<-read_xlsx("Clawback Data Set.xlsx")
applications<-read_xlsx("Applications Data Set.xlsx")
customers<-read_xlsx("Customers Data Set.xlsx")
customers[,5]<-as.factor(customers$`Q  Factor 1`)
customers[,6]<-as.factor(customers$`Q Factor 2`)
customers[,7]<-as.factor(customers$`Q Factor 3`)

#Calculating clawback amount
clawback<-clawback %>%
  mutate(length=`Account Close Date`- `Account Open Date`)
applications<-applications %>%
  mutate(revenue=`Loan Amount`*rate)

clawback_merge<-merge(clawback,applications)

clawback_merge<-clawback_merge %>%
  mutate(revenue=`Loan Amount`*rate)

sum(applications$revenue)-sum(clawback_merge$revenue)

#Building Multinominal logistic regression

data<-merge(customers,applications)
data_2<-merge(data,clawback,all.x=TRUE)

data_2$`Close Reason`<-replace(data_2$`Close Reason`, is.na(data_2$`Close Reason`), "None")
data_2$length<-replace(data_2$length, is.na(data_2$length), 0)
data_2$`Close Reason`<-as.factor(data_2$`Close Reason`)
data_balance<-upSample(x=data_2[,-19],y=data_2$`Close Reason`,yname="Close Reason")

train.set.1<-sample(nrow(data_balance),0.75*nrow(data_balance))

fit<-vglm(`Close Reason` ~ 
            `Q  Factor 1` + `Q Factor 2` + `Q Factor 3`
          + `Loan Amount`
          + `Home Value`
          + `Annual Income`
          + `Interest Rate (p.a.)`
          + `Term (months)`,
          data=data_balance[train.set.1,],
          family=multinomial)

summary(fit)
plot(fit)

model_1<-predict(fit,newdata=data_balance[train.set.1,],type="response")
model_1<-colnames(model_1)[max.col(model_1)]
model_1<-as.factor(model_1)
data_balance$`Close Reason`[train.set.1]<-as.factor(data_balance$`Close Reason`[train.set.1])
confusionMatrix(model_1,data_balance$`Close Reason`[train.set.1])


test_model_1<-predict(fit,newdata=data_balance[-train.set.1,],type="response")
test_model_1<-colnames(test_model_1)[max.col(test_model_1)]
test_model_1<-as.factor(test_model_1)
data_balance$`Close Reason`[-train.set.1]<-as.factor(data_balance$`Close Reason`[-train.set.1])
confusionMatrix(test_model_1,data_balance$`Close Reason`[-train.set.1])

#RandomForest Algorithm

data_matrix_fit<-model.matrix(`Close Reason` ~ 
                                `Q  Factor 1` + `Q Factor 2` + `Q Factor 3`
                              + `Loan Amount`
                              + `Home Value`
                              + `Annual Income`
                              + `Interest Rate (p.a.)`
                              + `Term (months)`,
                              data=data_balance)[,-1]
data_dummy_fit<-data.frame(data_matrix_fit)
data_dummy_fit["Close Reason"]<-data_balance$`Close Reason`
train.set.2<-sample(nrow(data_dummy_fit),0.75*nrow(data_dummy_fit))

fit_2<-randomForest(`Close Reason`~.,
          data=data_dummy_fit[train.set.2,],
          importance=TRUE,mtry=3,proximity=FALSE)
plot(fit_2)
varImpPlot(fit_2)

model_2<-predict(fit_2,newdata=data_dummy_fit[train.set.2,],type="response")
model_2<-as.factor(model_2)
data_dummy_fit$`Close Reason`[train.set.2]<-as.factor(data_dummy_fit$`Close Reason`[train.set.2])
confusionMatrix(model_2,data_dummy_fit$`Close Reason`[train.set.2])


test_model_2<-predict(fit_2,newdata=data_dummy_fit[-train.set.2,],type="response")
test_model_2<-as.factor(test_model_2)
data_dummy_fit$`Close Reason`[-train.set.2]<-as.factor(data_dummy_fit$`Close Reason`[-train.set.2])
confusionMatrix(test_model_2,data_dummy_fit$`Close Reason`[-train.set.2])


#Simulate Policy Home Loans
profit_1<-c()
profit_2<-c()
system.time(
for (i in 1:5000) {
  id<-c(1:nrow(applications)) #Generate Customer ID
  #Generate Customer Demographics
  customer_demo<-cbind(id,sample(levels(customers$`Q  Factor 1`),length(id),replace=TRUE))
  customer_demo<-cbind(customer_demo,sample(levels(customers$`Q Factor 2`),length(id),replace=TRUE))
  customer_demo<-as.data.frame(cbind(customer_demo,sample(levels(customers$`Q Factor 3`),length(id),replace=TRUE)))
  customer_demo$id<-as.numeric(customer_demo$id)
  
  #Generate Loan Applications
  application_loan<-as.data.frame(cbind(id,home_value=runif(length(id),min(applications$`Home Value`),max(applications$`Home Value`))))
  for (i in 1:length(id)) {
    application_loan$loan_amt[i]<-(1-sample(runif(1,0.1,0.3),1))*application_loan$home_value[i]
  }
  application_loan<-cbind(application_loan,annual_income=runif(length(id),min(applications$`Annual Income`),max(applications$`Annual Income`)))
  application_loan<-cbind(application_loan,interest_rate=runif(length(id),min(applications$`Interest Rate (p.a.)`),max(applications$`Interest Rate (p.a.)`)))
  application_loan<-as.data.frame(cbind(application_loan,term=sample(c(10,15,20,25,30,35,40),length(id),replace=TRUE)))
  
  #Generate Campaign Expenses
  campaignID<-c(1:38)
  marketing<-c("Own marketing","TV campaign","RiskyLending")
  campaign<-sample(marketing,length(campaignID),replace=TRUE)
  admin.exp<-c(450,550,750,850)
  A.exp1<-sample(admin.exp[c(3,4)],length(which(campaign=="Own marketing")),replace=TRUE)
  admin.cost1<-cbind(campaign[which(campaign=="Own marketing")],admin.expense=A.exp1)
  A.exp2<-sample(admin.exp[c(1,2)],length(which(campaign=="RiskyLending")),replace=TRUE)
  admin.cost2<-cbind(campaign[which(campaign=="RiskyLending")],admin.expense=A.exp2)
  admin.cost3<-cbind(campaign[which(campaign=="TV campaign")],admin.expense=rep(1850,length(campaign[which(campaign=="TV campaign")])))

  serv.cost1<-cbind(campaign[which(campaign=="Own marketing")],serv.expense=rep(550,length(campaign[which(campaign=="Own marketing")])))
  serv.cost2<-cbind(campaign[which(campaign=="RiskyLending")],serv.expense=rep(350,length(campaign[which(campaign=="RiskyLending")])))
  serv.cost3<-cbind(campaign[which(campaign=="TV campaign")],serv.expense=rep(1850,length(campaign[which(campaign=="TV campaign")])))

  admin.campaign<-rbind(admin.cost1,admin.cost2,admin.cost3)
  serv.campaign<-rbind(serv.cost1,serv.cost2,serv.cost3)

  campaign_expenses<-as.data.frame(cbind(admin.campaign,serv.campaign)[,-3])
  campaign_expenses$admin.expense<-as.numeric(campaign_expenses$admin.expense)
  campaign_expenses$serv.expense<-as.numeric(campaign_expenses$serv.expense)

  #Model Commission Profitability
  application_loan<-application_loan %>%
    mutate(revenue=loan_amt*0.025)

  data_simulate<-merge(customer_demo,application_loan)

  data_simulate<-data_simulate %>%
    rename(`Q  Factor 1`=V2, `Q Factor 2`=V3, `Q Factor 3`=V4, `Loan Amount`=loan_amt, `Home Value`=home_value,`Annual Income`=annual_income,`Interest Rate (p.a.)`=interest_rate, `Term (months)`=term)

  #Model Clawback Profitability
  data_simulate$`Q  Factor 1`<-as.factor(data_simulate$`Q  Factor 1`)
  data_simulate$`Q Factor 2`<-as.factor(data_simulate$`Q Factor 2`)
  data_simulate$`Q Factor 3`<-as.factor(data_simulate$`Q Factor 3`)
  simulate_matrix_fit<-model.matrix(~ 
                                  `Q  Factor 1` + `Q Factor 2` + `Q Factor 3`
                                + `Loan Amount`
                                + `Home Value`
                                + `Annual Income`
                                + `Interest Rate (p.a.)`
                                + `Term (months)`,
                                data=data_simulate)[,-1]
  simulate_matrix_fit<-data.frame(simulate_matrix_fit)
  model_simulate<-predict(fit_2,newdata=simulate_matrix_fit,type="response")
  
  est_PP_lambda<-nrow(clawback)/12
  Njump<-rpois(1,lambda=est_PP_lambda*18)

  data_simulate<-as.data.frame(cbind(data_simulate,predict=model_simulate))

  clawback_simulate<-data_simulate %>%
    filter(predict=="DEFAULT"|predict=="REFINANCE")
  clawback_simulate<-clawback_simulate[sample(nrow(clawback_simulate),Njump),]
2
  #Compute Profit
  profit_2<-c(profit_2,sum(data_simulate$revenue)-sum(clawback_simulate$revenue,campaign_expenses$admin.expense,campaign_expenses$serv.expense))

})
hist(profit_2)
mean(profit_2)
min(profit_2)
max(profit_2)
sd(profit_2)
skewness(profit_2)
kurtosis(profit_2)
hist(profit_1)
mean(profit_1)
min(profit_1)
max(profit_1)
sd(profit_1)
skewness(profit_1)
kurtosis(profit_1)

write.csv(profit_1, file="Simulation Profitability Before.csv")
write.csv(profit_2, file="Simulation Profitability After.csv")
write.csv(data_2, file="CLEAN_data_set.csv")
write.csv(data_simulate, file="Simulation_data_set.csv")


#Macroeconomic Predictions (Demand for Loans)

loan_commitments<-read.csv("Flow_investor&owner_occupier.csv")
loan_commitments_total<-loan_commitments%>%
  filter(Attribute5=="Number",Attribute4=="Total housing excluding refinancing",
         Table.Name=="owner_occupier_1")
loan_commitments_refinance<-loan_commitments%>%
  filter(Attribute5=="Number",Attribute4=="External refinancing",
         Table.Name=="owner_occupier_1")
write.csv(loan_commitments_total, file="total_loans.csv")
write.csv(loan_commitments_refinance, file="total_refinancing.csv")

macro_loan_data<-read.csv("macroeconomic_data.csv")
train.set.3<-sample(nrow(macro_loan_data),0.75*nrow(macro_loan_data))
loan_fit<-lm(Total.Loans~Cash.Rate+Inflation+Unemployment+Housing.Approvals, data=macro_loan_data[train.set.3,])
summary(loan_fit)
anova(loan_fit)
model_loan<-predict(loan_fit,newdata=macro_loan_data[train.set.3,],type="response")
MSE.loan<-sum(((macro_loan_data$Total.Loans[train.set.3]-model_loan)/macro_loan_data$Total.Loans[train.set.3])^2)/length(model_loan)
predict_loan<-predict(loan_fit,newdata=macro_loan_data[-train.set.3,],type="response")
MSE.predict.loan<-sum(((macro_loan_data$Total.Loans[-train.set.3]-predict_loan)/macro_loan_data$Total.Loans[-train.set.3])^2)/length(predict_loan)

avg.house.aprove<-7
est.cash.rate<-3.6
est.unemployment<-4.7
est.inflation<-3
est.macro.data<-data.frame(Cash.Rate=est.cash.rate,Inflation=est.inflation,Unemployment=est.unemployment,Housing.Approvals=avg.house.aprove)

est.loan<-predict(loan_fit,newdata=est.macro.data,type="response")
est_loan_demand<-(est.loan-mean(macro_loan_data$Total.Loans[15:39]))/mean(macro_loan_data$Total.Loans[15:39])

refinance_fit<-lm(Refinancing~Cash.Rate+Inflation+Unemployment+Housing.Approvals, data=macro_loan_data[train.set.3,])
summary(refinance_fit)
anova(refinance_fit)
model_refinance<-predict(refinance_fit,newdata=macro_loan_data[train.set.3,],type="response")
MSE.refinance<-sum(((macro_loan_data$Refinancing[train.set.3]-model_refinance)/macro_loan_data$Refinancing[train.set.3])^2)/length(model_refinance)
predict_refinance<-predict(refinance_fit,newdata=macro_loan_data[-train.set.3,],type="response")
MSE.predict.refinance<-sum(((macro_loan_data$Refinancing[-train.set.3]-predict_refinance)/macro_loan_data$Refinancing[-train.set.3])^2)/length(predict_refinance)

est.refinance<-predict(refinance_fit,newdata=est.macro.data,type="response")
est_refinance<-(est.refinance-mean(macro_loan_data$Refinancing[15:39]))/mean(macro_loan_data$Total.Loans[15:39])

house_growth<-9.4/100

profit_3<-c()
profit_4<-c()

system.time(
for (i in 1:5000) {
  id<-c(1:runif(1,nrow(applications)*(1+est_loan_demand-MSE.predict.loan),nrow(applications)*(1+est_loan_demand+MSE.predict.loan))) #Generate Customer ID
  #Generate Customer Demographics
  customer_demo<-cbind(id,sample(levels(customers$`Q  Factor 1`),length(id),replace=TRUE))
  customer_demo<-cbind(customer_demo,sample(levels(customers$`Q Factor 2`),length(id),replace=TRUE))
  customer_demo<-as.data.frame(cbind(customer_demo,sample(levels(customers$`Q Factor 3`),length(id),replace=TRUE)))
  customer_demo$id<-as.numeric(customer_demo$id)
  
  #Generate Loan Applications
  application_loan<-as.data.frame(cbind(id,home_value=runif(length(id),min(applications$`Home Value`)*(1+house_growth),max(applications$`Home Value`)*(1+house_growth))))
  for (i in 1:length(id)) {
    application_loan$loan_amt[i]<-(1-sample(runif(1,0.1,0.3),1))*application_loan$home_value[i]
  }
  application_loan<-cbind(application_loan,annual_income=runif(length(id),min(applications$`Annual Income`),max(applications$`Annual Income`)))
  application_loan<-cbind(application_loan,interest_rate=runif(length(id),min(applications$`Interest Rate (p.a.)`),max(applications$`Interest Rate (p.a.)`)))
  application_loan<-as.data.frame(cbind(application_loan,term=sample(c(10,15,20,25,30,35,40),length(id),replace=TRUE)))
  
  #Generate Campaign Expenses
  campaignID<-c(1:38)
  marketing<-c("Own marketing","TV campaign","RiskyLending")
  campaign<-sample(marketing,length(campaignID),replace=TRUE)
  admin.exp<-c(450,550,750,850)
  A.exp1<-sample(admin.exp[c(3,4)],length(which(campaign=="Own marketing")),replace=TRUE)
  admin.cost1<-cbind(campaign[which(campaign=="Own marketing")],admin.expense=A.exp1)
  A.exp2<-sample(admin.exp[c(1,2)],length(which(campaign=="RiskyLending")),replace=TRUE)
  admin.cost2<-cbind(campaign[which(campaign=="RiskyLending")],admin.expense=A.exp2)
  admin.cost3<-cbind(campaign[which(campaign=="TV campaign")],admin.expense=rep(1850,length(campaign[which(campaign=="TV campaign")])))
  
  serv.cost1<-cbind(campaign[which(campaign=="Own marketing")],serv.expense=rep(550,length(campaign[which(campaign=="Own marketing")])))
  serv.cost2<-cbind(campaign[which(campaign=="RiskyLending")],serv.expense=rep(350,length(campaign[which(campaign=="RiskyLending")])))
  serv.cost3<-cbind(campaign[which(campaign=="TV campaign")],serv.expense=rep(1850,length(campaign[which(campaign=="TV campaign")])))
  
  admin.campaign<-rbind(admin.cost1,admin.cost2,admin.cost3)
  serv.campaign<-rbind(serv.cost1,serv.cost2,serv.cost3)
  
  campaign_expenses<-as.data.frame(cbind(admin.campaign,serv.campaign)[,-3])
  campaign_expenses$admin.expense<-as.numeric(campaign_expenses$admin.expense)
  campaign_expenses$serv.expense<-as.numeric(campaign_expenses$serv.expense)
  
  #Model Commission Profitability
  application_loan<-application_loan %>%
    mutate(revenue=loan_amt*0.025)
  
  data_simulate<-merge(customer_demo,application_loan)
  
  data_simulate<-data_simulate %>%
    rename(`Q  Factor 1`=V2, `Q Factor 2`=V3, `Q Factor 3`=V4, `Loan Amount`=loan_amt, `Home Value`=home_value,`Annual Income`=annual_income,`Interest Rate (p.a.)`=interest_rate, `Term (months)`=term)
  
  #Model Clawback Profitability
  data_simulate$`Q  Factor 1`<-as.factor(data_simulate$`Q  Factor 1`)
  data_simulate$`Q Factor 2`<-as.factor(data_simulate$`Q Factor 2`)
  data_simulate$`Q Factor 3`<-as.factor(data_simulate$`Q Factor 3`)
  simulate_matrix_fit<-model.matrix(~ 
                                      `Q  Factor 1` + `Q Factor 2` + `Q Factor 3`
                                    + `Loan Amount`
                                    + `Home Value`
                                    + `Annual Income`
                                    + `Interest Rate (p.a.)`
                                    + `Term (months)`,
                                    data=data_simulate)[,-1]
  simulate_matrix_fit<-data.frame(simulate_matrix_fit)
  model_simulate<-predict(fit_2,newdata=simulate_matrix_fit,type="response")
  
  est_PP_lambda<-runif(1,nrow(clawback)*(1+est_refinance-MSE.predict.refinance),nrow(clawback)*(1+est_refinance+MSE.predict.refinance))/12
  Njump<-rpois(1,lambda=est_PP_lambda*18)
  
  data_simulate<-as.data.frame(cbind(data_simulate,predict=model_simulate))
  
  clawback_simulate<-data_simulate %>%
    filter(predict=="DEFAULT"|predict=="REFINANCE")
  clawback_simulate<-clawback_simulate[sample(nrow(clawback_simulate),Njump, replace=TRUE),]
  
  #Compute Profit
  profit_4<-c(profit_4,sum(data_simulate$revenue)-sum(clawback_simulate$revenue,campaign_expenses$admin.expense,campaign_expenses$serv.expense))
  
})

hist(profit_3)
mean(profit_3)
min(profit_3)
max(profit_3)
sd(profit_3)
skewness(profit_3)
kurtosis(profit_3)
hist(profit_4)
mean(profit_4)
min(profit_4)
max(profit_4)
sd(profit_4)
skewness(profit_4)
kurtosis(profit_4)

write.csv(profit_3, file="Macro Simulation Profit Before.csv")
write.csv(profit_4, file="Macro Simulation Profit After.csv")

macro_1<-read.csv(file="Macro Simulation Profit Before.csv")
macro_2<-read.csv(file="Macro Simulation Profit After.csv")
prof_1<-read.csv(file="Simulation Profitability Before.csv")
prof_2<-read.csv(file="Simulation Profitability After.csv")

(mean(prof_2[,2])-mean(prof_1[,2]))/mean(prof_1[,2])
