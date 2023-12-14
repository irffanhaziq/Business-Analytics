dataindex = read.csv(file.choose())
str(dataindex)

dataindex$Creditability = as.factor(dataindex$Creditability)
dataindex$Account.Balance = as.factor(dataindex$Account.Balance)
dataindex$Payment.Status.of.Previous.Credit = as.factor(dataindex$Payment.Status.of.Previous.Credit)
dataindex$Occupation = as.factor(dataindex$Occupation)
dataindex$Sex...Marital.Status = as.factor(dataindex$Sex...Marital.Status)
dataindex$Type.of.apartment = as.factor(dataindex$Type.of.apartment)
dataindex$Most.valuable.available.asset = as.factor(dataindex$Most.valuable.available.asset)
dataindex$Telephone = as.factor(dataindex$Telephone)
dataindex$Foreign.Worker = as.factor(dataindex$Foreign.Worker)
dataindex$Purpose = as.factor(dataindex$Purpose)
dataindex$Value.Savings.Stocks = as.factor(dataindex$Value.Savings.Stocks)
dataindex$No.of.Credits.at.this.Bank = as.factor(dataindex$No.of.Credits.at.this.Bank)
dataindex$Guarantors = as.factor(dataindex$Guarantors)
dataindex$Concurrent.Credits = as.factor(dataindex$Concurrent.Credits)
dataindex$Length.of.current.employment =  as.factor(dataindex$Length.of.current.employment)

table(dataindex$Account.Balance)
table(dataindex$Account.Balance)/sum(table(dataindex$Account.Balance))
AccBalance = table(dataindex$Account.Balance)/sum(table(dataindex$Account.Balance))*100
names(AccBalance)[1] ="No Account"
names(AccBalance)[2] ="None"
names(AccBalance)[3] ="Below 200 DM"
names(AccBalance)[4] ="200 DM or Above"
AccBalance

table(dataindex$Payment.Status.of.Previous.Credit)
PaymentStatus = table(dataindex$Payment.Status.of.Previous.Credit)/sum(table(dataindex$Payment.Status.of.Previous.Credit))*100

library(DataCombine)
replacements <- data.frame(from = 4, to = 3)
dataindex <- FindReplace(data = dataindex, Var = "Account.Balance", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Account.Balance)
table(dataindex$Account.Balance)/sum(table(dataindex$Account.Balance))
AccBalance = table(dataindex$Account.Balance)/sum(table(dataindex$Account.Balance))*100
names(AccBalance)[1] ="No Account"
names(AccBalance)[2] ="No Balance"
names(AccBalance)[3] ="Some Balance"

table(dataindex$Payment.Status.of.Previous.Credit)
table(dataindex$Payment.Status.of.Previous.Credit)/sum(table(dataindex$Payment.Status.of.Previous.Credit))
PaymentStatus = table(dataindex$Payment.Status.of.Previous.Credit)/sum(table(dataindex$Payment.Status.of.Previous.Credit))*100

replacements <- data.frame(from = c(0,1), to = 0)
dataindex <- FindReplace(data = dataindex, Var = "Payment.Status.of.Previous.Credit", replacements, from = "from", to = "to", exact = FALSE)
replacements <- data.frame(from = c(3,4), to = 3)
dataindex <- FindReplace(data = dataindex, Var = "Payment.Status.of.Previous.Credit", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Payment.Status.of.Previous.Credit)
table(dataindex$Payment.Status.of.Previous.Credit)/sum(table(dataindex$Payment.Status.of.Previous.Credit))
PaymentStatus = table(dataindex$Payment.Status.of.Previous.Credit)/sum(table(dataindex$Payment.Status.of.Previous.Credit))*100
names(PaymentStatus)[1] ="Some Problems"
names(PaymentStatus)[2] ="Paid Up"
names(PaymentStatus)[3] ="No Problem"


names(dataindex)[7]="Value.Savings.Stocks"
str(dataindex)
names(dataindex)
replacements <- data.frame(from = c(3,4), to = 3)
dataindex <- FindReplace(data = dataindex, Var = "Value.Savings.Stocks", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Value.Savings.Stocks)
table(dataindex$Value.Savings.Stocks)/sum(table(dataindex$Value.Savings.Stocks))
Value.Savings.Stocks = table(dataindex$Value.Savings.Stocks)/sum(table(dataindex$Value.Savings.Stocks))*100

table(dataindex$Length.of.current.employment)
replacements <- data.frame(from = c(1,2), to = 1)
dataindex <- FindReplace(data = dataindex, Var = "Length.of.current.employment", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Length.of.current.employment)
table(dataindex$Length.of.current.employment)/sum(table(dataindex$Length.of.current.employment))
Length.of.current.employment = table(dataindex$Length.of.current.employment)/sum(table(dataindex$Length.of.current.employment))*100

table(dataindex$Sex...Marital.Status)
replacements <- data.frame(from = c(1,2), to = 1)
dataindex <- FindReplace(data = dataindex, Var = "Sex...Marital.Status", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Sex...Marital.Status)
table(dataindex$Sex...Marital.Status)/sum(table(dataindex$Sex...Marital.Status))
Sex...Marital.Status = table(dataindex$Sex...Marital.Status)/sum(table(dataindex$Sex...Marital.Status))*100


table(dataindex$No.of.Credits.at.this.Bank)
replacements <- data.frame(from = c(2,3,4), to = 2)
dataindex <- FindReplace(data = dataindex, Var = "No.of.Credits.at.this.Bank", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$No.of.Credits.at.this.Bank)
table(dataindex$No.of.Credits.at.this.Bank)/sum(table(dataindex$No.of.Credits.at.this.Bank))
No.of.Credits.at.this.Bank = table(dataindex$No.of.Credits.at.this.Bank)/sum(table(dataindex$No.of.Credits.at.this.Bank))*100

table(dataindex$Guarantors)
replacements <- data.frame(from = c(2,3), to = 2)
dataindex <- FindReplace(data = dataindex, Var = "Guarantors", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Guarantors)
table(dataindex$Guarantors)/sum(table(dataindex$Guarantors))
Guarantors = table(dataindex$Guarantors)/sum(table(dataindex$Guarantors))*100

table(dataindex$Concurrent.Credits)
replacements <- data.frame(from = c(1,2), to = 1)
dataindex <- FindReplace(data = dataindex, Var = "Concurrent.Credits", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Concurrent.Credits )
table(dataindex$Concurrent.Credits )/sum(table(dataindex$Concurrent.Credits ))
Concurrent.Credits  = table(dataindex$Concurrent.Credits )/sum(table(dataindex$Concurrent.Credits ))*100

table(dataindex$Purpose)
replacements <- data.frame(from = c(3,4,5,6), to = 3)
dataindex <- FindReplace(data = dataindex, Var = "Purpose", replacements, from = "from", to = "to", exact = FALSE)
replacements <- data.frame(from = c(8,9,10), to = 3)
dataindex <- FindReplace(data = dataindex, Var = "Purpose", replacements, from = "from", to = "to", exact = FALSE)
table(dataindex$Purpose)
table(dataindex$Purpose)/sum(table(dataindex$Purpose ))
Purpose  = table(dataindex$Purpose )/sum(table(dataindex$Purpose ))*100


table(dataindex$Creditability,dataindex$Account.Balance)
table(dataindex$Creditability,dataindex$Payment.Status.of.Previous.Credit)

t.test(dataindex$Duration.of.Credit..month.~dataindex$Creditability)
chisq.test(dataindex$Creditability,dataindex$Account.Balance)

indexs=sample(1:nrow(dataindex),size=0.5*nrow(dataindex))
train50 = dataindex[indexs,]
test50 = dataindex[-indexs,]
names(train50)
str(test50)
LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + 
                         Purpose + Value.Savings.Stocks + Length.of.current.employment + 
                         Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment +
                         Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years.,
                       family=binomial, data = train50)

LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit +
                              Purpose + Length.of.current.employment,family=binomial, data = train50)

fit50 = fitted.values(LogisticModel50final)
Threshold50 = rep(0,500)
for(i in 1:500)
  if(fit50[i]>=0.5 ) Threshold50[i] =1
install.packages('gmodels')
library(gmodels)
CrossTable(train50$Creditability, Threshold50, digits=1, prop.r=F,prop.t=F,prop.chisq=F,chisp=F,data=train50)

install.packages("ROCR")
library(ROCR)
pd = predict(LogisticModel50final,test50)
pred=prediction(pd,test50$Creditability)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

install.packages("tree")
library(tree)



Train50_tree <- tree(Creditability ~ Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+
                       Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+
                       Guarantors+Duration.in.Current.address+Most.valuable.available.asset+Age..years.+Concurrent.Credits+
                       Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone, data=train50, method="class")
summary(Train50_tree)
plot(Train50_tree)
text(Train50_tree,petty=0,cex=0.6)
Test50_pred = predict(Train50_tree,test50,type ="class")
table(Test50_pred, test50$Creditability)


Train50_prune8 = prune.misclass(Train50_tree,best=8)
Test50_prune8_pred = predict(Train50_prune8,test50,type="class")
table(Test50_prune8_pred, test50$Creditability)
plot(Train50_prune8)
