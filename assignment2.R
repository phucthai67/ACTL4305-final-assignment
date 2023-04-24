setwd("~/Dropbox/ACTL4305/Assignment part 2")
#packages
library(ggplot2)
library(caret)
library(tidyr)
library(dplyr)
library(corrplot)
library(MASS)
library(tree)
library(randomForest)
library(boot)
dim(gen.data)

dim(test.sev)
gen.data<-read.csv('A2-data.csv')%>%dplyr::select(-Index)
View(gen.data)
sum(gen.data$claim.incurred)/sum(gen.data$exposure)
lapply(gen.data, class)
gen.data$region<-as.factor(gen.data$region)
gen.data$ncd.level<-as.factor(gen.data$ncd.level)

#creating frequency, severity and total loss
gen.data1<-gen.data%>%
  dplyr::select(-year)%>%
  mutate(freq=round(claim.count/exposure,0),
         sev=ifelse(claim.incurred>0,
                    claim.incurred/claim.count,0),
         tot.loss=claim.incurred/exposure)

dim(gen.data1)
View(gen.data)
View(gen.data1)
#determining numeric and categorical features
cat.col<-which(lapply(gen.data1, class)=='factor')
num.col<-which(lapply(gen.data1, class)!='factor')
length(cat.col)+length(num.col)
summary(gen.data1$freq)
#summary plot for frequency, severity and tot.loss
ggplot(train.data)+
  geom_bar(aes(x=claim.count))
ggplot(train.data)+
  geom_density(aes(x=sev))
ggplot(train.data%>%
         filter(sev>0))+
  geom_density(aes(x=sev))
ggplot(gen.data1)+
  geom_density(aes(x=tot.loss))
?geom_bar

sev.req<-which(gen.data1$claim.count>0)
length(sev.req)
sevdata<-gen.data1[sev.req,]

ggplot(data=sevdata)+
  geom_density(aes(x=sev))
gg.sev<-ggplot(test.sev, 
               aes(y=log(sev)))+
  ylab('log of severity')
gg.freq<-ggplot(train.data, 
                aes(y=as.factor(claim.count)))
ggplot(gen.data1)+
  geom_bar(aes(x=claim.count))
gg.tot.loss<-ggplot(gen.data1, 
                    aes(y=ifelse(tot.loss==0,0,log(tot.loss))))
mean(gen.data1$freq)
sd(gen.data1$freq)
summary(as.factor(gen.data1$freq))
gen.data1$freq<-round(gen.data1$freq,0)
?round
gg.tot.loss+
  geom_boxplot(aes(x=as.factor(year)))


sev<-as.numeric(gen.data1$sev)
freq<-as.integer(gen.data1$freq)
tot.loss<-as.numeric(gen.data1$tot.loss)

gen.data1$ncd.level<-as.factor(gen.data1$ncd.level)
gen.data1$region<-as.factor(gen.data1$region)

lapply(gen.data1,class)

gg.freq+
  geom_point(aes(x=exposure))
ggplot(gen.data1)+
  geom_point(aes(x=exposure,y=freq/exposure))
num.col
head(gen.data[,num.col])
corrplot(cor(gen.data1[,num.col]), 
         method ="circle",title='correlation plot',
         mar=c(0,0,1,0)
         )
View(gen.data)
gg.freq<-ggplot(train.data, 
                aes(y=as.factor(claim.count)))
gg.freq+
  geom_boxplot(aes(x=cubic.cent))+
  ylab('claim count')
gg.freq+
  geom_point(aes(x=driver.age))
colnames(gen.data)
gg.freq+
  geom_boxplot(aes(x=as.factor(yrs.licensed)))
gg.freq+
  geom_boxplot(aes(x=cubic.cent))+
  ylab('claim count')
gg.freq+
  geom_boxplot(aes(x=vehicle.value))+
  ylab('claim count')
gg.freq+
  geom_boxplot(aes(x=no.seats))
gg.freq+
  geom_point(aes(x=horse.power))
gg.freq+
  geom_point(aes(x=weight))
gg.freq+
  geom_point(aes(x=cubic.cent))
gg.freq+
  geom_point(aes(x=length))
gg.freq+
  geom_point(aes(x=width))
gg.freq+
  geom_point(aes(x=height))
gg.freq+
  geom_point(aes(x=prior.claims))
gg.freq+
  geom_count(aes(x=business.type))
gg.freq+
  geom_count(aes(x=marital.status))
gg.freq+
  geom_count(aes(x=fuel.type))
gg.freq+
  geom_count(aes(x=ncd.level))
gg.freq+
  geom_count(aes(x=region))
summary(gen.data)
View(gen.data1)





#fitting severity
gg.sev+
  geom_boxplot(aes(x=as.factor(year)))
gg.sev+
  geom_boxplot(aes(x=business.type))
gg.sev+
  geom_boxplot(aes(x=driver.gender))
gg.sev+
  geom_boxplot(aes(x=marital.status))
gg.sev+
  geom_boxplot(aes(x=as.factor(region)))
gg.sev+
  geom_boxplot(aes(x=body.code))
gg.sev+
  geom_boxplot(aes(x=fuel.type))
gg.sev+
  geom_point(aes(exposure))
gg.sev+
  geom_point(aes(driver.age))
gg.sev+
  geom_point(aes(yrs.licensed))
gg.sev+
  geom_boxplot(aes(ncd.level))
num.col[-c(14:18)]
gg.sev+
  geom_point(aes(vehicle.age))
gg.sev+
  geom_point(aes(vehicle.value))
gg.sev+
  geom_point(aes(cubic.cent))



gg.tot.loss+
  geom_boxplot(aes(x=as.factor(year)))


ggplot(data=gen.data1)+
  geom_point(aes(x=height,y=weight))+
  facet_grid(~fuel.type)
ggplot(data=gen.data1)+
  geom_boxplot(aes(x=driver.gender,y=driver.age))+
  facet_grid(~marital.status)



#checking correlation
lapply(gen.data1[,num.col],class)
corrplot(cor(gen.data1[,num.col]), 
         method ="circle",title='correlation plot',
         mar=c(0,0,1,0)
)
cor(gen.data$vehicle.age,gen.data$vehicle.value)
head(gen.data[,13:19])
#creating principle components
#size
View(gen.size.pca1$x)
gen.size.pca1 <- prcomp(train.data[, 12:18],  scale = TRUE)
gen.size_colsnames <- paste0("PCsize", c(1:7))
colnames(gen.size.pca1$x)<-gen.size_colsnames
train.data1<-cbind(train.data[,-c(12:18)],gen.size.pca1$x[,1:3])

gen.pca<-rep(NA,nrow(gen.data1)*3)
dim(gen.pca)<-c(nrow(gen.data1),3)
for(j in 1:3){
for(i in 1:nrow(gen.data1)){
 gen.pca[i,j]<-sum(gen.size.pca1$rotation[,j]* (gen.data1[i,12:18]-
     gen.size.pca1$center)/gen.size.pca1$scale )
}
} 
colnames(gen.pca)<-gen.size_colsnames[1:3]
gen.data3<-cbind(gen.data1[,-c(12:18)],gen.pca)

#age
gen.age.pca1 <- prcomp(train.data[, 10:11],  scale = TRUE)
gen.age_colsnames <- paste0("PCage", c(1:2))
colnames(gen.age.pca1$x)<-gen.age_colsnames
gen.pca.age<-rep(NA,nrow(gen.data1))
  for(i in 1:nrow(gen.data1)){
    gen.pca.age[i]<-sum(gen.age.pca1$rotation[,1]* (gen.data1[i,10:11]-
                          gen.age.pca1$center)/gen.age.pca1$scale )
  }
gen.data3<-cbind(gen.data3[,-c(10:11)],PCage1=gen.pca.age)


#second pca
View(train.data)
gen.size.pca <- prcomp(gen.data[, 13:19],  scale = TRUE)
head(gen.data[,13:19])
gen.age.pca <- prcomp(gen.data[, 11:12],  scale = TRUE)
gen.size_colsnames <- paste0("PCsize", c(1:7))
gen.age_colsnames <- paste0("PCage", c(1:2))
colnames(gen.size.pca$x)<-gen.size_colsnames
colnames(gen.age.pca$x)<-gen.age_colsnames



gen.size.sum<-summary(gen.size.pca)
plot(gen.size.sum$importance[2,],type='l')

gen.data2<-cbind(gen.data1,gen.size.pca$x[,1:3],PCage1=gen.age.pca$x[,1])

head(gen.data2[,12:18])
gen.data3<-gen.data2[,-c(10:11,12:18)]
View(gen.data3)
dim(train.data1)
summary(sev.modelgam1.1)
sevdata<-gen.data1[sev.req,]
sevdata1<-gen.data3[sev.req,]
lapply(gen.data1,class)

View(gen.data1%>%
  group_by(region)%>%
  summarize(mean.freq=mean(claim.count),
            
            count=n())%>%
  filter(count>500&count<1000))
region.analysis<-train.data1%>%
       group_by(region)%>%
       summarize(mean.freq=mean(claim.count),
                 count=n())
View(region.analysis%>%filter(count>1000))
bodycode.analysis<-train.data1%>%
  group_by(body.code)%>%
  summarize(mean.freq=mean(claim.count),
            sd.freq=sd(claim.count),
            count=n())
marital.analysis<-train.data1%>%
  group_by(marital.status)%>%
  summarize(mean.freq=mean(claim.count),
            sd.freq=sd(claim.count),
            count=n())
ncd.analysis<-train.data1%>%
  group_by(ncd.level)%>%
  summarize(mean.freq=mean(claim.count),
            sd.freq=sd(claim.count),
            count=n())
View(marital.analysis)
###Model fitting
#partitioning data
set.seed(1)
index <- createDataPartition(gen.data$year, p = 0.75, list = FALSE)
train.data <- gen.data1[index, ]
test.data <- gen.data1[-index, ]

train.data1 <- gen.data3[index, ]
test.data1 <- gen.data3[-index, ]


region1<-ifelse(gen.data1$region%in%c(1,32),'group1',
                ifelse(gen.data1$region %in% c(4,3),'group2',
                 ifelse(gen.data1$region %in% c(16,34,31),'group3',      
                  ifelse(gen.data1$region %in%  c(30,11),'group4', 
                   ifelse(gen.data1$region %in%  c(21,38,33), 'group5',
                   ifelse(gen.data1$region %in%  c(5,13),'group6',
                    ifelse(gen.data1$region %in%  c(14,12,23,37,19), 'group7',
               ifelse(gen.data1$region %in%  c(36,6),'group8',gen.data1$region
                                                          ))))))))
region1<-as.factor(region1)

gen.data11<-cbind(gen.data1[,-8],region1)
gen.data31<-cbind(gen.data3[,-8],region1)

train.data11 <- gen.data11[index, ]
test.data11 <- gen.data11[-index, ]
train.data31 <- gen.data31[index, ]
test.data31 <- gen.data31[-index, ]
#fitting claim count

#negbin, no offset
countmodel.nb<-glm.nb(claim.count~.-freq-sev-tot.loss-claim.incurred,
                     data=train.data)
countmodel.nb1<-glm.nb(claim.count~.-freq-sev-tot.loss-claim.incurred,
                      data=train.data1)
summary(countmodel.nb1)
dim(train.data)
AIC(countmodel.nb1)
BIC(countmodel.nb)



#negbin, with offset
countmodel.nb.off<-glm.nb(claim.count~.+offset(log(exposure))-exposure-freq-
                        sev-tot.loss-claim.incurred, data=train.data)

summary(countmodel.nb.off1)
summary(train.data1)
View(train.data1)
countmodel.nb.off1<-glm.nb(claim.count~.+offset(log(exposure))-exposure-freq-
                        sev-tot.loss-claim.incurred, data=train.data1)
AIC(countmodel.nb.off1)
countmodel.none<-glm.nb(claim.count~1,data=train.data)
summary(countmodel.nb.off1)


#diagnostic check for models
summary(countmodel.nb1)
AIC(countmodel.nb.off1)
BIC(countmodel.nb.off1)
anova.count.nb.off<-anova(countmodel.nb.off)
anova.count.nb.off1<-anova(countmodel.nb.off1)

#comparing AIC, BIC
View(train.data1)
tab1<-cbind(rbind(AIC(countmodel.nb),AIC(countmodel.nb1),
                        AIC(countmodel.nb.off),AIC(countmodel.nb.off1)),
                  rbind(BIC(countmodel.nb),BIC(countmodel.nb1),
                        BIC(countmodel.nb.off),BIC(countmodel.nb.off1)))
colnames(tab1)<-c('AIC','BIC')
rownames(tab1)<-c('Negative binomial with no PC','Negative binomial with PC',
                  'Negative binomial with no PC (with offset)','Negative binomial with PC (with offset)')

k=5
set.seed(1)
P1<-cv.glm(train.data,countmodel.nb,K=k)
P2<-cv.glm(train.data1,countmodel.nb1,K=k)
P3<-cv.glm(train.data,countmodel.nb.off,K=k)
P4<-cv.glm(train.data1,countmodel.nb.off1,K=k)
cv.error1<-c(P1$delta[2],P2$delta[2],P3$delta[2],P4$delta[2])
pred.nb<-predict(countmodel.nb,test.data,type='response')
pred.nb1<-predict(countmodel.nb1,test.data1,type='response')
pred.nb.off<-predict(countmodel.nb.off,test.data,type='response')
pred.nb.off1<-predict(countmodel.nb.off1,test.data1,type='response')
test.MSE<-c(RMSE(test.data$claim.count,pred.nb),
            RMSE(test.data$claim.count,pred.nb1),
            RMSE(test.data$claim.count,pred.nb.off),
            RMSE(test.data$claim.count,pred.nb.off1))
cbind(tab1,cv.error1,test.MSE)
#plots of residuals
plot(countmodel.nb.off$fitted.values,rstandard(countmodel.nb.off),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off)) 
qqline(rstandard(countmodel.nb.off),col="red")
View(tab8)
plot(countmodel.nb$fitted.values,rstandard(countmodel.nb),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb)) 
qqline(rstandard(countmodel.nb),col="red")

plot(countmodel.nb.off1$fitted.values,rstandard(countmodel.nb.off1),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off1)) 
qqline(rstandard(countmodel.nb.off1),col="red")

plot(countmodel.nb1$fitted.values,rstandard(countmodel.nb1),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb1)) 
qqline(rstandard(countmodel.nb1),col="red")


 




pred.nb<-predict(countmodel.nb,test.data,type='response')
pred.nb1<-predict(countmodel.nb1,test.data1,type='response')
pred.nb.off<-predict(countmodel.nb.off,test.data,type='response')
pred.nb.off1<-predict(countmodel.nb.off1,test.data1,type='response')
test.MSE<-c(RMSE(test.data$claim.count,pred.nb),
            RMSE(test.data$claim.count,pred.nb1),
            RMSE(test.data$claim.count,pred.nb.off),
            RMSE(test.data$claim.count,pred.nb.off1))

pred.train.nb<-predict(countmodel.nb,train.data,type='response')
pred.train.nb1<-predict(countmodel.nb1,train.data1,type='response')
pred.train.nb.off<-predict(countmodel.nb.off,train.data,type='response')
pred.train.nb.off1<-predict(countmodel.nb.off1,train.data1,type='response')
train.MSE<-c(RMSE(train.data$claim.count,pred.train.nb),
            RMSE(train.data$claim.count,pred.train.nb1),
            RMSE(train.data$claim.count,pred.train.nb.off),
            RMSE(train.data$claim.count,pred.train.nb.off1))

summary(countmodel.nb.off)
sum(abs(countmodel.nb1$coefficients)>0.1)
tab1<-cbind(tab1,cv.error1,test.MSE)
tab1



#########REMOVE fuel type from this point on
#checking for interaction effect
count.personal<-glm.nb(claim.count~(marital.status+driver.gender+driver.age)^2,
                      data=train.data1)

count.vehicle1<-glm.nb(claim.count~(PCage1+
                              PCsize1+PCsize2+PCsize3+fuel.type)^2,
                      data=train.data1)
count.vehicle<-glm.nb(claim.count~(vehicle.age+vehicle.value+no.seats+cubic.cent
                                      +horse.power+weight+fuel.type
                                    +length+width+height)^2,
                       data=train.data)
summary(count.vehicle)
summary(train.data$body.code)
summary(count.vehicle1)
summary(count.personal)
View(train.data1)
summary(countmodel.nb.off)
#refit with some variables removed to data
countmodel.none<-glm.nb(claim.count~1,data=train.data)
step.choose<-stepAIC(countmodel.nb.off,
                     direction = "backward",
                     k = 2,
                     scope = list(upper = countmodel.nb.off, lower = countmodel.none)
)

#remove fuel.type,length,height vs remove using stepAIC
summary(countmodel.nb.off)
countmodel.nb.off2.1<-glm.nb(claim.count~.+offset(log(exposure))-exposure-freq-sev-
                             tot.loss-claim.incurred-length-height-fuel.type,data=train.data)

countmodel.nb.off2.2<-glm.nb(claim.count ~ business.type + driver.age + driver.gender + 
  marital.status + yrs.licensed + ncd.level + region + body.code + 
  vehicle.age + vehicle.value + no.seats + horse.power + weight + 
  width + prior.claims + offset(log(exposure)),data=train.data)
summary(countmodel.nb.off2.1)

#collapsing categorical variable for the above 2 models
countmodel.nb.off2.3<-glm.nb(claim.count~.+offset(log(exposure))-exposure-
                               freq-sev-tot.loss-claim.incurred-length-height-
                           fuel.type,data=train.data11)
countmodel.nb.off2.4<-glm.nb(claim.count~business.type+driver.age+
                               driver.gender+yrs.licensed+ncd.level+
                               region1+vehicle.age+vehicle.value 
                               +no.seats+horse.power+weight+width+prior.claims 
                             + offset(log(exposure)),data=train.data11)
summary(countmodel.nb.off2.2)
tab4
#fit comparison
pred.nb.off2.1<-predict(countmodel.nb.off2.1,test.data,type='response')
pred.nb.off2.2<-predict(countmodel.nb.off2.2,test.data,type='response')
pred.nb.off2.3<-predict(countmodel.nb.off2.3,test.data11,type='response')
pred.nb.off2.4<-predict(countmodel.nb.off2.4,test.data11,type='response')
test.MSE1<-c(RMSE(test.data1$claim.count,pred.nb.off2.1),
             RMSE(test.data1$claim.count,pred.nb.off2.2),
             RMSE(test.data11$claim.count,pred.nb.off2.3),
             RMSE(test.data11$claim.count,pred.nb.off2.4))

pred.train.nb.off2.1<-predict(countmodel.nb.off2.1,train.data,type='response')
pred.train.nb.off2.2<-predict(countmodel.nb.off2.2,train.data,type='response')
pred.train.nb.off2.3<-predict(countmodel.nb.off2.3,train.data11,type='response')
pred.train.nb.off2.4<-predict(countmodel.nb.off2.4,train.data11,type='response')
train.MSE1<-c(RMSE(train.data$claim.count,pred.train.nb.off2.1),
              RMSE(train.data$claim.count,pred.train.nb.off2.2),
              RMSE(train.data11$claim.count,pred.train.nb.off2.3),
              RMSE(train.data11$claim.count,pred.train.nb.off2.4))

k=5
set.seed(1)
P5<-cv.glm(train.data,countmodel.nb.off2.1,K=k)
P6<-cv.glm(train.data,countmodel.nb.off2.2,K=k)
P7<-cv.glm(train.data11,countmodel.nb.off2.3,K=k)
P8<-cv.glm(train.data11,countmodel.nb.off2.4,K=k)
cv.error2<-c(P5$delta[2],P6$delta[2],P7$delta[2],P8$delta[2])


tab2<-cbind(c(AIC(countmodel.nb.off),
      AIC(countmodel.nb.off2.1),
      AIC(countmodel.nb.off2.2),
      AIC(countmodel.nb.off2.3),
      AIC(countmodel.nb.off2.4)),
      c(BIC(countmodel.nb.off),
      BIC(countmodel.nb.off2.1),
      BIC(countmodel.nb.off2.2),
      BIC(countmodel.nb.off2.3),
      BIC(countmodel.nb.off2.4)))
colnames(tab2)<-c('AIC','BIC')
rownames(tab2)<-c('model with offset (1)',
                  'unimportant variables with |coef|>0.1 removed (2)',
                  'variables chosen via stepAIC (3)',
                  'model (2) with collapsed categorical variables (4)',
                  'model (3) with collapsed categorical variable (5)')
tab2

plot(countmodel.nb.off2.1$fitted.values,rstandard(countmodel.nb.off2.1),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off2.1)) 
qqline(rstandard(countmodel.nb.off2.1),col="red")

plot(countmodel.nb.off2.2$fitted.values,rstandard(countmodel.nb.off2.2),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off2.2)) 
qqline(rstandard(countmodel.nb.off2.2),col="red")

plot(countmodel.nb.off2.3$fitted.values,rstandard(countmodel.nb.off2.3),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off2.3)) 
qqline(rstandard(countmodel.nb.off2.3),col="red")

plot(countmodel.nb.off2.4$fitted.values,rstandard(countmodel.nb.off2.4),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off2.4)) 
qqline(rstandard(countmodel.nb.off2.4),col="red")
#models including interaction effect
summary(count.personal)
countmodel.nb.off3.1<-glm.nb(claim.count~.+offset(log(exposure))-exposure-
                               freq-sev-tot.loss-claim.incurred-length-height-
                           fuel.type+weight:height+cubic.cent:weight,
                         data=train.data)
countmodel.nb.off3.2<-glm.nb(claim.count~business.type + driver.age + driver.gender + marital.status + 
                               yrs.licensed + ncd.level + region + body.code + vehicle.age + 
                               vehicle.value + no.seats + horse.power + weight + width + 
                               prior.claims + offset(log(exposure))+
                               weight:height+cubic.cent:weight,
                         data=train.data)
#collapsing categorical variable for the above 2 models
countmodel.nb.off3.3<-glm.nb(claim.count~.+offset(log(exposure))-exposure-
                               freq-sev-tot.loss-claim.incurred-length-height-
                               fuel.type+height:width+cubic.cent:weight,
                             data=train.data11)
countmodel.nb.off3.4<-glm.nb(claim.count~business.type + driver.age + driver.gender +
                               yrs.licensed + ncd.level + region1+ vehicle.age + 
                               vehicle.value + no.seats + horse.power + weight + width + 
                               prior.claims + offset(log(exposure))+
                               +height:width+cubic.cent:weight,
                         data=train.data11)
summary(countmodel.nb.off3.1)

pred.nb.off3.1<-predict(countmodel.nb.off3.1,test.data,type='response')
pred.nb.off3.2<-predict(countmodel.nb.off3.2,test.data,type='response')
pred.nb.off3.3<-predict(countmodel.nb.off3.3,test.data11,type='response')
pred.nb.off3.4<-predict(countmodel.nb.off3.4,test.data11,type='response')
test.MSE2<-c(RMSE(test.data$claim.count,pred.nb.off3.1),
             RMSE(test.data$claim.count,pred.nb.off3.2),
             RMSE(test.data11$claim.count,pred.nb.off3.3),
             RMSE(test.data11$claim.count,pred.nb.off3.4))
pred.train.nb.off3.1<-predict(countmodel.nb.off3.1,train.data,type='response')
pred.train.nb.off3.2<-predict(countmodel.nb.off3.2,train.data,type='response')
pred.train.nb.off3.3<-predict(countmodel.nb.off3.3,train.data11,type='response')
pred.train.nb.off3.4<-predict(countmodel.nb.off3.4,train.data11,type='response')
train.MSE2<-c(RMSE(train.data$claim.count,pred.train.nb.off3.1),
              RMSE(train.data$claim.count,pred.train.nb.off3.2),
              RMSE(train.data11$claim.count,pred.train.nb.off3.3),
              RMSE(train.data11$claim.count,pred.train.nb.off3.4))
View(train.data1)
k=5
set.seed(1)
P9<-cv.glm(train.data,countmodel.nb.off3.1,K=k)
P10<-cv.glm(train.data,countmodel.nb.off3.2,K=k)
P11<-cv.glm(train.data11,countmodel.nb.off3.3,K=k)
P12<-cv.glm(train.data11,countmodel.nb.off3.4,K=k)
cv.error3<-c(P9$delta[2],P10$delta[2],P11$delta[2],P12$delta[2])


tab3<-cbind(c(AIC(countmodel.nb.off3.1),
              AIC(countmodel.nb.off3.2),
              AIC(countmodel.nb.off3.3),
              AIC(countmodel.nb.off3.4)),
            c(BIC(countmodel.nb.off3.1),
              BIC(countmodel.nb.off3.2),
              BIC(countmodel.nb.off3.3),
              BIC(countmodel.nb.off3.4)))

colnames(tab3)<-c('AIC','BIC')
rownames(tab3)<-c('model (2) with interaction',
                  'model (3) with interaction',
                  'model (4) with interaction',
                  'model (5) with interaction')
tab4<-rbind(tab2,tab3)
cv.error1
cv.error2
cv.error3
View(tab4)
tab4<-cbind(tab4,cv.error=c(cv.error1[3],cv.error2,cv.error3))
tab4<-cbind(tab4,test.RMSE=c(test.MSE[3],test.MSE1,test.MSE2))
tab1
summary(countmodel.nb.off2.4)


plot(countmodel.nb.off3.1$fitted.values,rstandard(countmodel.nb.off3.1),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off3.1)) 
qqline(rstandard(countmodel.nb.off3.1),col="red")

plot(countmodel.nb.off3.2$fitted.values,rstandard(countmodel.nb.off3.2),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off3.2)) 
qqline(rstandard(countmodel.nb.off3.2),col="red")

plot(countmodel.nb.off3.3$fitted.values,rstandard(countmodel.nb.off3.3),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off3.3)) 
qqline(rstandard(countmodel.nb.off3.3),col="red")

plot(countmodel.nb.off3.4$fitted.values,rstandard(countmodel.nb.off3.4),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Negative Binomial")
abline(h=0,col="red",lty=2)
qqnorm(rstandard(countmodel.nb.off3.4)) 
qqline(rstandard(countmodel.nb.off3.4),col="red")
tab1

View(tab4)
#severity model


region.analysis.sev<-gen.data1%>%
  filter(sev>0)%>%
  group_by(region)%>%
  summarize(mean.sev=mean(sev),
            median.sev=median(sev),
            count=n())

ncd.analysis.sev<-train.data1%>%
  filter(sev>0)%>%
  group_by(ncd.level)%>%
  summarize(mean.sev=mean(sev),
            median.sev=median(sev),
            count=n())
ncd.analysis<-train.data1%>%
  group_by(ncd.level)%>%
  summarize(mean.sev=mean(claim.count),
            
            count=n())

bodycode.analysis.sev<-train.data1%>%
  filter(sev>0)%>%
  group_by(body.code)%>%
  summarize(mean.sev=mean(sev),
            median.sev=median(sev),
            count=n())

marital.analysis.sev<-train.data1%>%
  filter(sev>0)%>%
  group_by(marital.status)%>%
  summarize(mean.sev=mean(sev),
            median.sev=median(sev),
            count=n())
View(region.analysis.sev%>%filter(count>100))
View(train.sev%>%
       filter(sev>0)%>%
       group_by(region)%>%
       summarize(mean.sev=mean(sev),
                 median.sev=median(sev),
                 count=n()))
train.sev<-train.data%>%
  filter(sev>0)

test.sev<-test.data%>%
  filter(sev>0)
train.sev1<-train.data1%>%
  filter(sev>0)
lapply(train.sev1,class)
test.sev1<-test.data1%>%
  filter(sev>0)


View(res2)
region2<-ifelse(gen.data1$region %in% c(30,21,34,12),'group1',
                ifelse(gen.data1$region %in% c(20,38,27,16,37),'group2',
                       ifelse(gen.data1$region %in% c(5,19),'group3',
                              ifelse(gen.data1$region %in% c(28,22),'group4',
                                     ifelse(gen.data1$region %in% c(1,25),'group6',
                                            ifelse(gen.data1$region %in% c(32,15,18), 'group7',gen.data1$region
                                            ))))))

body.code2<-ifelse(gen.data1$body.code %in% c('C','G'),'group1',
                   gen.data1$body.code)
ncd.level2<-ifelse(gen.data1$ncd.level %in% c(4,6),'group1',
                   gen.data1$body.code)
region2<-as.factor(region2)
body.code2<-as.factor(body.code2)
ncd.level2<-as.factor(ncd.level2)
gen.data12<-cbind(gen.data1[,-c(7,8,9)],region2,body.code2,ncd.level2)
gen.data32<-cbind(gen.data3[,-c(7,8,9)],region2,body.code2,ncd.level2)

train.sev2<-gen.data32[index,]%>%
  filter(sev>0)
test.sev2<-gen.data32[-index,]%>%
  filter(sev>0)

sev.modelgam<-glm(sev~.-claim.incurred-freq-claim.count-tot.loss,
                  data=train.sev,
               family=Gamma(link='log'))

sev.modelgam1<-glm(sev~.-claim.incurred-freq-claim.count-tot.loss,
                   data=train.sev1,
               family=Gamma(link='log'))


View(tab8)
?summary.glm
View(sevdata)
summary(sev.modelgam1)
tab4
tab5<-cbind(c(AIC(sev.modelgam),
              AIC(sev.modelgam1)),
            c(BIC(sev.modelgam),
              BIC(sev.modelgam1)))
colnames(tab5)<-c('AIC','BIC')
rownames(tab5)<-c('model with no PC','model with PC')
set.seed(1)
P13<-cv.glm(train.sev,sev.modelgam,K=k)
P14<-cv.glm(train.sev1,sev.modelgam1,K=k)
cv.err.sev<-c(P13$delta[2],P14$delta[2])
pred.sevgam<-predict(sev.modelgam,test.sev,type='response')
pred.sevgam1<-predict(sev.modelgam1,test.sev1,type='response')
test.sev.MSE<-c(RMSE(test.sev$sev,pred.sevgam),
                RMSE(test.sev$sev,pred.sevgam1))
tab5<-cbind(tab5,cv.error=cv.err.sev,test.RMSE=test.sev.MSE)
graphics.off()
plot(sev.modelgam$fitted.values,rstandard(sev.modelgam),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Gamma",xlim = c(0,3000),ylim=c(-4,4))
abline(h=0,col="red",lty=2)
qqnorm(rstandard(sev.modelgam)) 
qqline(rstandard(sev.modelgam),col="red")
meannegbin
plot(sev.modelgam1$fitted.values,rstandard(sev.modelgam1),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Gamma",xlim = c(0,200000),ylim=c(-2,3))
qqnorm(rstandard(sev.modelgam1)) 
qqline(rstandard(sev.modelgam1),col="red")

k=5
set.seed(1)
P13<-cv.glm(train.sev,sev.modelgam,K=k)
P14<-cv.glm(train.sev1,sev.modelgam1,K=k)
cv.err.sev<-c(P13$delta[2],P14$delta[2])

pred.sevgam<-predict(sev.modelgam,test.sev,type='response')
pred.sevgam1<-predict(sev.modelgam1,test.sev1,type='response')
test.sev.MSE<-c(RMSE(test.sev$sev,pred.sevgam),
                RMSE(test.sev$sev,pred.sevgam1))




sev.model.none<-glm(sev~1,
                    data=train.sev1,
                    family=Gamma(link='log'))
step.choose1<-stepAIC(sev.modelgam1,
                     direction = "backward",
                     k = 2,
                     scope = list(upper = sev.modelgam1, 
                                  lower = sev.model.none)
)


#refit with stepAIC vs remove unimportant variables with |coef|>0.1
sev.modelgam1.1<-glm(sev~.-claim.incurred-freq-claim.count-tot.loss-body.code
                     -exposure-marital.status-driver.gender-fuel.type,
                 data=train.sev1,
                 family=Gamma(link='log'))

sev.modelgam1.2<-glm(sev ~ exposure + ncd.level + PCage1,family=Gamma(link='log'),
                     data=train.sev1)
#same as above but with collapsed category
View(train.sev2)
sev.modelgam1.3<-glm(sev~.-claim.incurred-freq-claim.count-tot.loss
                     -exposure-marital.status-driver.gender-fuel.type,
                     data=train.sev2,
                     family=Gamma(link='log'))
sev.modelgam1.4<-glm(sev ~ exposure + ncd.level2 +region2+body.code2
                     + PCage1,family=Gamma(link='log'),
                     data=train.sev2)

summary(sev.modelgam1.1)

tab6<-cbind(c(AIC(sev.modelgam1),
              AIC(sev.modelgam1.1),
              AIC(sev.modelgam1.2),
              AIC(sev.modelgam1.3),
              AIC(sev.modelgam1.4)),
            c(BIC(sev.modelgam1),
              BIC(sev.modelgam1.1),
              BIC(sev.modelgam1.2),
              BIC(sev.modelgam1.3),
              BIC(sev.modelgam1.4))
            )
colnames(tab6)<-c('AIC','BIC')
rownames(tab6)<-c('original model (1)',
                  'unimportant variable with |coef|>6 removed (2)',
                  'variables chosen using stepAIC (3)',
                  'model 2 with collapsed categorical variable',
                  'model 3 with collapsed categorical variable')
tab6
k=5
set.seed(1)
P15<-cv.glm(train.sev1,sev.modelgam1.1,K=k)
P16<-cv.glm(train.sev1,sev.modelgam1.2,K=k)
P17<-cv.glm(train.sev2,sev.modelgam1.3,K=k)
P18<-cv.glm(train.sev2,sev.modelgam1.4,K=k)
cv.err.sev1<-c(P15$delta[2],P16$delta[2],P17$delta[2],P18$delta[2])


pred.sevgam1.1<-predict(sev.modelgam1.1,test.sev1,type='response')
pred.sevgam1.2<-predict(sev.modelgam1.2,test.sev1,type='response')
pred.sevgam1.3<-predict(sev.modelgam1.3,test.sev2,type='response')
pred.sevgam1.4<-predict(sev.modelgam1.4,test.sev2,type='response')
test.sev.MSE1<-c(RMSE(test.sev1$sev,pred.sevgam1.1),
                RMSE(test.sev1$sev,pred.sevgam1.2),
                RMSE(test.sev2$sev,pred.sevgam1.3),
                RMSE(test.sev2$sev,pred.sevgam1.4))

#looking for interaction
sev.personal<-glm(sev~(marital.status+driver.gender+driver.age)^2,
                  data=train.sev1,
                  family=Gamma(link='log'))


sev.vehicle<-glm(sev~(PCage1+ PCsize1+PCsize2+PCsize3+fuel.type)^2,
                       data=train.sev1,
                  family=Gamma(link='log'))
summary(sev.vehicle)
summary(sev.personal)
summary(train.sev$body.code)




View(train.sev)
View(region.analysis.sev)
#models with interaction



sev.modelgam2.1<-glm(sev~.-claim.incurred-freq-claim.count-tot.loss-body.code
                     -exposure-marital.status-driver.gender-fuel.type
                     +PCage1:fuel.type,data=train.sev1,family=Gamma(link='log'))
summary(sev.modelgam2.1)
tab8
sev.modelgam2.2<-glm(sev~exposure + ncd.level + PCage1+PCage1:fuel.type,
                     data=train.sev1,family=Gamma(link='log'))

sev.modelgam2.3<-glm(sev~.-claim.incurred-freq-claim.count-tot.loss
                     -exposure-marital.status-driver.gender-fuel.type
                     +PCage1:fuel.type,data=train.sev2,family=Gamma(link='log'))
sev.modelgam2.4<-glm(sev~exposure + ncd.level2 + PCage1+PCage1:fuel.type,
                     data=train.sev2,family=Gamma(link='log'))

tab7<-cbind(c(AIC(sev.modelgam2.1),
              AIC(sev.modelgam2.2),
              AIC(sev.modelgam2.3),
              AIC(sev.modelgam2.4)),
            c(BIC(sev.modelgam2.1),
              BIC(sev.modelgam2.2),
              BIC(sev.modelgam2.3),
              BIC(sev.modelgam2.4)
              )
)
tab6[-c(4,5),]
colnames(tab7)<-c('AIC','BIC')
rownames(tab7)<-c('(2) with interaction effect',
                  '(3) with interaction effect',
                  '(4) with interaction effect',
                  '(5) with interaction effect')
tab7<-rbind(tab6,tab7)

set.seed(1)
P19<-cv.glm(train.sev1,sev.modelgam2.1,K=k)
P20<-cv.glm(train.sev1,sev.modelgam2.2,K=k)
P21<-cv.glm(train.sev2,sev.modelgam2.3,K=k)
P22<-cv.glm(train.sev2,sev.modelgam2.4,K=k)
cv.err.sev2<-c(P19$delta[2],P20$delta[2],P21$delta[2],P22$delta[2])

pred.sevgam2.1<-predict(sev.modelgam2.1,test.sev1,type='response')
pred.sevgam2.2<-predict(sev.modelgam2.2,test.sev1,type='response')
pred.sevgam2.3<-predict(sev.modelgam2.3,test.sev2,type='response')
pred.sevgam2.4<-predict(sev.modelgam2.4,test.sev2,type='response')
test.sev.MSE2<-c(RMSE(test.sev1$sev,pred.sevgam2.1),
                 RMSE(test.sev1$sev,pred.sevgam2.2),
                 RMSE(test.sev1$sev,pred.sevgam2.3),
                 RMSE(test.sev1$sev,pred.sevgam2.4))
tab8<-cbind(tab7,cv.error=c(cv.err.sev[2],cv.err.sev1,cv.err.sev2),
            test.error=c(test.sev.MSE[2],test.sev.MSE1,test.sev.MSE2))
View(tab8)

tab4
plot(sev.modelgam1$fitted.values,rstandard(sev.modelgam1),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="Diagnostic checking: SDR vs FV, Gamma",xlim = c(0,max(sev.modelgam$fitted.values)+500),ylim=c(-3,3))
abline(h=0,col="red",lty=2)
tab8
View(tab4)
summary(count.vehicle)
summary(countmodel.nb.off2.3)
####conclusion so far
#FREQUENCY: count.model.nb.off2.3
#reason: simplicity, no real standout, so simplest model is the best
View(ncd.analysis)
#SEVERITY: sev.modelgam1.1
#Reason: standout result but does not fit very well
tab8
View(gen.data1)
#pure premium error calculation
final.pred.freq<-predict(countmodel.nb.off2.3,test.data11,type='response')/test.data11$exposure
final.pred.sev<-predict(sev.modelgam1.1,test.data1,type='response')
View(cbind(pred.nb.off2.4,test.data$exposure,final.pred.freq,test.data$claim.count))
final.pred.prem<-final.pred.freq*final.pred.sev
tab4
testMSE.prem<-sqrt(mean((final.pred.prem-test.data1$tot.loss)^2))
postResample(pred=final.pred.prem,obs=test.data$tot.loss)




meannegbin<-countmodel.nb.off2.3$fitted.values
numrow<-max(train.data11$claim.count)+1

emp<-rep(0,numrow)
for(i in 1:numrow){
  emp[i]<-sum(train.data11$claim.count==(i-1)) 
  }
sum(emp)
expnb<-rep(0,numrow)
for(i in 1:numrow){
  expnb[i]<-sum(dnbinom(i-1,mu=meannegbin,size=summary(countmodel.nb.off2.3)$theta))
  }
emp
freq.tab<-cbind(emp,expnb)
tab8
