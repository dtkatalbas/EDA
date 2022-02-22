setwd("C:/Users/Dylan/OneDrive - Texas Tech University/Spring 2022/IE 4331 Exploratory Data Analysis/Assignments/Assignment 3")
dat<-read.csv("SKU Master.csv")

dat<-na.omit(dat)

dat<-dat[dat$UomCube<2,]
dat<-dat[dat$UomCube>0,]

dat<-dat[dat$UomWeight<50,]
dat<-dat[dat$UomWeight>0,]

dat$Uom<-as.factor(dat$Uom)
dat$Whs<-as.factor(dat$Whs)
dat$Flow<-as.factor(dat$Flow)

dat$SkuNbr<-as.factor(dat$SkuNbr)

dat<-dat[dat$Uom %in% c("CA","EA","PL","LB"),] #would not filter

#datCA<-dat[dat$Uom=="CA",]
#datEA<-dat[dat$Uom=="EA",]
#datPL<-dat[dat$Uom=="PL",]
#datLB<-dat[dat$Uom=="LB",]

#dat<-merge(datCA,datEA,all=TRUE) #ask about all=true
#dat<-merge(dat,datPL,all=TRUE)
#dat<-merge(dat,datLB,all=TRUE)

dat<-droplevels(dat)

boxplot(dat$Uom) # 1 Outlier above the whisker
plot(dat$UnitsPerCase,dat$UomWeight) #seemingly no correlation

#dat<-dat[!(dat$Whs==""),] #remove observations that has a blank entry or leave it in?
dat<-droplevels(dat)

plot(dat$Whs)
plot(dat$Uom)

plot(dat$Flow,dat$UomCube) #dat$Flow must be a factor not character

#consider only Direct to Store
dat<-dat[dat$Flow=="DD",]
boxplot(dat$UomWeight) 
#there are 3 outliers above the boxplot which have the value of 29,20, and 17.80
#unsure of what UoM meaning and how it correlates to weight. 
#Units per case seem to be an important factor here

hist(dat$UomWeight,
     breaks=20,
     main="Frequency of Weight",
     xlab="Weight in Pounds",
     font.lab=3,
     col="tomato")

dotchart(dat$UomWeight,
     main="Weight of SKUs",
     labels=dat$SkuNbr,
     pch=21,
     bg="Blue",
     ylab="SKU Number",
     xlab="Weight in Pounds",
     cex=1.5
     ) #SKU 47849990402 has the highest UomWeight at 29
dat<-droplevels(dat)
stripchart(dat$UomWeight~dat$Uom,
           main="Weight by Unit of Measure",
           xlab="Weight in Pounds",
           ylab="Unit of Measurement",
           pch=21,
           bg="Olivedrab",
           cex=1.5,
           cex.lab=1.5,
           cex.main=1.5)
legend("topright",
       c("EA= each","CA = Case"))

       