mydata<-read.csv("C:/Users/ravi/Desktop/Admission_Prediction.csv")
head(mydata)
tail(mydata)
str(mydata)
dim(mydata)
names(mydata)
summary(mydata)
range(mydata$GRE.Score)
mean(mydata$GRE.Score)
sd(mydata$GRE.Score)
#Gre score analysis.
#histogram
hist(mydata$GRE.Score,col="green",xlab="Class for GRE Score",ylab="Frequency",main="Histogram for GRE Score",
     labels=TRUE)
#boxplot
boxplot(mydata$GRE.Score,col="orange",
        main="Descreptive Analysis of GRE Score",horizontal=TRUE,xlab="GRE Score")
#scatter plot
library(corrplot) 
cor(mydata$GRE.Score ,mydata$Chance.of.Admit) # correlation score
plot(mydata$GRE.Score,mydata$Chance.of.Admit,col="blue",
     main="Scatter Plot Between GRE Score and Chance of Admission",
     xlab="GRE Score",ylab="Chance of Admission")
library(ggplot2)
ggplot(data= mydata, aes(x = mydata$GRE.Score, y = mydata$Chance.of.Admit)) + geom_point()+geom_smooth(method='lm',se=TRUE)+labs(x="CGPA",y="Chances of admissions") 

#chi square test
chisq.test(mydata$GRE.Score,mydata$Chance.of.Admit)

#correlation plot
rplot<-cor(mydata) 
rplot
corrplot(rplot, method="number")
corrplot(rplot, method="number", addCoef.col = "black")

# TOEFL SCORE ANALYSIS
range(mydata$TOEFL.Score)
mean(mydata$TOEFL.Score)
#histogram 
hist(mydata$TOEFL.Score , col = "pink" , xlab = "TOEFL SCORE",ylab = "frequency" , main = "histogram of TOEFL SCORE" , labels = TRUE)
boxplot(mydata$TOEFL.Score,
        horizontal=FALSE,
        main="Boxplot for TOEFL Score",
        xlab="TOEFL Score",
        col="pink")
chisq.test(mydata$TOEFL.Score , mydata$Chance.of.Admit)
ggplot(data= mydata, aes(x = mydata$TOEFL.Score, y = mydata$Chance.of.Admit)) + geom_point()+geom_smooth(method='lm',se=FALSE)+labs(x="TOEFL SCORE",y="Chances of admissions")
cor(mydata$TOEFL.Score , mydata$Chance.of.Admit)

#Analysis on University RATING
summary(mydata$University.Rating)
a=table(mydata$University.Rating)
a
barplot(a,
        col=rainbow(2),
        legend=rownames(a),
        main="Barplot of University Rating ",
        xlab="University Rating",
        ylab="Count")

cor(mydata$University.Rating,mydata$Chance.of.Admit)
ggplot(data= mydata, aes(x = mydata$University.Rating, y = mydata$Chance.of.Admit)) + geom_point()+geom_smooth(method='lm',se=FALSE)+labs(x="university rating",y="Chances of admissions")
chisq.test(mydata$University.Rating,mydata$Chance.of.Admit)

#Analysis of CGPA
summary(mydata$CGPA)
hist(mydata$CGPA , col ="yellow", xlab = "CGPA" , ylab = "Frequency", main ="histogram of cgpa" , labels = TRUE )
ggplot(data= mydata, aes(x = mydata$CGPA, y = mydata$Chance.of.Admit)) + geom_point()+geom_smooth(method='lm',se=FALSE)+labs(x="CGPA",y="Chances of admissions")
cor(mydata$CGPA,mydata$Chance.of.Admit)
chisq.test(mydata$CGPA,mydata$Chance.of.Admit)

#Analysis of LOR
summary(mydata$LOR)
a=table(mydata$LOR)
a
barplot(a,
        col=rainbow(2),
        legend=rownames(a),
        main="Barplot of lor ",
        xlab="lor",
        ylab="Count")
chisq.test(mydata$LOR,mydata$Chance.of.Admit)
cor(mydata$LOR,mydata$Chance.of.Admit)
ggplot(data= mydata, aes(x = mydata$LOR, y = mydata$Chance.of.Admit)) + geom_point()+geom_smooth(method='lm',se=FALSE)+labs(x="STRENGTH OF LOR",y="Chances of admissions")

#Analysis of SOP
summary(mydata$SOP)
plot(mydata$SOP,mydata$Chance.of.Admit,type="h",
     col="green",
     xlab="SOP STRENGTH",
     ylab="Chance of Admission",
     main="Horizontal Line Plot")
chisq.test(mydata$SOP,mydata$Chance.of.Admit)
cor(mydata$SOP,mydata$Chance.of.Admit)
ggplot(data= mydata, aes(x = mydata$SOP, y = mydata$Chance.of.Admit)) + geom_point()+geom_smooth(method='lm',se=FALSE)+labs(x="STRENGTH OF SOP",y="Chances of admissions")

#Analysis of Research 
summary(mydata$Research)
a=table(mydata$Research)
a
barplot(a,
        col=rainbow(2),
        legend=rownames(a),
        main="Barplot of Research",
        xlab="Research",
        ylab="Count")

cor(mydata$Research,mydata$Chance.of.Admit)
# pie chart
pct=round(a/sum(a)*100)
lbs=paste(c("No","Yes")," ",pct,"%",sep=" ")
pie(a,labels=lbs,
    main="Research Done or Not")

#checking for null values
any(is.na(mydata)) 

#multicollinearity
#correlation plot is above 
install.packages("VIF")
library(VIF)
car::vif(lm(Chance.of.Admit~., data=mydata)) 

#data preparation
data=mydata[,-c(1)]
head(data)

# splitting data
library(caTools)
set.seed(1)
sample=sample.split(data$Chance.of.Admit,SplitRatio = 0.80)
train_set=subset(data,sample==TRUE)
test_set=subset(data,sample==FALSE)

dim(train_set)
dim(test_set)

# Multi regresser model
mod1=lm(Chance.of.Admit~.,train_set)
summary(mod1)
plot(mod1)


mod2=lm((Chance.of.Admit)~GRE.Score+TOEFL.Score+LOR+CGPA+Research,train_set)
sm = summary(mod2)
sm
mean(sm$residuals^2)
plot(mod2)


# single regressor
mod11=lm(Chance.of.Admit~(GRE.Score),train_set)
summary(mod11)
plot(mod11)
mod12=lm(Chance.of.Admit~TOEFL.Score,train_set)
summary(mod12)
mod13=lm(Chance.of.Admit~LOR,train_set)
summary(mod13)
mod14=lm(Chance.of.Admit~SOP,train_set)
summary(mod14)
mod15=lm(Chance.of.Admit~CGPA,train_set)
summary(mod15)
mod16=lm(Chance.of.Admit~Research,train_set)
summary(mod16)
mod16=lm(Chance.of.Admit~university,train_set)
summary(mod16)
#predictions
predictions = predict(mod2,test_set)
final_data=cbind(test_set,predictions)
final_data
ggplot(data= final_data, aes(x = final_data$GRE.Score, y = final_data$predictions)) + geom_point(color = "red")+geom_smooth(method='lm',se=FALSE)+labs(x="GRE SCORE",y="PREDICTIONS")
cor(final_data$Chance.of.Admit,final_data$predictions)

#RESIDUAL 
residuals=final_data$Chance.of.Admit - final_data$predictions
residuals=as.data.frame(residuals*residuals)
head(residuals,10)
s=sum(residuals)/101
sqrt(s)
str(residuals)

#----building single regressor linear regression model--- 
#-----model 1---- 
mod3=lm(Chance.of.Admit~GRE.Score,data = train_set) 
summary(mod3) 
plot(mod3)



#mod4
mod4=lm(Chance.of.Admit~TOEFL.Score,data = train_set)

summary(mod4) 
plot(mod4) #tap 4 times ctr+enter to see residual vs fitted , normal qq plot ,scale location  deviations std , residual vs leverage


#mod5
mod5=lm(Chance.of.Admit~CGPA,data = train_set)
summary(mod5) 
plot(mod5)

#MOD6 
mod6=lm(Chance.of.Admit~LOR,data = train_set)
summary(mod6) 
plot(mod6)

#MOD7 
mod7=lm(Chance.of.Admit~SOP,data = train_set)
summary(mod7) 
plot(mod7)

#MOD8 
mod8=lm(Chance.of.Admit~Research,data = train_set)
summary(mod8) 
plot(mod8)

#MOD9 
mod9=lm(Chance.of.Admit~University.Rating,data = train_set)
summary(mod9) 
plot(mod9)

#MOD10 
mod10=lm(Chance.of.Admit~GRE.Score+TOEFL.Score+CGPA,data = train_set)
summary(mod10) 
plot(mod10)

#MOD11 
mod11=lm(Chance.of.Admit~GRE.Score+TOEFL.Score+CGPA+University.Rating,data = train_set)
summary(mod11) 
plot(mod11)

#MOD11 
mod12=lm(Chance.of.Admit~GRE.Score+TOEFL.Score+CGPA+LOR+Research,data = train_set)
summary(mod12) 
plot(mod12)

#robust standard error
#--------robust standard error -------
#(kindly look upon standard error these are different from mod1 like they are standard errorthese are robust standard error) 
library("lmtest") 
library("sandwich") 
coeftest(mod1, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod2, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod3, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod4, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod5, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod6, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod7, vcov = vcovHC(mod1, type = "HC3")) 
coeftest(mod8, vcov = vcovHC(mod1, type = "HC3")) 

#BP TEST(IF P < 0.05 DATA IS HETEROSKEDASTIC)
bptest(mod1) 
bptest(mod2) 
bptest(mod3) 
bptest(mod4) 
bptest(mod5) 
bptest(mod6) 
bptest(mod7) 
bptest(mod8) 
bptest(mod9) 
bptest(mod10) 
bptest(mod11) 

