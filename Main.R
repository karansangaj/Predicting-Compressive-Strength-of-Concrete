
#########################################################
#                                                       #
#                    Linear Regression                  #
#                                                       #
#########################################################

require(MASS)
require(faraway)
require(ISLR)
require(caret)

install.packages('devEMF')

# read data
concrete=read.csv("concrete.csv")
par(mfrow=c(1,1))
cor(concrete)
png(file="Pairs.png",width=10,height=10,units="in",res=300, pointsize=8)
pairs(concrete,pch=19,col = c("red", "yellow", "orange", "green"))
dev.off()

# Additive Model
lmod=lm(strength~.,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="AdditiveModel_FittedValues.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue",ylab="Actual Values",xlab="Fitted Values")
abline(a=0,b=1)
dev.off()


# calculate AIC and BIC
n=dim(concrete)[1]
p=dim(concrete)[2]-1
AIC=n*log(sum(residuals(lmod)^2)/n)+2*p
BIC=n*log(sum(residuals(lmod)^2)/n)+log(n)*p
paste("AIC = ",AIC)
paste("BIC = ",BIC)

# Reduced Additive Model
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="ReducedAdditiveModel_FittedValues.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue",ylab="Actual Values",xlab="Fitted Values")
abline(a=0,b=1)
dev.off()

# calculate AIC and BIC
n=dim(concrete)[1]
p=dim(concrete)[2]-1
AIC=n*log(sum(residuals(lmod)^2)/n)+2*p
BIC=n*log(sum(residuals(lmod)^2)/n)+log(n)*p
paste("AIC = ",AIC)
paste("BIC = ",BIC)

# calculate centered predictors
concrete$cement_c=concrete$cement-mean(concrete$cement)
concrete$slag_c=concrete$slag-mean(concrete$slag)
concrete$fly_ash_c=concrete$fly_ash-mean(concrete$fly_ash)
concrete$water_c=concrete$water-mean(concrete$water)
concrete$superplasticizer_c=concrete$superplasticizer-mean(concrete$superplasticizer)
concrete$course_aggregate_c=concrete$course_aggregate-mean(concrete$course_aggregate)
concrete$fine_aggregate_c=concrete$fine_aggregate-mean(concrete$fine_aggregate)
concrete$age_c=concrete$age-mean(concrete$age)

# Full Additive Model with Interactions
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="AdditiveModelInteractions_FittedValues.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue",ylab="Actual Values",xlab="Fitted Values")
abline(a=0,b=1)
dev.off()


# calculate AIC and BIC
n=dim(concrete)[1]
p=dim(concrete)[2]-1
AIC=n*log(sum(residuals(lmod)^2)/n)+2*p
BIC=n*log(sum(residuals(lmod)^2)/n)+log(n)*p
paste("AIC = ",AIC)
paste("BIC = ",BIC)

# Complete Second Order Model
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="CompleteSecondOrderModel_FittedValues.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue",ylab="Actual Values",xlab="Fitted Values")
abline(a=0,b=1)
dev.off()

# calculate AIC and BIC
n=dim(concrete)[1]
p=dim(concrete)[2]-1
AIC=n*log(sum(residuals(lmod)^2)/n)+2*p
BIC=n*log(sum(residuals(lmod)^2)/n)+log(n)*p
paste("AIC = ",AIC)
paste("BIC = ",BIC)

# Complete Second Order Model with insignifcant predictors removed
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+course_aggregate_c:fine_aggregate_c,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)


# Complete Second Order Model with insignifcant predictors removed - revised
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)

# Complete Second Order Model with insignifcant predictors removed - revised 2
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=concrete)
par(mfrow=c(2,2))
plot(lmod)
summary(lmod)
vif(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="SecondOrderModelinsignifcantremoved_FittedValues.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue",ylab="Actual Values",xlab="Fitted Values")
abline(a=0,b=1)
dev.off()

# calculate AIC and BIC
n=dim(concrete)[1]
p=dim(concrete)[2]-1
AIC=n*log(sum(residuals(lmod)^2)/n)+2*p
BIC=n*log(sum(residuals(lmod)^2)/n)+log(n)*p
paste("AIC = ",AIC)
paste("BIC = ",BIC)



#########################################################
#                                                       #
#                    Linear Regression                  #
#               Training and Validation Sets            #
#                                                       #
#########################################################

set.seed(100)
val=sample(1:49,10,replace = F)
train=concrete[-val,]
validate=concrete[val,]

mse=data.frame(matrix(NA,nrow=5,ncol=2))  # create dummy data frame to store MSE values
colnames(mse)=c("model","MSE")
mse$model=c("additive","red add","full add int","comp 2nd ord","red 2nd ord")

# Additive model and estimate test MSE
lmod=lm(strength~.,data=train)
pred=predict(lmod,validate)
mse[1,2]=mean((validate$strength-pred)^2)
mean((validate$strength-pred)^2)

# Reduced Additive model and estimate test MSE
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age,data=train)
pred=predict(lmod,validate)
mse[2,2]=mean((validate$strength-pred)^2)
mean((validate$strength-pred)^2)

# Full Additive Model with Interactions and estimate test MSE
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=train)
pred=predict(lmod,validate)
mse[3,2]=mean((validate$strength-pred)^2)
mean((validate$strength-pred)^2)

# Complete Second Order Model and estimate test MSE
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=train)
pred=predict(lmod,validate)
mse[4,2]=mean((validate$strength-pred)^2)
mean((validate$strength-pred)^2)

# Complete Second Order Model with insignifcant predictors removed
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=train)
pred=predict(lmod,validate)
mse[5,2]=mean((validate$strength-pred)^2)
mean((validate$strength-pred)^2)

# plot estimated Test Error
par(mfrow=c(1,1))
png(file="LinearTestValidation.png",width=10,height=10,units="in",res=300)
barplot(MSE~model,data=mse,col="blue",ylab="test MSE estimate",ylim=c(0,300))
text(0.7, 280, mse[1,2])
text(5.5, 280, mse[2,2])
text(3.1, 155, mse[3,2])
text(1.9, 85, mse[4,2])
text(4.3, 80, mse[5,2])
dev.off()


#########################################################
#                                                       #
#                    Linear Regression                  #
#                 k-Fold Cross-Validation               #
#                                                       #
#########################################################

mse=data.frame(matrix(NA,nrow=5,ncol=2))  # create dummy data frame to store MSE values
colnames(mse)=c("model","MSE")
mse$model=c("additive","red add","full add int","comp 2nd ord","red 2nd ord")

seed=100  # set seed
folds=5  # set number of folds
# Additive Model and estimate test MSE using 5-Fold CV
lmod.cv=train(strength~.,data=concrete,method="lm", 
              trControl=trainControl(method="cv",number=folds))
lmod.cv$results[2]
mse[1,2]=lmod.cv$results[2]^2

# Reduced Additive Model and estimate test MSE
lmod.cv=train(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age,data=concrete,method="lm", 
              trControl=trainControl(method="cv",number=folds))
lmod.cv$results[2]
mse[2,2]=lmod.cv$results[2]^2

# Full Additive Model with Interactions and estimate test MSE
lmod.cv=train(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete,method="lm", 
              trControl=trainControl(method="cv",number=folds))
lmod.cv$results[2]
mse[3,2]=lmod.cv$results[2]^2

# Complete Second Order Model and estimate test MSE
lmod.cv=train(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete,
              method="lm",trControl=trainControl(method="cv",number=folds))
lmod.cv$results[2]
mse[4,2]=lmod.cv$results[2]^2

# Complete Second Order Model with insignifcant predictors removed
lmod.cv=train(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=concrete,
              method="lm",trControl=trainControl(method="cv",number=folds))
lmod.cv$results[2]
mse[5,2]=lmod.cv$results[2]^2

# plot estimated Test Error
par(mfrow=c(1,1))
png(file="KfoldLinearRegression.png",width=10,height=10,units="in",res=300)
barplot(MSE~model,data=mse,col="blue",ylab="test MSE estimate",ylim=c(0,120))
text(0.7, 115, mse[1,2])
text(5.5, 115, mse[2,2])
text(3.1, 80, mse[3,2])
text(1.9, 65, mse[4,2])
text(4.3, 65, mse[5,2])
dev.off()

#########################################################
#                                                       #
#                    Ridge Regression                   #
#                                                       #
#########################################################


require(ISLR)
require(glmnet)
require(faraway)
require(dplyr)
require(leaps)


# read data and explore
concrete=data.frame(read.csv("concrete.csv"))

# calculate centered predictors
cement_c=concrete$cement-mean(concrete$cement)
slag_c=concrete$slag-mean(concrete$slag)
fly_ash_c=concrete$fly_ash-mean(concrete$fly_ash)
water_c=concrete$water-mean(concrete$water)
superplasticizer_c=concrete$superplasticizer-mean(concrete$superplasticizer)
course_aggregate_c=concrete$course_aggregate-mean(concrete$course_aggregate)
fine_aggregate_c=concrete$fine_aggregate-mean(concrete$fine_aggregate)
age_c=concrete$age-mean(concrete$age)

# add squares and second-order interactions to data frame
concrete$cement_sq=cement_c*cement_c
concrete$slag_sq=slag_c*slag_c
concrete$fly_ash_sq=fly_ash_c*fly_ash_c
concrete$water_sq=water_c*water_c
concrete$superplasticizer_sq=superplasticizer_c*superplasticizer_c
concrete$course_aggregate_sq=course_aggregate_c*course_aggregate_c
concrete$fine_aggregate_sq=fine_aggregate_c*fine_aggregate_c
concrete$age_sq=age_c*age_c
concrete$cement_x_slag=cement_c*slag_c
concrete$cement_x_fly_ash=cement_c*fly_ash_c
concrete$cement_x_water=cement_c*water_c
concrete$cement_x_superplasticizer=cement_c*superplasticizer_c
concrete$cement_x_course_aggregate=cement_c*course_aggregate_c
concrete$cement_x_fine_aggregate=cement_c*fine_aggregate_c
concrete$cement_x_age=cement_c*age_c
concrete$slag_x_fly_ash=slag_c*fly_ash_c
concrete$slag_x_water=slag_c*water_c
concrete$slag_x_superplasticizer=slag_c*superplasticizer_c
concrete$slag_x_course_aggregate=slag_c*course_aggregate_c
concrete$slag_x_fine_aggregate=slag_c*fine_aggregate_c
concrete$slag_x_age=slag_c*age_c
concrete$fly_ash_x_water=fly_ash_c*water_c
concrete$fly_ash_x_superplasticizer=fly_ash_c*superplasticizer_c
concrete$fly_ash_x_course_aggregate=fly_ash_c*course_aggregate_c
concrete$fly_ash_x_fine_aggregate=fly_ash_c*fine_aggregate_c
concrete$fly_ash_x_age=fly_ash_c*age_c
concrete$water_x_superplasticizer=water_c*superplasticizer_c
concrete$water_x_course_aggregate=water_c*course_aggregate_c
concrete$water_x_fine_aggregate=water_c*fine_aggregate_c
concrete$water_x_age=water_c*age_c
concrete$superplasticizer_x_course_aggregate=superplasticizer_c*course_aggregate_c
concrete$superplasticizer_x_fine_aggregate=superplasticizer_c*fine_aggregate_c
concrete$superplasticizer_x_age=superplasticizer_c*age_c
concrete$course_aggregate_x_fine_aggregate=course_aggregate_c*fine_aggregate_c
concrete$course_aggregate_x_age=course_aggregate_c*age_c
concrete$fine_aggregate_x_age=fine_aggregate_c*age_c

# now we have a data frame with 44 predictors that enable us to 
# fit a full second order model

#########################################################
#                                                       #
#                    Ridge Regression                   #
#                 Complete Second Order                 #
#########################################################

#  create predictor matric and vector for response
x=model.matrix(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete)[,-1]
y=concrete$strength

# compare lambda=0 to OLS
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete)
coef(glmnet(x,y,alpha=0,lambda=0)) # alpha=0 performs ridge regression
summary(lmod)

# compare lambda=100 to OLS
summary(lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete))
coef(glmnet(x,y,alpha=0,lambda=100))

# create grid for lambda, fit model using all lambdas
grid=10^seq(-1,-5,length=100) # lambda ranges from 0.1 to 0.00001 
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)  

# plot coefficent values as we change lambda
plot(ridge.mod,xlab="L2 Norm")  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r
mse.r

# best mse = 59.1651 at lambda = 1e-05
# recall best 5-fold cv mse = 60.0625 -- so essentially same estimated test error

# get coefficents for best model and compare to OLS
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef
summary(lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete))
length(ridge.coef)
# plotfitted values for OLS and Ridge, compare with actual with actual
fit.ridge=predict(ridge.mod,s=bestlam.r,x)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="SecondOrder_Ridge_OLS.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.ridge,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()

# compare R2 for each fit
R2.ridge=cor(fit.ridge,concrete$strength)^2
R2.lm=cor(lmod$fitted.values,concrete$strength)^2
R2.ridge
R2.lm

#########################################################
#                                                       #
#                    Ridge Regression                   #
#             Reduced Complete Second Order             #
#########################################################

#  create predictor matric and vector for response
x=model.matrix(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,concrete)[,-1]
y=concrete$strength

# compare lambda=0 to OLS
# Note: glmnet standardizes coefficient for fitting, but converts estimates back to original scale
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=concrete)
coef(glmnet(x,y,alpha=0,lambda=0)) # alpha=0 performs ridge regression
summary(lmod)

# compare lambda=100 to OLS
summary(lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=concrete))
coef(glmnet(x,y,alpha=0,lambda=100))

# create grid for lambda, fit model using all lambdas
grid=10^seq(-1,-5,length=100) # lambda ranges from 0.1 to 0.00001 
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)  

# plot coefficent values as we change lambda
plot(ridge.mod,xlab="L2 Norm")  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.ridge=cv.glmnet(x,y,alpha=0,lambda=grid)
plot(cv.ridge)
bestlam.r=cv.ridge$lambda.min
mse.r=min(cv.ridge$cvm)
bestlam.r
mse.r

# best mse = 60.75168 at lambda = 0.014174
# recall best 5-fold cv mse = 60.6815 -- so essentially same estimated test error

# get coefficents for best model and compare to OLS
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam.r)
ridge.coef
summary(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+course_aggregate_c:fine_aggregate_c,data=concrete)
length(ridge.coef)
# plotfitted values for OLS and Ridge, compare with actual with actual
fit.ridge=predict(ridge.mod,s=bestlam.r,x)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="ReducedSecondOrder_Ridge_OLS.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.ridge,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()

# compare R2 for each fit
R2.ridge=cor(fit.ridge,concrete$strength)^2
R2.lm=cor(lmod$fitted.values,concrete$strength)^2
R2.ridge
R2.lm


#########################################################
#                                                       #
#                    Lasso Regression                   #
#                                                       #
#########################################################

require(ISLR)
require(glmnet)
require(faraway)
require(dplyr)
require(leaps)


# read data and explore
concrete=data.frame(read.csv("concrete.csv"))

# calculate centered predictors
cement_c=concrete$cement-mean(concrete$cement)
slag_c=concrete$slag-mean(concrete$slag)
fly_ash_c=concrete$fly_ash-mean(concrete$fly_ash)
water_c=concrete$water-mean(concrete$water)
superplasticizer_c=concrete$superplasticizer-mean(concrete$superplasticizer)
course_aggregate_c=concrete$course_aggregate-mean(concrete$course_aggregate)
fine_aggregate_c=concrete$fine_aggregate-mean(concrete$fine_aggregate)
age_c=concrete$age-mean(concrete$age)

# add squares and second-order interactions to data frame
concrete$cement_sq=cement_c*cement_c
concrete$slag_sq=slag_c*slag_c
concrete$fly_ash_sq=fly_ash_c*fly_ash_c
concrete$water_sq=water_c*water_c
concrete$superplasticizer_sq=superplasticizer_c*superplasticizer_c
concrete$course_aggregate_sq=course_aggregate_c*course_aggregate_c
concrete$fine_aggregate_sq=fine_aggregate_c*fine_aggregate_c
concrete$age_sq=age_c*age_c
concrete$cement_x_slag=cement_c*slag_c
concrete$cement_x_fly_ash=cement_c*fly_ash_c
concrete$cement_x_water=cement_c*water_c
concrete$cement_x_superplasticizer=cement_c*superplasticizer_c
concrete$cement_x_course_aggregate=cement_c*course_aggregate_c
concrete$cement_x_fine_aggregate=cement_c*fine_aggregate_c
concrete$cement_x_age=cement_c*age_c
concrete$slag_x_fly_ash=slag_c*fly_ash_c
concrete$slag_x_water=slag_c*water_c
concrete$slag_x_superplasticizer=slag_c*superplasticizer_c
concrete$slag_x_course_aggregate=slag_c*course_aggregate_c
concrete$slag_x_fine_aggregate=slag_c*fine_aggregate_c
concrete$slag_x_age=slag_c*age_c
concrete$fly_ash_x_water=fly_ash_c*water_c
concrete$fly_ash_x_superplasticizer=fly_ash_c*superplasticizer_c
concrete$fly_ash_x_course_aggregate=fly_ash_c*course_aggregate_c
concrete$fly_ash_x_fine_aggregate=fly_ash_c*fine_aggregate_c
concrete$fly_ash_x_age=fly_ash_c*age_c
concrete$water_x_superplasticizer=water_c*superplasticizer_c
concrete$water_x_course_aggregate=water_c*course_aggregate_c
concrete$water_x_fine_aggregate=water_c*fine_aggregate_c
concrete$water_x_age=water_c*age_c
concrete$superplasticizer_x_course_aggregate=superplasticizer_c*course_aggregate_c
concrete$superplasticizer_x_fine_aggregate=superplasticizer_c*fine_aggregate_c
concrete$superplasticizer_x_age=superplasticizer_c*age_c
concrete$course_aggregate_x_fine_aggregate=course_aggregate_c*fine_aggregate_c
concrete$course_aggregate_x_age=course_aggregate_c*age_c
concrete$fine_aggregate_x_age=fine_aggregate_c*age_c

# now we have a data frame with 44 predictors that enable us to 
# fit a full second order model if we wish

#########################################################
#                                                       #
#                        Lasso                          #
#               Complete Second Order                   #
#########################################################

#  create predictor matric and vector for response
x=model.matrix(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,concrete)[,-1]
y=concrete$strength

# compare lambda=0 to OLS
# Note: glmnet standardizes coefficient for fitting, but converts estimates back to original scale
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete)
coef(glmnet(x,y,alpha=1,lambda=0)) # alpha=1 performs lasso
summary(lmod)

# compare lambda=0.005 to OLS
summary(lm(strength~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age+I(cement_c^2)+I(slag_c^2)+I(fly_ash_c^2)+I(water_c^2)+I(superplasticizer_c^2)+I(course_aggregate_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:slag_c+cement_c:fly_ash_c+cement_c:water_c+cement_c:superplasticizer_c+cement_c:course_aggregate_c+cement_c:fine_aggregate_c+cement_c:age_c+slag_c:fly_ash_c+slag_c:water_c+slag_c:superplasticizer_c+slag_c:course_aggregate_c+slag_c:fine_aggregate_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:course_aggregate_c+fly_ash_c:fine_aggregate_c+fly_ash_c:age_c+water_c:superplasticizer_c+water_c:course_aggregate_c+water_c:fine_aggregate_c+water_c:age_c+superplasticizer_c:course_aggregate_c+superplasticizer_c:fine_aggregate_c+superplasticizer_c:age_c+course_aggregate_c:fine_aggregate_c+course_aggregate_c:age_c+fine_aggregate_c:age_c,data=concrete))
vif(lmod)
coef(glmnet(x,y,alpha=1,lambda=0.005))

# create grid for lambda, fit model using all lambdas
grid=10^seq(-2,-6,length=100) # lambda ranges from 0.01 to 0.000001 
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  

# check coefficent values for each value of lambda
plot(lasso.mod)  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l
mse.l

# best mse = 59.15413 at lambda = 0.000001
# recall best 5-fold cv mse = 60.0625 -- so essentially same estimated test error

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef
summary(lmod)
length(lasso.coef)

# plotfitted values for OLS and Lasso, compare with actual with actual
fit.lasso=predict(lasso.mod,s=bestlam.l,x)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="SecondOrder_Lasso_OLS.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.lasso,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()

# compare R2 for each fit
R2.lasso=cor(fit.lasso,concrete$strength)^2
R2.lm=cor(lmod$fitted.values,concrete$strength)^2
R2.lasso
R2.lm

#########################################################
#                                                       #
#                        Lasso                          #
#             Reduced Complete Second Order             #
#########################################################

#  create predictor matric and vector for response
x=model.matrix(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,concrete)[,-1]
y=concrete$strength

# compare lambda=0 to OLS
lmod=lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=concrete)
coef(glmnet(x,y,alpha=1,lambda=0)) # alpha=1 performs lasso
summary(lmod)

# compare lambda=0.005 to OLS
summary(lm(strength~cement+slag+fly_ash+water+superplasticizer+age+I(cement_c^2)+I(water_c^2)+I(fine_aggregate_c^2)+I(age_c^2)+cement_c:water_c+cement_c:superplasticizer_c+cement_c:fine_aggregate_c+slag_c:water_c+slag_c:age_c+fly_ash_c:water_c+fly_ash_c:superplasticizer_c+fly_ash_c:age_c+water_c:course_aggregate_c+water_c:fine_aggregate_c,data=concrete))
vif(lmod)
coef(glmnet(x,y,alpha=1,lambda=0.005))

# create grid for lambda, fit model using all lambdas
grid=10^seq(-2,-6,length=100) # lambda ranges from 0.01 to 0.000001 
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  

# check coefficent values for each value of lambda
plot(lasso.mod)  # x-axis is in terms of sum(beta^2)
abline(h=0,lty=3)

# optimize lambda using cross-validation
set.seed(123)
cv.lasso=cv.glmnet(x,y,alpha=1,lambda=grid)
plot(cv.lasso)
bestlam.l=cv.lasso$lambda.min
mse.l=min(cv.lasso$cvm)
bestlam.l
mse.l

# best mse = 60.7566 at lambda = 0.00005462277
# recall best 5-fold cv mse = 60.6841 -- so essentially same estimated test error

# get coefficents for best model and compare to OLS
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.l)
lasso.coef
summary(lmod)
length(lasso.coef)

# plotfitted values for OLS and Lasso, compare with actual with actual
fit.lasso=predict(lasso.mod,s=bestlam.l,x)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="ReducedSecondOrder_Lasso_OLS.png.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.lasso,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()

# compare R2 for each fit
R2.lasso=cor(fit.lasso,concrete$strength)^2
R2.lm=cor(lmod$fitted.values,concrete$strength)^2
R2.lasso
R2.lm


#########################################################
#                                                       #
#                   Subset Selection                    #
#                using Cross-Validation                 #
#                 Complete Second Order                 #
#                                                       #
#########################################################


require(MASS)
require(faraway)
require(ISLR)
require(caret)
require(leaps)

# read data 
concrete=data.frame(read.csv("concrete.csv"))

# calculate centered predictors
cement_c=concrete$cement-mean(concrete$cement)
slag_c=concrete$slag-mean(concrete$slag)
fly_ash_c=concrete$fly_ash-mean(concrete$fly_ash)
water_c=concrete$water-mean(concrete$water)
superplasticizer_c=concrete$superplasticizer-mean(concrete$superplasticizer)
course_aggregate_c=concrete$course_aggregate-mean(concrete$course_aggregate)
fine_aggregate_c=concrete$fine_aggregate-mean(concrete$fine_aggregate)
age_c=concrete$age-mean(concrete$age)

# add squares and second-order interactions to data frame
concrete$cement_sq=cement_c*cement_c
concrete$slag_sq=slag_c*slag_c
concrete$fly_ash_sq=fly_ash_c*fly_ash_c
concrete$water_sq=water_c*water_c
concrete$superplasticizer_sq=superplasticizer_c*superplasticizer_c
concrete$course_aggregate_sq=course_aggregate_c*course_aggregate_c
concrete$fine_aggregate_sq=fine_aggregate_c*fine_aggregate_c
concrete$age_sq=age_c*age_c
concrete$cement_x_slag=cement_c*slag_c
concrete$cement_x_fly_ash=cement_c*fly_ash_c
concrete$cement_x_water=cement_c*water_c
concrete$cement_x_superplasticizer=cement_c*superplasticizer_c
concrete$cement_x_course_aggregate=cement_c*course_aggregate_c
concrete$cement_x_fine_aggregate=cement_c*fine_aggregate_c
concrete$cement_x_age=cement_c*age_c
concrete$slag_x_fly_ash=slag_c*fly_ash_c
concrete$slag_x_water=slag_c*water_c
concrete$slag_x_superplasticizer=slag_c*superplasticizer_c
concrete$slag_x_course_aggregate=slag_c*course_aggregate_c
concrete$slag_x_fine_aggregate=slag_c*fine_aggregate_c
concrete$slag_x_age=slag_c*age_c
concrete$fly_ash_x_water=fly_ash_c*water_c
concrete$fly_ash_x_superplasticizer=fly_ash_c*superplasticizer_c
concrete$fly_ash_x_course_aggregate=fly_ash_c*course_aggregate_c
concrete$fly_ash_x_fine_aggregate=fly_ash_c*fine_aggregate_c
concrete$fly_ash_x_age=fly_ash_c*age_c
concrete$water_x_superplasticizer=water_c*superplasticizer_c
concrete$water_x_course_aggregate=water_c*course_aggregate_c
concrete$water_x_fine_aggregate=water_c*fine_aggregate_c
concrete$water_x_age=water_c*age_c
concrete$superplasticizer_x_course_aggregate=superplasticizer_c*course_aggregate_c
concrete$superplasticizer_x_fine_aggregate=superplasticizer_c*fine_aggregate_c
concrete$superplasticizer_x_age=superplasticizer_c*age_c
concrete$course_aggregate_x_fine_aggregate=course_aggregate_c*fine_aggregate_c
concrete$course_aggregate_x_age=course_aggregate_c*age_c
concrete$fine_aggregate_x_age=fine_aggregate_c*age_c

best.mods=regsubsets(strength~.,data=concrete,nvmax=44,method="exhaustive")
summary(best.mods)

# use 5-fold CV to determine which best model for each number of predictors
# has the lowest estimated test error

pred.sbs=function(obj,new,id){
        form=as.formula(obj$call[[2]])
        mat=model.matrix(form,new)
        coefi=coef(obj,id=id)
        xvars=names(coefi)
        return(mat[,xvars]%*%coefi)
}

# set up for cross validation

k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:k,nrow(concrete),replace=T) 
# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,44,dimnames=list(NULL,paste(1:44)))

# perform CV
for (j in 1:k){
        # pick models with lowest RSS with 1-9 predictors fit without kth fold
        best.mods=regsubsets(strength~.,data=concrete[folds!=j,],
                             nvmax=44,method="exhaustive")
        # estimate test error for all nine models by predicting kth fold 
        for (i in 1:44){
                pred=pred.sbs(best.mods,concrete[folds==j,],id=i)
                cv.err[j,i]=mean((concrete$strength[folds==j]-pred)^2)  # save error est
        }
}

mse.cv=apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE
which.min(mse.cv) #min MSE for n predictors
apply(cv.err,2,mean)

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:44,mse.cv,type="b",xlab="no. of predictors",ylab="Estimated test MSE",ylim=c(0,100))
points(min,mse.cv[min],cex=2,col="red",lwd=2)
abline(v=c(0:50),lty=3)

length(best.mods)

# Model with minimum test error is complete second order model
# to finalize the model, refit our best subsets p=16 model using all the observations

coef(best.mods,id=44)
xvarm=names(coef(best.mods,id=44))[2:45]
concrete.best=concrete[,c("strength",xvarm)]
lmod=lm(strength~.,data=concrete.best)
summary(lmod)
par(mfrow=c(2,2),mar=c(4,4,4,4))
plot(lmod)
vif(lmod)


#########################################################
#                                                       #
#                   Subset Selection                    #
#             using Criterion-Based Methods             #
#             Reduced Complete Second Order             #
#########################################################

require(MASS)
require(faraway)
require(ISLR)
require(caret)
require(leaps)

# read data 
concrete=data.frame(read.csv("concrete.csv"))

# calculate centered predictors
cement_c=concrete$cement-mean(concrete$cement)
slag_c=concrete$slag-mean(concrete$slag)
fly_ash_c=concrete$fly_ash-mean(concrete$fly_ash)
water_c=concrete$water-mean(concrete$water)
superplasticizer_c=concrete$superplasticizer-mean(concrete$superplasticizer)
course_aggregate_c=concrete$course_aggregate-mean(concrete$course_aggregate)
fine_aggregate_c=concrete$fine_aggregate-mean(concrete$fine_aggregate)
age_c=concrete$age-mean(concrete$age)

# add squares and second-order interactions to data frame
concrete$cement_sq=cement_c*cement_c
concrete$slag_sq=slag_c*slag_c
concrete$fly_ash_sq=fly_ash_c*fly_ash_c
concrete$water_sq=water_c*water_c
concrete$superplasticizer_sq=superplasticizer_c*superplasticizer_c
concrete$course_aggregate_sq=course_aggregate_c*course_aggregate_c
concrete$fine_aggregate_sq=fine_aggregate_c*fine_aggregate_c
concrete$age_sq=age_c*age_c
concrete$cement_x_slag=cement_c*slag_c
concrete$cement_x_fly_ash=cement_c*fly_ash_c
concrete$cement_x_water=cement_c*water_c
concrete$cement_x_superplasticizer=cement_c*superplasticizer_c
concrete$cement_x_course_aggregate=cement_c*course_aggregate_c
concrete$cement_x_fine_aggregate=cement_c*fine_aggregate_c
concrete$cement_x_age=cement_c*age_c
concrete$slag_x_fly_ash=slag_c*fly_ash_c
concrete$slag_x_water=slag_c*water_c
concrete$slag_x_superplasticizer=slag_c*superplasticizer_c
concrete$slag_x_course_aggregate=slag_c*course_aggregate_c
concrete$slag_x_fine_aggregate=slag_c*fine_aggregate_c
concrete$slag_x_age=slag_c*age_c
concrete$fly_ash_x_water=fly_ash_c*water_c
concrete$fly_ash_x_superplasticizer=fly_ash_c*superplasticizer_c
concrete$fly_ash_x_course_aggregate=fly_ash_c*course_aggregate_c
concrete$fly_ash_x_fine_aggregate=fly_ash_c*fine_aggregate_c
concrete$fly_ash_x_age=fly_ash_c*age_c
concrete$water_x_superplasticizer=water_c*superplasticizer_c
concrete$water_x_course_aggregate=water_c*course_aggregate_c
concrete$water_x_fine_aggregate=water_c*fine_aggregate_c
concrete$water_x_age=water_c*age_c
concrete$superplasticizer_x_course_aggregate=superplasticizer_c*course_aggregate_c
concrete$superplasticizer_x_fine_aggregate=superplasticizer_c*fine_aggregate_c
concrete$superplasticizer_x_age=superplasticizer_c*age_c
concrete$course_aggregate_x_fine_aggregate=course_aggregate_c*fine_aggregate_c
concrete$course_aggregate_x_age=course_aggregate_c*age_c
concrete$fine_aggregate_x_age=fine_aggregate_c*age_c

# now we have a data frame with 44 predictors that enable us to 
# fit a full second order model if we wish


#########################################################
#                                                       #
#                  Best Subset Selection                #
#                                                       #
#########################################################

# best subset selection
# find the best subset of predictors based on RSS for each
# set of models with 1-44 predictors

best.mods=regsubsets(strength~.,nvmax=44,data=concrete,method="exhaustive")
best.sum=summary(best.mods)
best.sum

# can use criterion-based methods to select best model
names(best.sum)
p=1:44
aic=best.sum$bic+2*p-log(dim(concrete)[1])*p
which.min(aic)
which.min(best.sum$bic)
which.max(best.sum$adjr2)

# plot criteria to get visual confirmation

par(mfrow=c(2,2),mar=c(4,4,4,4))
plot(p,aic,pch=19,type="b",main="AIC",xlab="p",ylab="AIC")
points(which.min(aic),aic[which.min(aic)],cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

plot(p,best.sum$bic,pch=19,type="b",main="BIC",xlab="p",ylab="BIC")
points(which.min(best.sum$bic),best.sum$bic[which.min(best.sum$bic)],
       cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

plot(p,best.sum$adjr2,pch=19,type="b",main="adj-R2",xlab="p",ylab="ADJ_R2")
points(which.max(best.sum$adjr2),best.sum$adjr2[which.max(best.sum$adjr2)],
       cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

#  conclude best p=17 since close to lowest BIC and AIC, highest adjR2 
#  fit best model for p=17

xvarm=names(coef(best.mods,id=17))[2:18]
concrete.best=concrete[,c("strength",xvarm)]
lmod=lm(strength~.,data=concrete.best)
par(mfrow=c(2,2))
fit.best=predict(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="SecondOrder_BestSubset_OLS.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.best,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()
summary(lmod)
vif(lmod)
mean((concrete.best$strength-predict(lmod,concrete))^2)

# forward selection
# start with intercept, add predictor that produces lowest RSS
# take that one predictor model and add the predictor from the
# remaining predictors that produces the lowest RSS
# continue until all 44 predictors have been added

fwd.mods=regsubsets(strength~.,data=concrete,nvmax=44,method="forward")
fwd.sum=summary(fwd.mods)
fwd.sum

# can use criterion to select best model
aic=fwd.sum$bic+2*p-log(dim(concrete)[1])*p
which.max(fwd.sum$adjr2)
which.min(fwd.sum$bic)
which.min(aic)

# plot criteria to get visual confirmation

par(mfrow=c(2,2))
plot(p,aic,pch=19,type="b",main="AIC",xlab="p",ylab="AIC")
points(which.min(aic),aic[which.min(aic)],cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

plot(p,fwd.sum$bic,pch=19,type="b",main="BIC",xlab="p",ylab="BIC")
points(which.min(fwd.sum$bic),fwd.sum$bic[which.min(fwd.sum$bic)],
       cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

plot(p,fwd.sum$adjr2,pch=19,type="b",main="adj-R2",xlab="p",ylab="ADJ_R2")
points(which.max(fwd.sum$adjr2),fwd.sum$adjr2[which.max(fwd.sum$adjr2)],
       cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

# This time the BIC says p=16, and the other two AIC = 38 and ADJ R2 = 41
# check to see what best p=16 model is for best and fwd

coef(best.mods,id=16)
coef(fwd.mods,id=16)

#  fit best model for p=16

xvarm=names(coef(fwd.mods,id=16))[2:17]
concrete.best=concrete[,c("strength",xvarm)]
lmod=lm(strength~.,data=concrete.best)
par(mfrow=c(2,2))
fit.best=predict(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="SecondOrder_BestSubset_Fwd__OLS.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.best,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()
summary(lmod)
vif(lmod)
mean((concrete.best$strength-predict(lmod,concrete))^2)



# backward selection
# start with full model, remove predictor that produces lowest RSS
# take that n predictor model and remove the predictor from the
# model that produces the lowest RSS
# continue until all but one of the predictors remain

bkwd.mods=regsubsets(strength~.,data=concrete,nvmax=44,method="backward")
bkwd.sum=summary(bkwd.mods)
bkwd.sum

aic=bkwd.sum$bic+2*p-log(dim(concrete)[1])*p
which.max(bkwd.sum$adjr2)
which.min(bkwd.sum$bic)
which.min(aic)

# plot criteria to get visual confirmation

par(mfrow=c(2,2))
plot(p,aic,pch=19,type="b",main="AIC",xlab="p",ylab="AIC")
points(which.min(aic),aic[which.min(aic)],cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

plot(p,bkwd.sum$bic,pch=19,type="b",main="BIC",xlab="p",ylab="BIC")
points(which.min(bkwd.sum$bic),bkwd.sum$bic[which.min(bkwd.sum$bic)],
       cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

plot(p,bkwd.sum$adjr2,pch=19,type="b",main="adj-R2",xlab="p",ylab="ADJ_R2")
points(which.max(bkwd.sum$adjr2),bkwd.sum$adjr2[which.max(bkwd.sum$adjr2)],
       cex=1.5,col="red",lwd=2)
abline(v=c(1:9),lty=3)

# This time the BIC says p=25, and the other two like AIC = 37 and ADJ R2 = 39
# check to see what best p=25 model is for best and fwd

coef(best.mods,id=25)
coef(bkwd.mods,id=25)

#  fit best model for p=25

xvarm=names(coef(bkwd.mods,id=25))[2:26]
concrete.best=concrete[,c("strength",xvarm)]
lmod=lm(strength~.,data=concrete.best)
par(mfrow=c(2,2))
fit.best=predict(lmod)
fitted_values=lmod$fitted.values
strength=concrete$strength
png(file="SecondOrder_BestSubset_Bwd_OLS.png.png",width=8,height=8,units="in",res=300, pointsize=12)
plot(fitted_values,strength,pch=19,col="blue")
points(fit.best,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
dev.off()
summary(lmod)
vif(lmod)
mean((concrete.best$strength-predict(lmod,concrete))^2)

#########################################################
#                                                       #
#                     Random Forest                     #
#                                                       #
#########################################################


require(caret)
require(MASS)
require(randomForest)
concrete=read.csv("concrete.csv")
attach(concrete)

# tune model parameter mtry using caret
control=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid=expand.grid(mtry=c(1:8))
rf_gridsearch=train(strength~.,data=concrete, method="rf", metric="RMSE", 
                    tuneGrid=tunegrid, trControl=control)

print(rf_gridsearch)
plot(rf_gridsearch)

set.seed(123)
rf.mod=randomForest(strength~.,data=concrete,mtry=5, ntree=1000, 
                    importance=T)
rf.mod
plot(rf.mod)

varImpPlot(rf.mod,type=1,pch=19)
importance(rf.mod)


# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete)
plot(lmod$fitted.values,concrete$strength,pch=19,col="blue")
points(rf.mod$predicted,concrete$strength,col="red",lwd=2)
abline(a=0,b=1)
