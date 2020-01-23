library(readxl)
dat <- read_excel("C:/Users/yzhyu/Downloads/Real estate valuation data set (1).xlsx")
colnames(dat)<-c("No","x1","x2","x3","x4","x5","x6","y")
dat
x4_data=data.frame(table(dat$`X4 number of convenience stores`))
x4_data$Var1=as.numeric(x4_data$Var1)
barplot(x4_data$Freq,as.numeric(x4_data$Var1),main= "Barplot number of stores near houses",xlab="number of stores(1-11)", ylab = "number of houses",col=rainbow(11))
summary(dat$`X2 house age`)
summary(dat$`X3 distance to the nearest MRT station`)
summary(dat$`X5 latitude`)
summary(dat$`X6 longitude`)


#model selection
mod_1<-lm(y~x2+x3+x4+x5+x6,data = dat)
summary(mod_1)
car::vif(mod_1)
selectedMod<-step(mod_1)

summary(selectedMod)
all_vifs <- car::vif(selectedMod)
all_vifs



levels(factor(dat$x1))

mod_1<-lm(y~x2+x3+x4+x5+x6,data = dat)
summary(mod_1)
selectedMod<-step(mod_1)#use stepwise to find the best model from the full model we set
summary(selectedMod)
all_vifs <- car::vif(selectedMod)

summary(selectedMod)
all_vifs
#correlation matrix
cor(dat)[,2:6]

car::vif(mod_1)
library(perturb) # to calculate condition index:see if k>=30
colldiag(selectedMod)
## after eliminating x6 from the model, vifs have become normal(below 30)
## no more multicollinearity.
colldiag(mod_2)
#full model:x2-x6 all contribute to the y.
mod_1<-lm(y~x2+x3+x4+x5+x6,data = dat)
summary(mod_1)
anova(mod_1)
mod_2<-lm(y~x2+x3+x4+x5,data = dat)
summary(mod_2)

#build interaction plot for x5,x6,y number of levels=3
dat$y_new=rep(0,414)
summary(dat$y)
dat$y_new[dat$y<28&dat$y>0]=1
dat$y_new[dat$y<47&dat$y>=28]=2
dat$y_new
dat$y_new[dat$y>=47]=3
summary(dat$x5*10000)
summary(dat$x6*10000)
dat$x5n=rep(0,414)
dat$x5n[dat$x5<24.9630]=1
dat$x5n[dat$x5<24.9775&dat$x5>=24.9630]=2
dat$x5n
dat$x5n[dat$x5>=24.9775]=3
dat$x6n=rep(0,414)
dat$x6n[dat$x6<121.5281]=1
dat$x6n[dat$x6<121.5433&dat$x6>=121.5281]=2
dat$x6n
dat$x6n[dat$x6>=121.5433]=3
#dat_ynew(1,2,3) correspond to unit house price (low, medium, high)

interaction.plot(dat$x5n,dat$x6n,dat$y_new)

##test normality by ks test

e<-mod_2$residuals 
ks.test(e,"pnorm",0,sd(e))
plot(mod_2)#normality:questionable since two tails are not following line , homoscedasticity: check


#boxplot for y vs x7 each quarter of year
#one of reasons we dont study effect of x1 on y
x7<-factor(dat$x1)
levels(x7)=c(3,4,4,4,1,1,1,2,2,2,3,3)
dat$x7<-x7
mod_1<-lm(y~x7+x2+x3+x4+x5+x6,data = dat)
mod_1
plot(x7,dat$y,notch= T, main= "Unit house price vs. Season", xlab="Season", ylab="unit house price",col=rainbow(4))
plot(dat$x1,dat$y)

#save
dat$x8<-as.numeric(x7)
mod_2<-lm(y~x8+x2+x3+x4+x5+x6,data = dat)
summary(mod_2)
mod_3<-lm(y~x1+x2+x3+x4+x5,data = dat)
summary(mod_3)

