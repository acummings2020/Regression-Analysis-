

##read in health data with the goal of predicting PSA levels(prostate specific antigen), to help identify cancer risk

##find best subset of features to use, justifying with investigation into residuals
pdata=read.table(file.choose(), header= T)
y=pdata$PSA
X1=pdata$CVol
X2=pdata$Wgt
X3=pdata$Age
X4=pdata$GS
Z1=pdata$CVol
Z2=pdata$SVI
Z3=pdata$CPen
Z4=pdata$GS
pdata.lm1=lm(y~X1+X2+X3+X4, data=pdata)
pdata.lm2=lm(y~Z1+Z2+Z3+Z4, data=pdata)
outlierTest(pdata.lm1)
outlierTest(pdata.lm2)
par(mfrow=c(1,1))
residuals1=pdata.lm1$residuals
residuals2=pdata.lm2$residuals
fittedvalues1=pdata.lm1$fitted.values
fittedvalues2=pdata.lm2$fitted.values
betaestimates1=pdata.lm1$coefficients
betaestimates2=pdata.lm2$coefficients

qqPlot(pdata.lm1)
qqPlot(pdata.lm2)
fit1=lm(y~Z1+Z2+Z3+Z4, data=pdata)
fit2=lm(y~1, data=pdata)


app.step.fw=step(fit2, direction="forward", scope=list(upper=fit1, lower=fit2))

leapssubsets=regsubsets(y~Z1+Z2+Z3+Z4, data=pdata)
plot(leapssubsets, scale="adjr2")

subsets(leapssubsets, statistic="adjr2", main="Adjusted R^2 plot" , legend= FALSE, min.size=1)

subsets(leapssubsets, statistic="cp", main="Cp plot for all subset regression", legend=FALSE, min.size=1)

models=regsubsets(y~Z1+Z2+Z3+Z4, data=pdata, nvmax = 4,nbest=4)

res.sum=summary(models, all.best=TRUE, matrix=T)
res.sum=summary(models)
res.sum

cbind(res.sum$which, res.sum$adjr2,res.sum$cp, res.sum$bic)
pdata.lm3=lm(y~Z1+Z2+Z4, data=pdata)
outlierTest(pdata.lm3)
pdata2=read.table(file.choose(), header= T)
pdata.lm3=lm(y~Z1+Z2+Z4, data=pdata)

data.yesSV=pdata[pdata$SVI>0,]
data.noSV=pdata[pdata$SVI<=0,]
y1=data.yesSV$PSA
X1=data.yesSV$CVol
X2=data.yesSV$Wgt
X3=data.yesSV$Age
X4=data.yesSV$GS
Z1=data.yesSV$CVol
Z2=data.yesSV$SVI
Z3=data.yesSV$CPen
Z4=data.yesSV$GS
y11=data.yesSV$PSA
X11=data.yesSV$CVol
X22=data.yesSV$Wgt
X33=data.yesSV$Age
X44=data.yesSV$GS
Z11=data.yesSV$CVol
Z22=data.yesSV$SVI
Z33=data.yesSV$CPen
Z44=data.yesSV$GS

p1data1=lm(y1~X1+X2+X3+X4)
p1data2=lm(y1~Z1+Z2+Z3+Z4)
p0data1=lm(y11~X11+X22+X33+X44)
p0data2=lm(y11~Z11+Z22+Z33+Z44)
summary(p1data2)
summary(p0data1)
summary(p0data2)