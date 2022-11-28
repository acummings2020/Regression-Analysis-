#initialize data from different mobile applications' usage and gender data for that usage


appdata1=read.csv(file.choose(), header = T)
summary(appdata1)
g=appdata1$gender
respid=appdata1$respid
app1=appdata1$app_1
app2=appdata1$app_2
app3=appdata1$app_3
app4=appdata1$app_4
app5=appdata1$app_5
app6=appdata1$app_6


fdata=appdata1[appdata1$gender>0,]
mdata=appdata1[appdata1$gender<=0,]

#initial logistic model including all predictor variables
appdata1.logit=glm(formula=gender~app1+app2+app3+app4+app5+app6, data=appdata1, family=binomial)


#backwards elimination and leapsubsets, R^2(a,p), BIC




leapssubsets=regsubsets(g~app1+app2+app3+app4+app5+app6, data=appdata1)
plot(leapssubsets, scale="adjr2")

subsets(leapssubsets, statistic="adjr2", main="Adjusted R^2 plot" , legend= False, min.size=1)

subsets(leapssubsets, statistic="cp", main="Cp plot for all subset regression", legend=FALSE, min.size=1)

models=regsubsets(g~app1+app2+app3+app4+app5+app6, data=appdata1, nvmax = 6,nbest=6)

res.sum=summary(models, all.best=TRUE, matrix=T)

res.sum=summary(models)
res.sum

cbind(res.sum$which, res.sum$adjr2,res.sum$cp, res.sum$bic)
#end of sunbet Cbind, adjr2, cp, bic
#step forward
fit1=glm(gender~app1+app2+app3+app4+app5+app6, data=appdata1, family=binomial)
fit2=glm(gender~1, data=appdata1, family=binomial)
app.step.fw=step(fit2, direction="forward", scope=list(upper=fit1, lower=fit2))
outlierTest(appdata1.logit)
#model of choice with variables
#look at predicting gender based on app usage 
appdata1.logit2=glm(formula=gender~app1+app2+app3+app4, data=appdata1, family=binomial)
exp(coef(appdata1/appdata1.logit2))
anova(appdata1.logit2, test="Chisq")
outlierTest(appdata1.logit2)
summary(appdata1.logit2)
residuals=appdata1.logit2$residuals
fittedvalues=appdata1.logit2$fitted.values
betaestimates=appdata1.logit2$coefficients

plot(app1,residuals)
plot(app2,residuals)
plot(app3, residuals)
plot(app4, residuals)
shapiro.test(residuals)
qqnorm(residuals)#normal prob. plot of residuals
qqline(residuals)
bptest(appdata1.logit2)

newdata=read.csv(file.choose(), header = T)
pred.prob=predict(appdata1.logit2$fitted.values, newdata, type="response")
pred.prob

fdata1=pred.prob[>.5]
mdata1=pred.prob[<=.5]