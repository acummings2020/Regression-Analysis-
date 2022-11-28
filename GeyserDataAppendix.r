#####Geyser data code appendix 
#####please see word document for results


# read in Geyser data and set feature names, looking at using duratin to predict waitime until next eruption
gdata=read.table(file.choose(), header = T)
duration=gdata$Duration_min
waittime=gdata$waitingtime_min
height=gdata$Height
gdata.lmdur=lm(waittime~duration, data=gdata)
summary(gdata.lmdur)

#probability at X==4 # plot confidence intervals when waitime is 4 minutes and 2 minutes
geyser4waittime<-data.frame(duration=4)
pred.prob<-predict(gdata.lmdur, geyser4waittime)
pred.prob
predict(gdata.lmdur,geyser4waittime,interval="predict")
predict(gdata.lmdur,geyser4waittime,interval="confidence")
#probability at X==2
geyser2waittime<-data.frame(duration=2)
pred.prob<-predict(gdata.lmdur, geyser2waittime)
pred.prob
predict(gdata.lmdur,geyser2waittime,interval="predict")
predict(gdata.lmdur,geyser2waittime,interval="confidence")

geyser.lmheight=lm(waittime~duration+height, data=gdata)
summary(geyser.lmheight)

#check intervals with stepwise subset selection
fit1=lm(waittime~height+duration, data=gdata)
fit2=lm(waittime~1, data=gdata)
app.step.fw=step(fit2, direction="forward", scope=list(upper=fit1, lower=fit2))
#create new variable dfrom both height and duration and look at summary statistics
volume=duration*height
gdata.lmvol=lm(waittime~volume, data=gdata)
summary(gdata.lmvol)
#map model such that we can compare all 3 features now
gdata.lmall3=lm(waittime~duration+height+volume, data=gdata)
summary(gdata.lmall3)
data.less3=gdata[gdata$Duration_min<3,]
data.more3=gdata[gdata$Duration_min>=3,]
boxplot(data.more3$waitingtime_min)
boxplot(data.less3$waitingtime_min)
xless=data.less3$Duration_min
xmore=data.more3$Duration_min
wmore=data.more3$waitingtime_min
wless=data.less3$waitingtime_min
gdata.lmless=lm(wless~xless, data=data.less3)
gdata.lmmore=lm(wmore~xmore, data=data.more3)