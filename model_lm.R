# loading the required package
require(plyr)
require(ggplot2)

#importing the data
dat <- read.csv("~/Documents/prof.vaso/RDD_2.csv")

#adding on column that specify whether a city response to the arrest stimulus
dat<-ddply(dat, .(MetroName_Google), transform, sel=(!GoogleOWS_percent_rec[13]==0))
attach(dat)

#preliminary check with the data
plot(GoogleOWS_percent_rec~TIME,pch=19,cex=0.2)

plot(LnArrestsNumber~TIME,pch=19,cex=0.2)

plot(GoogleOWS_percent_rec~LnArrestsNumber,pch=19,cex=0.2)

# point plot by the state and city
pdf("~/Documents/prof.vaso/state.plot.pdf",height=50, width=20)


ggplot(dat) + geom_point(aes(x=TIME,y=GoogleOWS_percent_rec, col=MetroName_Google,size=0.1))+
    facet_wrap(~MetroSt_Google, ncol=4)


dev.off()

######################



#forming a new data.frame to make the model, named it as "new_data"
new_data <- as.data.frame(cbind(TIME,GoogleOWS_percent_rec,sel))

#adding one column to the "new_data", which specify the time_period of each row, whether in 4~10, or in 11~16
new_data$time_period <- apply(new_data,1,function(x){
    
    if(x[1] <= 10) tp = 1
    if(x[1] > 10) tp = 2
    return(tp)
})
#adding one column to the "new_data", subtracting the period two time by 10
new_data$time_transformed <- apply(new_data,1,function(x){
    
    if(x[1] <= 10) tp = x[1]
    if(x[1] > 10) tp = x[1]-7
    return(tp)
})
detach(dat)
attach(new_data)

#model 1
mod1 <- lm(GoogleOWS_percent_rec ~ time_transformed + as.factor(time_period) 
           + time_transformed * as.factor(time_period), data = new_data[sel==1,])
#model 2
mod3 <- lm(GoogleOWS_percent_rec ~ time_transformed + as.factor(time_period) 
           + time_transformed * as.factor(time_period), data = new_data[sel==0,])
#model 3
mod2 <-lm(GoogleOWS_percent_rec ~ time_transformed, dat = new_data[sel==1 & time_period==2,])


ggplot(new_data)+geom_point(aes(x=time_transformed, y=GoogleOWS_percent_rec, col=as.factor(time_period)))

ggplot(new_data[new_data[,3]==2,])+geom_point(aes(x=time_transformed, y=GoogleOWS_percent_rec))







