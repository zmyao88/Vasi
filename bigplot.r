dat <- read.csv("~/Documents/prof.vaso/data/AggregatedMetroTimeSeries_11.12.12.csv")
attach(dat)
require(ggplot2)
require(reshape)
require(plyr)
require(gridBase)

sz <- dlply(dat, .(MetroName_Google), transform)#, function(X)

pdf("~/Documents/prof.vaso/maybe.pdf",width=10, height=6)
for (i in 1:191)
{
    dat1 <- sz[[i]]
    dat1$log_ArrestsNumber_first <- log(dat1$ArrestsNumber_first+1)
    for(i in 2:76)
    {   
        dat1$Facebook_dummy_sum[i] <- dat1$Facebook_dummy_sum[i]+dat1$Facebook_dummy_sum[i-1]   
        dat1$Twitter_dummy_sum[i] <- dat1$Twitter_dummy_sum[i]+dat1$Twitter_dummy_sum[i-1]
        dat1$Website_dummy_sum[i] <- dat1$Website_dummy_sum[i]+dat1$Website_dummy_sum[i-1]
        dat1$Protest_dummy_sum[i] <- dat1$Protest_dummy_sum[i]+dat1$Protest_dummy_sum[i-1]
    }
    
    
    dat2 <- melt(dat1, id.var=c("TIME","MetroName_Google"), measure.vars=c("Facebook_dummy_sum","Twitter_dummy_sum",
                    "Website_dummy_sum","log_ArrestsNumber_first"))
    dat3 <- melt(dat1, id.var=c("TIME","MetroName_Google"), measure.vars=c("Protest_dummy_sum"))
    
    
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2,1)))
    vplayout <- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
    p1 <- qplot(x=TIME,y=Google_Occupyonlyall,data=dat1[dat1$TIME <33,],geom=c("point","line"),xlab=NULL, ylab="Google_Search")+
        opts(axis.text.x=NULL,title=paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy)))+
        geom_vline(xintercept=c(10,16), linetype="dotted",color="black")+scale_x_continuous(breaks=seq(0,32,by=1))
    p2 <- ggplot(dat2[dat2$TIME<33,],aes(x=TIME, y=value)) +
        geom_bar(aes(fill=factor(variable),width=0.5),color="black",stat="identity",position="dodge") +
        opts(legend.position=c(.1, .7))+scale_x_continuous(breaks=seq(0,32,by=1))
        geom_vline(xintercept=c(10,16), linetype="dotted",color="black")
    
    
    #ggplot(dat2,aes(x=TIME, y=value)) + geom_line(aes(color=variable)) + opts(legend.position=c(.1, .85))
    #p3 <- ggplot(dat3) + geom_bar(aes(x=TIME, y=value,fill="Protest_dummy_sum",width=0.5 ,space=8), stat="identity")+
     #   opts(legend.position=c(.1,.85))
    
    print(p1,vp = vplayout(1,1))
    print(p2,vp = vplayout(2,1))
}
dev.off()