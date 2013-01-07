require(gdata)
require(plyr)
require(reshape)
require(ggplot2)
dat <- read.csv("D:/Rlab/Vasi's Project/AggregatedMetroTimeSeries_11.12.12.csv", 
                header=T, na.strings=c("NA","#DIV/0!","#NULL!"))

sz <- dlply(dat, .(MetroName_Google), transform, .parallel=T)
View(sz[[2]])

plot(sz[[1]]$TIME,sz[[1]]$ArrestsNumber_first)

pdf("C:/Users/Zaiming/Documents/GitHub/Vasi/maybe.pdf",width=10, height=6)
for (h in 1:191)
{
    dat1 <- sz[[h]]
    dat1$log_ArrestsNumber_first <- log(dat1$ArrestsNumber_first+1)
    for(j in 2:76)
    {   
        dat1$Facebook_dummy_sum[j] <- dat1$Facebook_dummy_sum[j]+dat1$Facebook_dummy_sum[j-1]   
        dat1$Twitter_dummy_sum[j] <- dat1$Twitter_dummy_sum[j]+dat1$Twitter_dummy_sum[j-1]
        dat1$Website_dummy_sum[j] <- dat1$Website_dummy_sum[j]+dat1$Website_dummy_sum[j-1]
        #dat1$Protest_dummy_sum[j] <- dat1$Protest_dummy_sum[j]+dat1$Protest_dummy_sum[j-1]
    }
    dat1$Facebook_dummy_sum[dat1$Facebook_dummy_sum !=0] <- 1
    dat1$Twitter_dummy_sum[dat1$Twitter_dummy_sum !=0] <- 2
    dat1$Website_dummy_sum[dat1$Website_dummy_sum !=0] <-3
    #dat1$Protest_dummy_sum[dat1$Protest_dummy_sum !=0] <-4
    
    dat2 <- melt(dat1, id.var=c("TIME","MetroName_Google"), measure.vars=c("Facebook_dummy_sum","Twitter_dummy_sum",
                                                                           "Website_dummy_sum","Protest_dummy_sum"))
    dat3 <- melt(dat1, id.var=c("TIME","MetroName_Google"), measure.vars=c("Protest_dummy_sum"))
    
    cutoff <-76 #set cut off
    
    # design 2 WORKED!!!!
    result <- vector("list",3)
    k <- 0
    for (j in unique(dat2$variable))
    {
        k <- k+1
        data1 <- dat2[dat2$variable == j & dat2$TIME < cutoff, ]
        start=0
        end=0
        for (i in 1:nrow(data1))
        {
            if (data1$value[1] !=0)
            {
                start <- 1
            }else if (data1$value[i] != 0 && data1$value[i-1] ==0)
            {
                start <- data1$TIME[i]
            }
            if (data1$value[i] != 0)
            {
                end <- data1$TIME[i]
            }
        }
        rec_data<- as.data.frame(cbind(start, end))
        result[[k]] <- rec_data
    }
    
    
    
    
    yrng <- range(dat1[dat1$TIME <cutoff, ]$Google_Occupyonlyall)
    xrng <- range(dat1[dat1$TIME <cutoff, ]$TIME)
    
    p2 <- ggplot(data=dat1[dat1$TIME <cutoff, ], aes(x=TIME, y=Google_Occupyonlyall)) +
        geom_point() + geom_line() + 
        geom_point(data=dat1[dat1$TIME <cutoff, ][dat1$Protest_dummy_sum !=0,], aes(x=TIME, y=Protest_dummy_sum), col="red", alpha=1) +
        ggtitle(paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy))) +
        theme(axis.text.x=NULL, plot.title = element_text(size = rel(1))) + 
        geom_vline(xintercept=dat1[dat1$TIME <cutoff,][dat$ArrestsNumber_first!=0,3], linetype="dotted",color="black")+scale_x_continuous(breaks=seq(0,cutoff-1,by=1)) + 
        annotate("rect", xmin=result[[1]]$start, xmax=result[[1]]$end, ymin=0, ymax=yrng[2]/3, fill="green",alpha=0.5) +
        annotate("rect", xmin=result[[2]]$start, xmax=result[[2]]$end, ymin=yrng[2]/3, ymax=yrng[2]/3*2, fill="pink",alpha=0.5) +
        annotate("rect", xmin=result[[3]]$start, xmax=result[[3]]$end, ymin=yrng[2]/3*2, ymax=yrng[2], fill="blue",alpha=0.5)
    
    
  
    #####################
    
    
    print(p2)
}
dev.off()