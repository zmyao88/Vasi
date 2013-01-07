require(gdata)
require(plyr)
require(reshape)
require(ggplot2)
dat <- read.csv("D:/Rlab/Vasi's Project/AggregatedMetroTimeSeries_11.12.12.csv", 
                header=T, na.strings=c("NA","#DIV/0!","#NULL!"))

sz <- dlply(dat, .(MetroName_Google), transform, .parallel=T)
View(sz[[2]])

plot(sz[[1]]$TIME,sz[[1]]$ArrestsNumber_first)

pdf("~/Documents/prof.vaso/maybe.pdf",width=10, height=6)
for (i in 1:191)
{
    dat1 <- sz[[3]]
    dat1$log_ArrestsNumber_first <- log(dat1$ArrestsNumber_first+1)
    for(i in 2:76)
    {   
        dat1$Facebook_dummy_sum[i] <- dat1$Facebook_dummy_sum[i]+dat1$Facebook_dummy_sum[i-1]   
        dat1$Twitter_dummy_sum[i] <- dat1$Twitter_dummy_sum[i]+dat1$Twitter_dummy_sum[i-1]
        dat1$Website_dummy_sum[i] <- dat1$Website_dummy_sum[i]+dat1$Website_dummy_sum[i-1]
        dat1$Protest_dummy_sum[i] <- dat1$Protest_dummy_sum[i]+dat1$Protest_dummy_sum[i-1]
    }
    dat1$Facebook_dummy_sum[dat1$Facebook_dummy_sum !=0] <- 1
    dat1$Twitter_dummy_sum[dat1$Twitter_dummy_sum !=0] <- 2
    dat1$Website_dummy_sum[dat1$Website_dummy_sum !=0] <-3
    dat1$Protest_dummy_sum[dat1$Protest_dummy_sum !=0] <-4
    
    dat2 <- melt(dat1, id.var=c("TIME","MetroName_Google"), measure.vars=c("Facebook_dummy_sum","Twitter_dummy_sum",
                                                                           "Website_dummy_sum"))
    dat3 <- melt(dat1, id.var=c("TIME","MetroName_Google"), measure.vars=c("Protest_dummy_sum"))
    
    cutoff <-33 #set cut off
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2,1)))
    vplayout <- function(x,y){viewport(layout.pos.row=x, layout.pos.col=y)}
    p1 <- qplot(x=TIME,y=Google_Occupyonlyall,data=dat1[dat1$TIME <cutoff,],
                geom=c("point","line"),xlab=NULL, ylab="Google_Search") + 
                    ggtitle(paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy))) +
                            theme(axis.text.x=NULL,plot.title = element_text(size = rel(1))) + 
                            geom_vline(xintercept=dat1[dat1$TIME <cutoff,][dat$ArrestsNumber_first!=0,3], linetype="dotted",color="red")+scale_x_continuous(breaks=seq(0,cutoff-1,by=1))
    
        
    # 1st design failed
    p <- ggplot(data=dat2[dat2$TIME < cutoff, ], aes(x=TIME, y=factor(variable))) + 
        geom_tile(aes(fill=factor(value)), alpha=0.5) + 
        scale_fill_manual(breaks=c(0,2,4,6,8), values = c("white", "green", "blue","orange")) +
        geom_point(data=dat1[dat1$TIME <cutoff,], aes(x=TIME, y=Google_Occupyonlyall)) + 
        geom_line(data=dat1[dat1$TIME <cutoff,], aes(x=TIME, y=Google_Occupyonlyall))+
        ggtitle(paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy))) +
        theme(axis.text.x=NULL, plot.title = element_text(size = rel(1))) + 
        geom_vline(xintercept=dat1[dat1$TIME <cutoff,][dat$ArrestsNumber_first!=0,3], linetype="dotted",color="red")+scale_x_continuous(breaks=seq(0,cutoff-1,by=1))
    ###########################################################################
    
    # design 2 WORKED!!!!
    result <- vector("list",3)
    k <- 0
    for (j in unique(dat2$variable))
    {
        k <- k+1
        data1 <- dat2[dat2$variable == j & dat2$TIME < 33, ]
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
        ggtitle(paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy))) +
        theme(axis.text.x=NULL, plot.title = element_text(size = rel(1))) + 
        geom_vline(xintercept=dat1[dat1$TIME <cutoff,][dat$ArrestsNumber_first!=0,3], linetype="dotted",color="black")+scale_x_continuous(breaks=seq(0,cutoff-1,by=1)) + 
        annotate("rect", xmin=result[[1]]$start, xmax=result[[1]]$end, ymin=0, ymax=yrng[2]/3, fill="green",alpha=0.5) +
        annotate("rect", xmin=result[[2]]$start, xmax=result[[2]]$end, ymin=yrng[2]/3, ymax=yrng[2]/3*2, fill="pink",alpha=0.5) +
        annotate("rect", xmin=result[[3]]$start, xmax=result[[3]]$end, ymin=yrng[2]/3*2, ymax=yrng[2], fill="blue",alpha=0.5)

    print(p2)    
        
        # doesn't work either because the geom_rec has too many layer and it's stacked togher
        geom_rect(data=result[[1]], aes(NULL,NULL,xmin=start, xmax=end, colour=NULL, group=NULL), 
             ymin=0,ymax=yrng[2]/3, fill="pink", alpha=0.3) + 
        geom_rect(data=result[[2]], aes(NULL,NULL,xmin=start, xmax=end, colour=NULL, group=NULL), 
             ymin=yrng[2]/3,ymax=yrng[2]/3*2, fill="blue", alpha=0.3) +
        geom_rect(data=result[[3]], aes(NULL,NULL,xmin=start, xmax=end, colour=NULL, group=NULL), 
             ymin=yrng[2]/3*2,ymax=yrng[2], fill="green", alpha=0.3)
        #####################
    
        
   
    ##### Not working :(   
    print(p)
    col <- c("white", "blue", "green", "orange")
    p2 <- ggplot(data=dat2[dat2$TIME<cutoff,] ,aes(x=TIME, y=factor(variable))) + 
        geom_tile(aes(fill=col[value],group=variable)) +
        scale_fill_identity(labels=letters[0:4], breaks=col) + 
        geom_point(data=dat1[dat1$TIME <cutoff,], aes(x=TIME, y=Google_Occupyonlyall))
    qplot(x=TIME,y=Google_Occupyonlyall,data=dat1[dat1$TIME <cutoff,],
          geom=c("point","line"),xlab=NULL, ylab="Google_Search") + 
              ggtitle(paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy))) +
              theme(axis.text.x=NULL,plot.title = element_text(size = rel(1))) + 
              geom_vline(xintercept=dat1[dat1$TIME <cutoff,][dat$ArrestsNumber_first!=0,3], linetype="dotted",color="red")+scale_x_continuous(breaks=seq(0,cutoff-1,by=1))

    
    col <- c("white", "blue", "green", "orange")
    p2 <- qplot(x=TIME, y=factor(variable), fill=col[value], data=dat2[dat2$TIME<cutoff,], geom="tile", group=variable) +
        scale_fill_identity(labels=letters[0:4], breaks=col) + 
        geom_point(data=dat1[dat1$TIME <cutoff,], aes(x=TIME, y=Google_Occupyonlyall))
        qplot(x=TIME,y=Google_Occupyonlyall,data=dat1[dat1$TIME <cutoff,],
            geom=c("point","line"),xlab=NULL, ylab="Google_Search") + 
            ggtitle(paste(unique(dat1$MetroName_Google)," Progressive Index = ", unique(dat1$IndexProg_Dummy))) +
            theme(axis.text.x=NULL,plot.title = element_text(size = rel(1))) + 
            geom_vline(xintercept=dat1[dat1$TIME <cutoff,][dat$ArrestsNumber_first!=0,3], linetype="dotted",color="red")+scale_x_continuous(breaks=seq(0,cutoff-1,by=1))
    
    
    print(p3)
    p2 <- ggplot(dat2[dat2$TIME<cutoff,],aes(y=TIME, x=factor(variable), fill=value)) +
        geom_bar(aes(fill=factor(variable),width=0.5),color="black",stat="identity",position="dodge") +
        coord_flip()
        geom_bar(aes(fill=factor(variable),width=0.5),color="black",stat="identity",position="dodge") +
        opts(legend.position=c(.1, .7))+scale_x_continuous(breaks=seq(0,cutoff-1,by=1))
    geom_vline(xintercept=c(10,16), linetype="dotted",color="black")
    
####################
    
    
    print(p1,vp = vplayout(1,1))
    print(p2,vp = vplayout(2,1))
}
dev.off()