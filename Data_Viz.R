require(googleVis)
require(gdata)
require(reshape)
require(RColorBrewer)
require(ggplot2)
# load in data 
theData <- read.xls("D:/Rlab/Vasi's Project/RDD_2.xlsx", header=T, stringsAsFactors=F)
plotData <- data.frame(time=theData[,4], metro=theData[,3], 
                       state=theData[,2], log.arrest=theData[,5], 
                       google=theData[,6])
plotData$arrest <- exp(plotData$log.arrest)

# subset data and select unique data based on time & metro
plotData <- plotData[!duplicated(plotData[,1:2]),]
row.names(plotData) <- NULL
View(plotData)

##################
###Motion Chart###
##################

# plug into google_Viz for ploting
M <- gvisMotionChart(plotData, idvar="metro", timevar="time")
plot(M)

##############
###Heat Map###
##############

# resture the data
names(plotData)
dat1 <- melt(data = plotData, id.vars = c("metro", "time"), measure.vars = "google") 
dat3 <- dat1
dat3$variable <- NULL
dat2 <- cast(data = dat1, metro~time)
dat2 <- as.data.frame(dat2)
row.names(dat2) <- dat2[,1]

plot.mtx <- as.matrix(dat2[,2:ncol(dat2)])
plot_matrix <- plot.mtx[1:50,]

myColors <- colorRampPalette(brewer.pal(11, "Spectral"))


heatmap1 <- heatmap(plot_matrix, Rowv=NA, Colv=NA,
                    col = myColors(50), scale="col", margins=c(2,8))

####################################################
####### the new cooler design with ggplot2 #########
####################################################


dat1 <- melt(data = plotData, id.vars = c("state","metro", "time"), measure.vars = "google") 
dat3 <- dat1
dat3$variable <- NULL
head(dat3)

myColors <- colorRampPalette(brewer.pal(11, "Spectral"))

###### print all state in one heat_map #####

pdf("D:/Rlab/Vasi's Project/heat_map.pdf", height=30, width=20)

zp1 <- ggplot(dat3)
zp1 <- zp1 + geom_tile(aes(x = time, y = metro, fill = value)) + 
    scale_fill_gradientn(colours = myColors(100)) + 
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) + 
    coord_equal() + theme_bw()
print(zp1)

dev.off()

###### print citys with respect to states #####
pdf("D:/Rlab/Vasi's Project/heat_map_facet.pdf", height=25, width=25)

zp2 <- zp1 + facet_wrap(~state,ncol=3,scales = "free_y")
print(zp2)

dev.off()