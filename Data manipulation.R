require(gdata)


###### Load in Google general
google.general <- read.xls("D:/Rlab/Vasi's Project/Proj 2/GoogleInsight_General_11.02.12.xlsx", pattern="Day")
google.general[,1] <- as.Date(google.general[,1], "%Y-%m-%d")

###### Load in google restriced and merge state with metro
google.restricted <- read.xls("D:/Rlab/Vasi's Project/Proj 2/GoogleInsight_Restricted_11.02.12.xlsx",sheet=1)
google.restricted <- google.restricted[, c(1,2,4)]
names(google.restricted)[2] <- "Metro"
google.restricted$Date <- as.Date(google.restricted$Date,"%Y-%m-%d")


state.metro <- read.xls("D:/Rlab/Vasi's Project/Proj 2/GoogleInsight_Restricted_11.02.12.xlsx",sheet=2, pattern="STATE")
state.metro <- state.metro[,c(1:2)]
names(state.metro) <- c("State", "Metro")

# the state vs metro list is not correct
google.restricted2 <- merge(google.restricted, state.metro, by.x="Metro",by.y="Metro")


####### Load in cross sectional
Cross.sect <- read.csv("D:/Rlab/Vasi's Project/Proj 2/CrossSectional_11.02.12.csv", 
                       header = T, na.strings = c("#NULL!"),stringsAsFactors=F)

####### LOad in FaceBook & twitter
Facbk <- read.csv("D:/Rlab/Vasi's Project/Proj 2/FacebookTwitterETC_11.02.12.csv", 
                  header = T, na.strings = c("#NULL!"), stringsAsFactors=F)
for (i in 7:ncol(Facbk))
{
    Facbk[,i] <- as.Date(Facbk[,i], "%d-%b-%y")
}


##### Check the correctness
a2<-unique(google.general[,3:4])
uniq.val.general<- a2[order(a2[,1]),]
head(uniq.val.general)

a3 <- unique(state.metro)
uniq.val.stat.mtro <- a3[order(a3[,2]),]
head(uniq.val.stat.mtro,20)

a4 <- unique(Cross.sect[,c(2,4)])
uniq.val.cross.sect <- a4[order(a4[,2]),]
head(uniq.val.cross.sect,20)

# Huntsvill has no metro name in cross.sec



######### 
# Only include date from Sep 15 to day 77
start <- as.Date("2011-09-15", "%Y-%m-%d")
end <- start + 76
end2 <- as.Date(76,origin = "2011-09-15")
google.restricted2 <- google.restricted[google.restricted[,3] <= end & google.restricted[,3] >= start, ]
google.restricted2$Time <- b1[ , 3]-start + 1 


radical_metro_list <- Cross.sect[which(Cross.sect[,ncol(Cross.sect)] > 0), 4]
radical_metro_list <- data.frame(Metro = radical_metro_list, Index=c(1))
radical_metro_list <- unique(radical_metro_list)
#attach(radical_metro_list)
#radical_metro_list <- radical_metro_list[order(MetroName_Google, St), ]
#detach(radical_metro_list)
head(radical_metro_list)

head(google.restricted2)
google.restricted3 <- merge(google.restricted2, radical_metro_list, by="Metro",all.x=T, all.y=F)
google.restricted3[is.na(google.restricted3$Index),5] <- 0

# Funny thing here: there citys in different Metro with same name. However FACEBOOK data only has city name
State_City_Metro <- unique(Cross.sect[,2:4])
State_City_Metro <- State_City_Metro[order(State_City_Metro[,2]),]

####### Start to combine datasets
# Merge Facbook with State_Ctiy_Metro
Facbk2 <- merge(State_City_Metro, Facbk, by=c("St","City"),all.x=T)

# Merge Google_Restridcted with FaceBook2
b2 <- merge(google.restricted3, Facbk2, by.x="Metro", by.y="MetroName_Google", all.x=T, all.y=F )



##### Download Occupy arrest data
require(XML)
require(plyr)
require(stringr)
require(gdata)
Arrest <- readHTMLTable("http://stpeteforpeace.org/occupyarrests.sources.html", 
                        trim=T,which=7,as.data.frame=T, skip.rows=1, 
                        colClasses=c("character", "character", "numeric", "character", "character"))
names(Arrest)[1:4] <- c("Date","City","Arrest","Discript")
head(Arrest)
Arrest[,1] <- as.Date(Arrest[,1],"%m/%d/%Y")

Temp_arrest_city <- str_split(Arrest[,2], ", ") 
City <- sapply(Temp_arrest_city, FUN=function(x) x[1])
State <- sapply(Temp_arrest_city, FUN=function(x) x[2])
Arrest2 <- as.data.frame(cbind(Arrest,State,City))
Arrest2 <- Arrest2[,c(1,3,6:7)]

# Some errors here
Arrest4 <- Arrest2[order(Arrest2[,4],Arrest2[,1]), ]
Arrest3 <- merge(Arrest2, State_City_Metro[1:2], by="City", all.x=F, all.y=F)





?readHTMLTable
?merge
?match



