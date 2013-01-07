require(plyr)

dat1 <- read.csv("~/Documents/VASI/spectral analysis/OWSprotests_12.18.12.csv")
dat2 <- read.csv("~/Documents/VASI/spectral analysis/Population_LatitudeLongitude_12.18.12.csv")

#for the dat1, there are 948 different ID for cities(thus 948 different cities), however
#we found two cities that are very weird with their data they are 
dat1[dat1$City_FIPS=="4652980",]
dat1[dat1$City_FIPS=="3678608",]
#Thus we delete these two cities from the dat1 and dat2 and named the deleted data as dat12 and dat22
dat12 <- dat1[!(dat1$City_FIPS=="4652980" | dat1$City_FIPS=="3678608"),]
dat22 <- dat2[!(dat2$FIPS_city=="4652980" | dat2$FIPS_city=="3678608"),]

#the dat12 has 209 cities has protest(before deleting, there are 210 protest cities)
summary(ddply(dat12, .(City_FIPS), nrow) < 77)
#A better way to check this
lastValue <- function(x)   tail(x, 1)
sel <- ddply(dat12, .(City_FIPS), lastValue)
sum(sel$ows_protest)

#the cities in dat12 (946), there are 920 that we have longtitude and latitude
length(intersect(unique(dat12$City_FIPS),unique(dat22$FIPS_city)))

#we merged dat12 and dat22 by the City and St to check any ID between different dataset
dat3 <- merge(dat12, dat22, by = c('City', 'St'))
#it turns out the datasets are doing good with the ID, But:
#there is wrong code for the first day of city "Bessemer, AL", I changed it from 203000 to 105980
#and the following two line code states that the merged data frame are good
sum(!(dat3$City_FIPS==dat3$FIPS_city))

#there are 1232 unique cities in the dat22that have the longtitude and the latitude
length(unique(dat22[,"FIPS_city"]))


#So we need to concern about 1232 general cities and 209 protest cities

################################################################################

#the protest matrix
pro_mat <- as.data.frame(matrix(0, 209, 77))
rownames(pro_mat) <- sel$City_FIPS[sel$ows_protest==1]

for(i in 1:209)
{
    for(j in sel$TIME[sel$ows_protest==1][i]:77)
    {
        pro_mat[i,j]<-1
    }  
}

#to get the longtitue and latitude for the protested cities, which will be conveinient for the further code 
dis_pro <- as.data.frame(matrix(NA, 209, 2))
rownames(dis_pro) <- sel$City_FIPS[sel$ows_protest==1]
colnames(dis_pro) <- c("Latitude","Longitude")

a <- match(sel$City_FIPS[sel$ows_protest==1],dat22$FIPS_city)

##there are five protest cities that do not have the location

dis_pro[,1] <- dat22$Latitude[a]
dis_pro[,2] <- dat22$Longitude[a]

#the distance matrix

dis_mat <- matrix(NA, 209, 1232)
for(i in 1:209)
{
    lon1 <- dis_pro[i,2]/180*pi
    lat1 <- dis_pro[i,1]/180*pi
    for(j in 1:1232)
    {
        lon2 <- dat22$Longitude[j]/180*pi
        lat2 <- dat22$Latitude[j]/180*pi
        dis_mat[i,j] <- gcd.slc(lon1,lat1,lon2,lat2)
        
    }
    
}

dis_mat <- as.data.frame(dis_mat)
rownames(dis_mat) <- sel$City_FIPS[sel$ows_protest==1]
colnames(dis_mat) <- dat22$FIPS_city

##################################

bigresult <- function(protest, distance, threshold)
{
    
    mm <- apply(distance, 2, function(x) x<=threshold & x>2) 
    ms <- as.data.frame(matrix(NA, ncol(distance)*77, 1))
    for(i in 1:ncol(distance))
    {
        for(j in 1:77)
        {
            ms[(i-1)*77+j,1] <- sum(mm[,i] & as.logical(pro_mat[,j]),na.rm=T)
        }
    }
       return(ms)
}   
   
num15 <- bigresult(pro_mat, dis_mat, 15)
num30 <- bigresult(pro_mat, dis_mat, 30)
num45 <- bigresult(pro_mat, dis_mat, 45)
num60 <- bigresult(pro_mat, dis_mat, 60)
num100 <- bigresult(pro_mat, dis_mat, 100)

#get the rownames of the bigresult
row_names<- NULL
for(i in 1:ncol(dis_mat))
{
    row_names <- c(row_names,rep(colnames(dis_mat)[i],77))  
}

#the bigresult needs the first column as the protest date
protest <- as.data.frame(matrix(NA, ncol(dis_mat)*77, 1))
for(i in 1:ncol(dis_mat))
{
    cat(i)
    if (sum(colnames(dis_mat)[i]==rownames(dis_mat))==0) protest[((i-1)*77+1):(77*i),1] <- 0
    if (sum(colnames(dis_mat)[i]==rownames(dis_mat))==1) protest[((i-1)*77+1):(77*i),1] <- t(pro_mat[colnames(dis_mat)[i],])
}
sum(protest)

Time <- rep(c(1:77),ncol(dis_mat)) 
result <- cbind(row_names,Time,protest,num15,num30,num45,num60,num100)
colnames(result) <- c("FIPS_city", "Time", "OWS_protest", "15","30","45","60","100")

tempe <- dat22[,c("FIPS_city", "St", "City")]
result <- merge(result, tempe, by = "FIPS_city")
write.csv(result,"~/Documents/VASI/spectral analysis/bigresult.csv",row.names=F)




