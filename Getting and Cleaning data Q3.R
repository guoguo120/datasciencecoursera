Q1.
# install.packages("RCurl")
library(RCurl)
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
x <- getURL(URL)
## Or 
## x <- getURL(URL, ssl.verifypeer = FALSE)
out <- read.csv(textConnection(x))
str(out)
##load package dplyr
library(dplyr)
##filter household >10, sold >10,000
##agricultureLogical<-filter(out,ACR==3&AGS==6)
##or
agricultureLogical<-out$ACR[] == 3 & out$AGS[] == 6
which(agricultureLogical)

Q2.
##read jpg picture and find the 30th and 80th quantile
library(jpeg)
library(RCurl)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg " 
download.file(fileUrl,destfile="./data/Jeff.jpg",method="curl")
Jeff<- readJPEG("./data/Jeff.jpg",TRUE)
quantile(Jeff,c(0.3,0.8))

Q.3-5
## Q3. Gross Domestic Product & education--clean the data frame
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv " 
download.file(fileUrl,destfile="./data/GDP.csv",method="curl")
GDP<- read.csv("./data/GDP.csv", stringsAsFactors=FALSE, header=FALSE)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv  " 
download.file(fileUrl,destfile="./data/EDU.csv",method="curl")
EDU<- read.csv("./data/EDU.csv",stringsAsFactors=FALSE)
## GDP data are actually between row 6 and 331, column 1,2,34,5. Sort and clean the data
GDP2<-GDP[6:331,c(1,2,4,5)]
names(GDP2)<-c("CountryCode","Ranking","Economy","Grossdp")
GDP2$Ranking<-as.integer(GDP2$Ranking)
GDP2$Grossdp<-as.integer(GDP2$Grossdp)
GDPbyEDU<-merge(GDP2,EDU,"CountryCode")
##since only mathced countries have the ranking, count the matched country by 
colSums(!is.na(GDPbyEDU)) ## and look at the value of the Ranking column
## arrange the table in decending ranking
GDPbyEDU_Rank<-arrange(GDPbyEDU,desc(Ranking))
## look at the 13th country
GDPbyEDU_Rank[13,]

## now let's remove the unmatched items
GDPbyEDU_Rank<-GDPbyEDU_Rank[1:189,]

## Q4. find the average rank by income group
byIncome<-group_by(GDPbyEDU_Rank,Income.Group)
SumIncome<-summarize(byIncome,mn=mean(Ranking,na.rm=TRUE))

## Q5. Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. 
GDPbyEDU_Rank$qR <- cut (GDPbyEDU_Rank$Ranking, 
                   breaks = quantile (GDPbyEDU_Rank$Ranking, c (0, .20, .40, .60, .80, 1)), 
                   include.lowest = TRUE)
SumqR<-dcast(GDPbyEDU_Rank,qR~Income.Group)
