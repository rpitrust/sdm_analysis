# Jon's R code for T1 analysis
# 7/21/2014
# R version 3.1.0 
# RStudio Version 0.98.953
# Zip file: CSVs final clean 4.22.14.zip
# 148 csv files; 10 unusable csv files (12, 24, 31, 36, 53, 57, 75, 80, 106, 143) placed in a separate directory

#Required R Packages
#require(plyr)
#require(data.table)
#require(gtools)
require(ggplot2)

# Import multiple csv files

########## Change folder  ############################################################################
setwd("M:/R/T1_analysis/4.22.14_DATA")
readinpath<-"M:/R/T1_analysis/4.22.14_DATA"

#Create list of filenames w/the path specified above: 
filenames <- list.files(path = readinpath, pattern =".csv")
pathfilenames<-paste(readinpath,filenames)

#Create list of headers
CSVHeaders <- read.csv(paste(c(readinpath,"/Headers/headers.csv"), collapse=''))

#Read CSV function
read.csvfiles<-function(filenam)
  {read.csv(filenam, 
            header = FALSE,
            col.names=paste(colnames(CSVHeaders), sep =""), #Add a header to each file, makes rbind work!
            stringsAsFactors=FALSE #Fix for the mountains of errors because of mixed numeric and text data: http://stackoverflow.com/questions/1632772/appending-rows-to-a-dataframe-the-factor-problem
            )
   }

# #Bind files into a big data frame
CSVDataFrame<-do.call(rbind, lapply(filenames, read.csvfiles))

#Sanity check: 148 good csv files (participants) with 120 rows and 45 columns = 17760 rows x 45 columns
dim(CSVDataFrame)
head(CSVDataFrame)

#Drop misspelled final answers, fin_acc = 9, but keep fin_acc = 0 or 1
SDM_cleaned<-subset(CSVDataFrame, fin_acc!=9) #17390 rows
hist(SDM_cleaned$fin_acc)

#Logistic regression plots
#Final accuracy by stars
ggplot(SDM_cleaned, aes(stars, fin_acc, group=factor(ProgID))) + 
  geom_point() +
  stat_smooth(method="glm", family = "binomial", SE = F)


ggplot(SDM_cleaned, aes(num_pts, fin_acc, color=factor(ProgID))) + geom_line() 

ggplot(SDM_cleaned, aes(x=num_pts, y=fin_acc, colour=factor(ProgID))) + 
  geom_line() +
  stat_smooth(aes(y=fin_acc), method = "glm", family = "binomial", se=T)


ggplot(SDM_cleaned, aes(x=num_pts, y=fin_acc)) + geom_line(aes(group=time_pres)) + 
  stat_smooth(method="glm", family="binomial", se=TRUE)


