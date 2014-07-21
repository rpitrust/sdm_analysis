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
#setwd("M:/R/T1_analysis/4.22.14_DATA")
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

#Sanity check: 148 good csv files (participants) with 120 rows and 45 columns --> 17760 rows x 45 columns
dim(CSVDataFrame)
head(CSVDataFrame)

#Logistic regression plots; improper model b/c observation are repeated, not iid
#Overall and by participant
#Filter out practice
# Final accuracy (fin_acc) by time pressure (time_pres)
# Final accuracy (fin_acc) by final reaction time (RT4_fin)
# Final accuracy (fin_acc) by number of alternative answers (num_alt)

ggplot(CSVDataFrame, aes(x=time_pres, y= fin_acc)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

boxplot(CSVDataFrame$fin_acc)
outliers<-boxplot(CSVDataFrame$fin_acc)
outliers$out



