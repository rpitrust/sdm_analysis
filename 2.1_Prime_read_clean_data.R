#Jon Bakdash
#6/25/2015
#2.1 Prime data

#####Run once, installs required packages##########
install.packages("XLConnect")

#Loads the packages
library(XLConnect)

#Read in data

## Cleaned data
Prime<-readWorksheet(loadWorkbook("M:/R/T1_analysis/Prime/CLEAN data 2.1 prime 6-17-15 SP.xlsx"), sheet = 1)

  #Error b/c value M3935 in Excel is  "#NA"
  #Check to see if it is correct set to "NA"
    #Prime$Initial.Proportial[3934]
    
##db proportions of freq 1 answer
    freq1props<-readWorksheet(loadWorkbook("M:/R/T1_analysis/Prime/CLEAN data 2.1 prime 6-17-15 SP.xlsx"), sheet = 2) 

# Replace proportions = 999 with values from 'db proprtions of freq 1 answer' sheet    
#(3) All the brand new answers not determined to be noise (999s that are left) will get updated with the proportion 
#    of a frequency 1 answer from that question.
    
    #Alt.1 has no brand new answers
    sum(Prime$Alt1.Proportion==999)
    
    #Alt.2 has new answers
    Prime$Alt2.Proportion==999
    
    #Alt.3 has new answers
    Prime$Alt3.Proportion==999
    
    #Alt2: Replace 999 with new proportion by matching Question.Number in Prime to QID in freq1props 
    for (i in 1:dim(Prime)[1]) 
    {if(Prime$Alt2.Proportion[i]==999) 
      {for (j in 1:dim(freq1props)[1]) {if(Prime$Question.Number[i]==freq1props$QID[j]) 
                                          {Prime$Alt2.Proportion[i]<-freq1props$Average.of.Proportion[j]}}
      }
        }
   
    
    #Alt3: Replace 999 with new proportion by matching Question.Number in Prime to QID in freq1props 
    for (i in 1:dim(Prime)[1]) 
    {if(Prime$Alt3.Proportion[i]==999) 
      {for (j in 1:dim(freq1props)[1]) {if(Prime$Question.Number[i]==freq1props$QID[j]) 
                                          {Prime$Alt3.Proportion[i]<-freq1props$Average.of.Proportion[j]}}
      }
        }
    
    
    #For Alt2
    #Print Question.Number if it could not be matched in the second sheet
    for (i in 1:dim(Prime)[1]) 
      {
      if(Prime$Alt2.Proportion[i]==999) {print(Prime$Question.Number[i])}
    }
    
    #For Alt3
    #Print Question.Number if it could not be matched in the second sheet
    for (i in 1:dim(Prime)[1]) 
    {
      if(Prime$Alt3.Proportion[i]==999) {print(Prime$Question.Number[i])}
    }
    
    