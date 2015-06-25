#Jon Bakdash
#6/25/2015
#2.1 Prime data

#####Run once, installs required packages##########
install.packages("XLConnect")

#Loads the packages
library(XLConnect)

#Read in data

## Cleaned data
Prime<-readWorksheet(loadWorkbook("C:/Users/jonathan.z.bakdash/Desktop/T1_analysis/Prime/CLEAN data 2.1 prime 6-17-15 SP.xlsx"), sheet = 1)

#Error b/c value M3935 in Excel is  "#NA"
#Check to see if it is correct set to "NA"
#Prime$Initial.Proportial[3934]

##db proportions of freq 1 answer
#freq1props<-readWorksheet(loadWorkbook("M:/R/T1_analysis/Prime/CLEAN data 2.1 prime 6-17-15 SP.xlsx"), sheet = 2) 

#Create new variables

##Alternative answers (1 or 3)
NumAltTemp<-seq(3, length.out=dim(Prime)[1], by = 0)

for (i in 1:dim(Prime)[1]) 
{
  if(Prime$Alt2.Proportion[i]==999) {NumAltTemp[i]<-1}
} 

NumAlt<-NumAltTemp

#write.csv(Prime2, file="Prime2.csv")

Prime2<-data.frame(NumAlt, Prime)


##switch variable
Switch_to_alt<-seq(-1, length.out=dim(Prime)[1], by = 0)

### One Alt
for(i in 1:length(Prime2[,1])) 
{
  if(Prime2$NumAlt[i]==1) {
    if(Prime2$Initial.Answer[i]!=Prime2$Final.Answer[i]&&Prime2$Alt1.Answer [i]==Prime2$Final.Answer[i]) 
      Switch_to_alt[i]=1
    
    else Switch_to_alt[i]=0
  }}


### Three alt
for(i in 1:length(Prime2[,1]))    
{
  if(Prime2$NumAlt[i]==3) {
    if(Prime2$Initial.Answer[i]!=Prime2$Final.Answer[i]&&
       (Prime2$Alt1.Answer[i]==Prime2$Final.Answer[i]||
        Prime2$Alt2.Answer[i]==Prime2$Final.Answer[i]||
        Prime2$Alt3.Answer[i]==Prime2$Final.Answer[i]))
      Switch_to_alt[i]=1
    
    else Switch_to_alt[i]=0
  }}


Prime3<-data.frame(Switch_to_alt, Prime2)  

##Create new variable with Information Credibility 
SubjectvAlt<-seq(-1, length.out=dim(Prime)[1], by = 0)

for(i in 1:length(Prime3[,1])) 
{
  if(Prime3$NumAlt[i]==1) 
    SubjectvAlt[i]<- Prime3$Alt1.Proportion[i] - Prime3$Initial.Proportial[i]
  
  if(Prime3$NumAlt[i]==3&&Prime3$Switch_to_alt[i]==0) #No switch, take highest prop of alts
    SubjectvAlt[i]<-(max(Prime3$Alt1.Proportion[i], 
                         Prime3$Alt2.Proportion[i],
                         Prime3$Alt3.Proportion[i])-Prime3$Initial.Proportial[i])
  
  if(Prime3$NumAlt[i]==3&&Prime3$Switch_to_alt[i]==1) # Switch: Take prop of alt ans that matches fin ans 
    if(Prime3$Final.Answer[i]==Prime3$Alt1.Answer[i]) SubjectvAlt[i]<-(Prime3$Alt1.Proportion[i]-Prime3$Initial.Proportial[i])
    if(Prime3$Final.Answer[i]==Prime3$Alt2.Answer[i]) SubjectvAlt[i]<-(Prime3$Alt2.Proportion[i]-Prime3$Initial.Proportial[i])
    if(Prime3$Final.Answer[i]==Prime3$Alt3.Answer[i]) SubjectvAlt[i]<-(Prime3$Alt3.Proportion[i]-Prime3$Initial.Proportial[i])     
}

Prime4<-data.frame(SubjectvAlt, Prime3)   


# # Replace proportions = 999 with values from 'db proprtions of freq 1 answer' sheet    
# #(3) All the brand new answers not determined to be noise (999s that are left) will get updated with the proportion 
# #    of a frequency 1 answer from that question.
#     
#     #Alt.1 has no brand new answers
#     sum(Prime$Alt1.Proportion==999)
#     
#     #Alt.2 has new answers
#     Prime$Alt2.Proportion==999
#     
#     #Alt.3 has new answers
#     Prime$Alt3.Proportion==999
#     
#     #Alt2: Replace 999 with new proportion by matching Question.Number in Prime to QID in freq1props 
#     for (i in 1:dim(Prime)[1]) 
#     {if(Prime$Alt2.Proportion[i]==999) 
#       {for (j in 1:dim(freq1props)[1]) {if(Prime$Question.Number[i]==freq1props$QID[j]) 
#                                           {Prime$Alt2.Proportion[i]<-freq1props$Average.of.Proportion[j]}}
#       }
#         }
#    
#     
#     #Alt3: Replace 999 with new proportion by matching Question.Number in Prime to QID in freq1props 
#     for (i in 1:dim(Prime)[1]) 
#     {if(Prime$Alt3.Proportion[i]==999) 
#       {for (j in 1:dim(freq1props)[1]) {if(Prime$Question.Number[i]==freq1props$QID[j]) 
#                                           {Prime$Alt3.Proportion[i]<-freq1props$Average.of.Proportion[j]}}
#       }
#         }

#     
#     #For Alt2
#     #Print Question.Number if it could not be matched in the second sheet
#     for (i in 1:dim(Prime)[1]) 
#       {
#       if(Prime$Alt2.Proportion[i]==999) {print(Prime$Question.Number[i])}
#     }
#     
#     #For Alt3
#     #Print Question.Number if it could not be matched in the second sheet
#     for (i in 1:dim(Prime)[1]) 
#     {
#       if(Prime$Alt3.Proportion[i]==999) {print(Prime$Question.Number[i])}
#     }
#     
#Drop trials with missing questions

#1300 has 84 [5161 to 5244], the rest have 120
#
UniqueID1<-rep(1:43, each=120)
UniqueID2<-rep(44, each = 84)
UniqueID3<-rep(45, each = 120) 
UniqueID<-c(UniqueID1, UniqueID2, UniqueID3)

Prime5<-data.frame(UniqueID, Prime4)

#write.csv(Prime4, file="2.1_Prime_w_alt_switch_IC_fixed.csv")

#Drop Final.Accuracy == 9
PrimeFinal<-subset(Prime5, Final.Accuracy!=9)

