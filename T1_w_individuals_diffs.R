# Jon's R code for T1 analysis
# Last updated 5/4/2015
# R version 3.2.0 
# RStudio Version 0.98.1103
# Zip file: Complete SDM2.1_1.20.15.zip, 162 csv files
# Individual diffs: SDM2.1_Indiv_diff_3.23.15.csv

#Required R Packages
require(ggplot2)
require(lme4)
require(boot)
#require(geepack)
require(MESS)
require(MASS)
require(RColorBrewer)
require(foreign)
require(plyr)
require(mefa)
require(Hmisc)
require(AICcmodavg)
require(car)
require(lsr)

# Import multiple csv files
########## Change folder  ############################################################################
setwd("M:/R/T1_analysis/1.20.15_DATA")
readinpath<-"M:/R/T1_analysis/1.20.15_DATA"

#Create list of filenames w/the path specified above: 
filenames<-list.files(path = readinpath, pattern =".csv")
pathfilenames<-paste(readinpath,filenames)

#Create list of headers
  CSVHeaders<-read.csv(paste(c(readinpath,"/Headers/headers.csv"), collapse=''))

#Read CSV function
  read.csvfiles<-function(filenam)
  {
    read.csv(filenam, 
             header = FALSE,
             col.names=paste(colnames(CSVHeaders), sep =""), #Add a header to each file, makes rbind work!
             stringsAsFactors=FALSE #Fix for the mountains of errors because of mixed numeric and text data: http://stackoverflow.com/questions/1632772/appending-rows-to-a-dataframe-the-factor-problem
             )
  }

#Bind files into a single data frame
  CSVDataFrame<-do.call(rbind, lapply(filenames, read.csvfiles))

#Sanity check: 162 csv files (participants) with 123 rows each:  19,926 rows x 46 columns
#dim(CSVDataFrame)
#head(CSVDataFrame)

#Create unique ID
  UniqueID<-rep(1:162, each=123)
  CSVDataFrame<-cbind(CSVDataFrame, UniqueID)  

#Import individual diffs csv and add to dataframe
  IndivDiffs <- read.csv("M:/R/T1_analysis/SDM2.1_Indiv_diff_3.23.15.csv")
  IndivDiffs$UniqueID<-as.numeric(IndivDiffs$UniqueID)

#Check for matched UniqueID between raw data and individual diffs 
#unique(CSVDataFrame$UniqueID)==IndivDiffs$UniqueID

#Replicate individual diff measures for each Ss 123 times, match to UniqueID
IndivDiffs.Long<-rep(IndivDiffs, each = 123)

#Check again 
#unique(CSVDataFrame$UniqueID)==unique(IndivDiffs.Long$UniqueID)

#Bind individual diffs to 
FullData<-cbind(CSVDataFrame, IndivDiffs.Long)

#Last check: ProgramIDs 
#unique(FullData$Ignored)==unique(IndivDiffs.Long$PID)

#Drops
#Drop Ss: UniqueID 46 (Lied on consent form about age)
FullData_dropSS<-subset(FullData, UniqueID!=46)

#Drop misspelled final answers, fin_acc = 9, but keep fin_acc = 0 or 1
FullData_dropmisspell<-subset(FullData_dropSS, fin_acc!=9)
#hist(FullData_dropmisspell$fin_acc)

#Drop init_prop=999 
#999 indicates subject's initial answer was disqualified from analyses of final accuracy.
SDM_cleaned<-subset(FullData_dropmisspell, Init_prop!=999)

#Subject switches to alt answer? Create new variable switch_to_alt (0 or 1). 
for(i in 1:length(SDM_cleaned[,1])) 
{
  if(SDM_cleaned$num_alt[i]==1)
    if(SDM_cleaned$Init_ans[i]!=SDM_cleaned$Fin_ans[i]&&SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i]) 
      SDM_cleaned$Switch_to_alt[i]=1
    
    else SDM_cleaned$Switch_to_alt[i]=0
}


for(i in 1:length(SDM_cleaned[,1]))    
{
  if(SDM_cleaned$num_alt[i]==3&&SDM_cleaned$Init_ans[i]!=SDM_cleaned$Fin_ans[i])
    if(SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i]||
       SDM_cleaned$Alt2_ans[i]==SDM_cleaned$Fin_ans[i]||
       SDM_cleaned$Alt3_ans[i]==SDM_cleaned$Fin_ans[i])
      SDM_cleaned$Switch_to_alt[i]=1
    
    else SDM_cleaned$Switch_to_alt[i]=0
}


#Create probability using pop freqs of alt answer - initial answer
for(i in 1:length(SDM_cleaned[,1])) 
{
  if (SDM_cleaned$num_alt[i]==1) 
    SDM_cleaned$SubjectvAlt[i]<- SDM_cleaned$Alt1_prop[i] - SDM_cleaned$Init_prop[i]
  
  if(SDM_cleaned$num_alt[i]==3&&SDM_cleaned$Switch_to_alt[i]==0) #No switch, take highest prop of alts
    SDM_cleaned$SubjectvAlt[i]<-(max(SDM_cleaned$Alt1_prop[i], 
                                     SDM_cleaned$Alt2_prop[i],
                                     SDM_cleaned$Alt3_prop[i])-SDM_cleaned$Init_prop[i])
  
  if(SDM_cleaned$num_alt[i]==3&&SDM_cleaned$Switch_to_alt[i]==1) # Switch: Take prop of alt ans that matches fin ans 
    if(SDM_cleaned$Fin_ans[i]==SDM_cleaned$Alt1_ans[i]) SDM_cleaned$SubjectvAlt[i]<-(SDM_cleaned$Alt1_prop[i]-SDM_cleaned$Init_prop[i])
    if(SDM_cleaned$Fin_ans[i]==SDM_cleaned$Alt2_ans[i]) SDM_cleaned$SubjectvAlt[i]<-(SDM_cleaned$Alt2_prop[i]-SDM_cleaned$Init_prop[i])
    if(SDM_cleaned$Fin_ans[i]==SDM_cleaned$Alt3_ans[i]) SDM_cleaned$SubjectvAlt[i]<-(SDM_cleaned$Alt3_prop[i]-SDM_cleaned$Init_prop[i])     
}


#Create probability using pop freqs of alt answer - initial answer 
SDM_cleaned$SubjectvAlt<- SDM_cleaned$Alt1_prop - SDM_cleaned$Init_prop 

#Calculate total RT; rescale by dividing by 1000 so model will converge
SDM_cleaned$TotalRT<-(as.numeric(SDM_cleaned$RT1_keypress) + as.numeric(SDM_cleaned$RT2_Entr) + as.numeric(SDM_cleaned$RT3_conf) + 
                        as.numeric(SDM_cleaned$Alt1_RT) + as.numeric(SDM_cleaned$Alt2_RT) +
                        as.numeric(SDM_cleaned$Alt3_RT) + as.numeric(SDM_cleaned$RT4_fin))/1000

#Drop Practice Trials
SDM_cleaned_noprac<-subset(SDM_cleaned, practice==0)
head(SDM_cleaned_noprac)

#numprac<-aggregate(practice ~ UniqueID, data=SDM_cleaned,  sum)
#write.csv(numprac, file = "numprac.csv")
#write.csv(SDM_cleaned, file = "SDM_cleaned.csv")

#Subset by number of alt answers (num_alt = 1 or 3) and time pressure (time_pres = 0 or 1)
Exp1A<-subset(SDM_cleaned_noprac, num_alt==1&time_pres==0)
Exp1B<-subset(SDM_cleaned_noprac, num_alt==1&time_pres==1)
Exp1C<-subset(SDM_cleaned_noprac, num_alt==3&time_pres==0)
Exp1D<-subset(SDM_cleaned_noprac, num_alt==3&time_pres==1)


#Probability distrubtion functions for individual diffs and switching probability, final accuracy,
# confidence
#Exp3Alt<-rbind(Exp1C, Exp1D)

#IndividualDiffSummary<-aggregate(Switch_to_alt ~ UniqueID + Avg_C + Avg_D + Avg_A + Avg_P+ TotalRT, data=Exp3Alt, mean)
#write.csv(IndividualDiffSummary, file = "IndividualDiffSummary.csv")
#write.csv(ExpCoefs, file = "ExpCoefs.csv")

#NeedForCog<-aggregate(RT4_fin ~ UniqueID + Avg_D, data=Exp3Alt, mean)

#plot(NeedForCog$Avg_Cog, NeedForCog$Switch_to_alt)
#plot(NeedForCog$Avg_Cog, NeedForCog$fin_acc)
#plot(NeedForCog$Avg_C, NeedForCog$fin_acc)

#deducer
#PCA on individual differences?

#Create dataset with cat_rank = 2 dropped
#SDM_cleaned_catrank<-subset(SDM_cleaned, cat_rank!=2)
#range(SDM_cleaned_catrank$cat_rank)

#Same thing, but for cat_rank 2 taken out
  #CatExp1A<-subset(SDM_cleaned_catrank, num_alt==1&time_pres==0)
  #CatExp1B<-subset(SDM_cleaned_catrank, num_alt==1&time_pres==1)
  #CatExp1C<-subset(SDM_cleaned_catrank, num_alt==3&time_pres==0)
  #CatExp1D<-subset(SDM_cleaned_catrank, num_alt==3&time_pres==1)

#Number of Participants
#length(unique(Exp1A[,47])) #38
#length(unique(Exp1B[,47])) #41
#length(unique(Exp1C[,47])) #40
#length(unique(Exp1D[,47])) #42

#Exploratory plots: Ordinary Least Squares (OLS) logistic regressions (assumes independent obs --> models are overfit)
#Reproduce previous figures

#Create custom color palettes 
  BluePal<-mypalette<-brewer.pal(7,"Blues")
  GreenPal<-mypalette<-brewer.pal(7,"Greens")
  OrangePal<-mypalette<-brewer.pal(7,"Oranges")
  PurplePal<-mypalette<-brewer.pal(7,"Purples")

#Display custom palette
#image(1:7,1,as.matrix(1:7),col=RedPal,xlab="Blues (sequential)",ylab="",xaxt="n",yaxt="n",bty="n")

#Customized minimal theme
custom_minimal_theme <- theme_minimal() + theme(text = element_text(size= 10), legend.position="none", 
                                                axis.text.x = element_text(size=rel(1.2)), axis.text.y = element_text(size=rel(1.2)))
#Change for the session: 
#theme_set(theme_grey(base_size = 18))                                                 

#Exp 1A
p1 <- ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  geom_smooth(method = 'glm', family = 'binomial', se = FALSE) +     custom_minimal_theme + 
  labs(title = "Exp1A: One Alt under no Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=BluePal) 

#Exp 1B
p2 <- ggplot(Exp1B, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + custom_minimal_theme  + 
  labs(title = "Exp1B: One Alt under Time Pressue", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=OrangePal)

#Exp 1C

p3 <- ggplot(Exp1C, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + custom_minimal_theme  + 
  labs(title = "Exp1C: Three Alt under no Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=GreenPal)

#Exp 1D
p4 <- ggplot(Exp1D, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + custom_minimal_theme  + 
  labs(title = "Exp1D: Three Alt under Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=PurplePal)


#######Note curves do not extrapolate beyond data###### 

# Multiple plot function
# Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(p1, p2, p3, p4, cols = 2)

#Logistic regressions by individual
# #1A
# SsExp1A<-data.frame(unique(Exp1A$UniqueID))
# Exp1ACoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1A)))
# colnames(Exp1ACoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
# for (i in 1:length(unique(Exp1A$UniqueID))) 
#   {
#   Exp1Atemp<-subset(Exp1A, UniqueID==SsExp1A[i,])
#   testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Atemp,family=binomial())
#   tempcoef<-coef(testm)
#   PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
#   Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#   Exp1ACoef[i,]<-c("1A",unique(Exp1Atemp$ProgID), unique(Exp1Atemp$UniqueID), tempcoef, PSE, Threshold)
#   }
# 
# #1B
# SsExp1B<-data.frame(unique(Exp1B$UniqueID))
# Exp1BCoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1B)))
# colnames(Exp1BCoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
# for (i in 1:length(unique(Exp1B$UniqueID))) 
#   {
#   Exp1Btemp<-subset(Exp1B, UniqueID==SsExp1B[i,])
#   testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Btemp,family=binomial())
#   tempcoef<-coef(testm)
#   PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
#   Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#   #print(tempcoef)
#   Exp1BCoef[i,]<-c("1B", unique(Exp1Btemp$ProgID), unique(Exp1Btemp$UniqueID), tempcoef, PSE, Threshold)
#   #print(SsExp1B[i,])
#   }
# 
# #1C
# SsExp1C<-data.frame(unique(Exp1C$UniqueID))
# Exp1CCoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1C)))
# colnames(Exp1CCoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
# for (i in 1:length(unique(Exp1C$UniqueID))) 
#   {
#   Exp1Ctemp<-subset(Exp1C, UniqueID==SsExp1C[i,])
#   testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Ctemp,family=binomial())
#   tempcoef<-coef(testm)
#   PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
#   Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#   #print(tempcoef)
#   Exp1CCoef[i,]<-c("1C",unique(Exp1Ctemp$ProgID), unique(Exp1Ctemp$UniqueID), tempcoef, PSE, Threshold)
#   #print(SsExp1C[i,])
#   }
# 
# #1D
# SsExp1D<-data.frame(unique(Exp1D$UniqueID))
# Exp1DCoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1D)))
# colnames(Exp1DCoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
# for (i in 1:length(unique(Exp1D$UniqueID))) 
#   {
#   Exp1Dtemp<-subset(Exp1D, UniqueID==SsExp1D[i,])
#   testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Dtemp,family=binomial())
#   tempcoef<-coef(testm)
#   PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
#   Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#   #print(tempcoef)
#   Exp1DCoef[i,]<-c("1D",unique(Exp1Dtemp$ProgID), unique(Exp1Dtemp$UniqueID), tempcoef, PSE, Threshold)
#   #print(SsExp1D[i,])
#   }
# 
# 
# ExpCoefs<-rbind(Exp1ACoef,Exp1BCoef, Exp1CCoef, Exp1DCoef)
# 
# 
# ExpCoefs[,5]
# OR<-exp(as.numeric(ExpCoefs[,5])
#         
#  ExpCoefs<-cbind(ExpCoefs,OR)
#         
#         
# write.csv(ExpCoefs, file = "ExpCoefs.csv")
#         
#         #Logistic regressions by individual, by cat_rank
#         #1A
#         SsExp1A<-data.frame(unique(Exp1A$UniqueID))
#         Exp1ACoef<-data.frame(matrix(ncol=8,nrow=length(SsExp1A)))
#         colnames(Exp1ACoef)<-c("Exp","ProgID", "UniqueID", "Catrank" "Intercept", "Slope", "PSE", "Threshold")
#         for (i in 1:length(unique(Exp1A$UniqueID))) 
#         {Exp1Atemp<-subset(Exp1A, UniqueID==SsExp1A[i,])
#         testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Atemp,family=binomial())
#         tempcoef<-coef(testm)
#         PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
#         Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#         Exp1ACoef[i,]<-c("1A",unique(Exp1Atemp$ProgID), unique(Exp1Atemp$UniqueID), tempcoef, PSE, Threshold)
#         }
#         
#         
#         testmodel<-glm(Switch_to_alt~SubjectvAlt+cat_rank+SubjectvAlt*cat_rank, data=Exp1A, family=binomial())
#         summary(testmodel)
        
# Generalized Linear Mixed-Effects Models (GLMM)
#### Exp 1A ####
# Null Model: DV = swtich to alt, Random Effect Intercept of Subject
Exp1ANullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1A)

#Model 2: Add SubjectvAlt as RE
Exp1AModel2<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1A)
        
#Model 3: Add cat_rank as FE  
Exp1AModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1A)  
        
#Model 4: Add interaction for SubjectvAlt * cat_rank
Exp1AModel4<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1A) 
        
#Model comparison
anova(Exp1ANullModel, Exp1AModel1, Exp1AModel2, Exp1AModel3) 
        
Exp1Amodels<-list()
Exp1Amodels[[1]]<-Exp1ANullModel
Exp1Amodels[[2]]<-Exp1AModel2
Exp1Amodels[[3]]<-Exp1AModel3
Exp1Amodels[[4]]<-Exp1AModel4

Exp1Atable<-aictab(Exp1Amodels) 
evidence(Exp1Atable)
Exp1Atable

unique(Exp1A[,47])

#Coefficents
ExtractGLMMcoefs<-function(GLMMmodel, Participants, Experiment) 
  {
  ExpCofs<-data.frame(c(coef(GLMMmodel)))
  Exp<-rep(Experiment,length(Participants))
  PSE<-(log(0.5)-ExpCofs[1])/ExpCofs[2]  
  Threshold<-(log(0.75)-ExpCofs[1])/ExpCofs[2] 
  CoefsAll<<-cbind(Exp, Participants, ExpCofs, PSE, Threshold)
  colnames(CoefsAll)<-c("Experiment", "Participant", "Intercept", "Slope", "PSE", "Threshold")
  print(CoefsAll)
  }  
      
ExtractGLMMcoefs(Exp1AModel2, unique(Exp1A[,47]), "1A")
Exp1ACoefsAll<-CoefsAll    
        
#### Exp 1B ####   
Exp1BNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1B)  
Exp1BModel2<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1B)
Exp1BModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1B)  
Exp1BModel4<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1B) 
        
anova(Exp1BNullModel, Exp1BModel2, Exp1BModel3, Exp1BModel4)   
Exp1Bmodels<-list()
Exp1Bmodels[[1]]<-Exp1BNullModel
Exp1Bmodels[[2]]<-Exp1BModel2
Exp1Bmodels[[3]]<-Exp1BModel3
Exp1Bmodels[[4]]<-Exp1BModel4

Exp1Btable<-aictab(Exp1Bmodels) 
Exp1Btable
evidence(Exp1Btable)

ExtractGLMMcoefs(Exp1BModel2, (unique(Exp1B[,47])), "1B")
Exp1BCoefsAll<-CoefsAll   
#length(unique(Exp1C[,47])) #40
#length(unique(Exp1D[,47])) #42

#### Exp 1C ####   
Exp1CNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1C)  
Exp1CModel2<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1C)
Exp1CModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1C)  
Exp1CModel4<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                             (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1C) 
        
        
anova(Exp1CNullModel, Exp1CModel2, Exp1CModel3, Exp1CModel4)   
Exp1Cmodels<-list()
Exp1Cmodels[[1]]<-Exp1CNullModel
Exp1Cmodels[[2]]<-Exp1CModel2
Exp1Cmodels[[3]]<-Exp1CModel3
Exp1Cmodels[[4]]<-Exp1CModel4

Exp1Ctable<-aictab(Exp1Cmodels) 
Exp1Ctable
evidence(Exp1Ctable)

ExtractGLMMcoefs(Exp1CModel2, (unique(Exp1C[,47])), "1C")
Exp1CCoefsAll<-CoefsAll  

 #### Exp 1D ####   
Exp1DNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1D)  
Exp1DModel2<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1D)
Exp1DModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1D)  
Exp1DModel4<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                             (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1D) 
    
anova(Exp1DNullModel, Exp1DModel2, Exp1DModel3, Exp1DModel4)
Exp1Dmodels<-list()
Exp1Dmodels[[1]]<-Exp1DNullModel
Exp1Dmodels[[2]]<-Exp1DModel2
Exp1Dmodels[[3]]<-Exp1DModel3
Exp1Dmodels[[4]]<-Exp1DModel4

Exp1Dtable<-aictab(Exp1Dmodels) 
Exp1Dtable
evidence(Exp1Dtable)

ExtractGLMMcoefs(Exp1DModel2, (unique(Exp1D[,47])), "1D")      
Exp1DCoefsAll<-CoefsAll  

#Combine all GLMM coefficients
ExpCoefsAll<-rbind(Exp1ACoefsAll, Exp1BCoefsAll, Exp1CCoefsAll, Exp1DCoefsAll)
colnames(ExpCoefsAll)<-c("Experiment", "Participant", "Intercept", "Slope", "PSE", "Threshold")
write.csv(ExpCoefsAll, file = "ExpCoefsAll.csv")

  #Create column of alt choices
  alt1<-rep(1, each = sum(dim(Exp1ACoefsAll)[1])+dim(Exp1BCoefsAll)[1])
  alt3<-rep(3, each = sum(dim(Exp1CCoefsAll)[1])+dim(Exp1DCoefsAll)[1])
  alt<-data.frame(c(alt1,alt3))
  colnames(alt)<-"Alt"
  
  #Create column of time pressure
  time1A<-rep(0, each = dim(Exp1ACoefsAll)[1]) 
  time1B<-rep(1, each = dim(Exp1BCoefsAll)[1])
  time1C<-rep(0, each = dim(Exp1CCoefsAll)[1])
  time1D<-rep(1, each = dim(Exp1DCoefsAll)[1])
  
  TimePressure<-data.frame(c(time1A, time1B, time1C, time1D))
  colnames(TimePressure)<-"TimePressure"
  
  oddsratio<-data.frame(exp(ExpCoefsAll$Slope))
  colnames(oddsratio)<-"OR" 

  ExpCoefsAll<-cbind(ExpCoefsAll, alt, TimePressure, oddsratio)

#Coefficients
  #Graphs
  hist(ExpCoefsAll$PSE)
  hist(ExpCoefsAll$Threshold)
  hist(ExpCoefsAll$OR)
  hist(log(ExpCoefsAll$OR))

  #Correlation
  cor(ExpCoefsAll$PSE, ExpCoefsAll$Threshold)
  cor(ExpCoefsAll$PSE, ExpCoefsAll$OR)
  cor(ExpCoefsAll$Threshold, ExpCoefsAll$OR)

  #Descriptives
  alt1coef<-subset(ExpCoefsAll,alt==1)
  alt3coef<-subset(ExpCoefsAll,alt==3)
  
  mean(alt1coef$PSE)
  sqrt(var(alt1coef$PSE))
  
  mean(alt3coef$PSE)
  sqrt(var(alt3coef$PSE))


  mean(alt1coef$Threshold)
  sqrt(var(alt1coef$Threshold))
    
  mean(alt3coef$Threshold)
  sqrt(var(alt3coef$Threshold))



  mean(alt1coef$OR)
  sqrt(var(alt1coef$OR))
    
  mean(alt3coef$OR)
  sqrt(var(alt3coef$OR))


  #PSE
    PSElm<-lm(PSE~Alt+TimePressure+(Alt*TimePressure), data = ExpCoefsAll)
    PSEanova<-Anova(PSElm, type = 3)
    etaSquared(PSElm, type = 3, anova = TRUE)

  #Threshold
    Thresholdlm<-lm(Threshold~Alt+TimePressure+(Alt*TimePressure), data = ExpCoefsAll)
    Thresholdanova<-Anova(Thresholdlm, type = 3)
    etaSquared(Thresholdlm, type = 3, anova = TRUE)

  #Odds ratio
    ORlm<-lm(OR~Alt+TimePressure+(Alt*TimePressure), data = ExpCoefsAll)
    ORanova<-Anova(ORlm, type = 3)
    etaSquared(ORlm, type = 3, anova = TRUE)

    #Log transformed b/c of positive skew
    ORloglm<-lm(log(OR)~Alt+TimePressure+(Alt*TimePressure), data = ExpCoefsAll)
    ORloganova<-Anova(ORlm, type = 3)
    etaSquared(ORloglm, type = 3, anova = TRUE)


 #GLMM plots
#Code for plots
  Exp1A$predoverall<-predict(Exp1AModel2,  re.form=~(1 + SubjectvAlt|UniqueID), type = "response")
  Exp1B$predoverall<-predict(Exp1BModel2,  re.form=~(1 + SubjectvAlt|UniqueID), type = "response")
  Exp1C$predoverall<-predict(Exp1CModel2,  re.form=~(1 + SubjectvAlt|UniqueID), type = "response")
  Exp1D$predoverall<-predict(Exp1DModel2,  re.form=~(1 + SubjectvAlt|UniqueID), type = "response")

#https://rpubs.com/bbolker/glmmchapter

#Exp 1A
p1 <- ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  geom_line(aes(y = predoverall))  + custom_minimal_theme + 
  labs(title = "Exp1A: One Alt under no Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=BluePal) 

#Exp 1B
p2 <- ggplot(Exp1B, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  geom_line(aes(y = predoverall)) + custom_minimal_theme  + 
  labs(title = "Exp1B: One Alt under Time Pressue", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=OrangePal)

#Exp 1C
p3 <- ggplot(Exp1C, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  geom_line(aes(y = predoverall)) + custom_minimal_theme  + 
  labs(title = "Exp1C: Three Alt under no Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=GreenPal)

#Exp 1D
p4 <- ggplot(Exp1D, aes(x = SubjectvAlt, y = Switch_to_alt, group = UniqueID, color = UniqueID)) +
  geom_line(aes(y = predoverall))+ custom_minimal_theme  + 
  labs(title = "Exp1D: Three Alt under Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
  scale_colour_gradientn(colours=PurplePal)

multiplot(p1, p2, p3, p4, cols = 2)
        