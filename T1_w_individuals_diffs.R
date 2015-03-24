# Jon's R code for T1 analysis
# Last updated 3/24/2015
# R version 3.1.3 
# RStudio Version 0.99.324
# Zip file: Complete SDM2.1_1.20.15.zip, 162 csv files
# Individual diffs: SDM2.1_Indiv_diff_3.23.15.csv

#Required R Packages
require(ggplot2)
require(lme4)
require(boot)
require(geepack)
require(MESS)
require(MASS)
require(RColorBrewer)
require(foreign)
require(plyr)
require(mefa)
require(Hmisc)

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

#Sanity check: 162 csv files (participants) with 123 rows each:  19,926 rows x 47 columns
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

numprac<-aggregate(practice ~ UniqueID, data=SDM_cleaned,  sum)
write.csv(numprac, file = "numprac.csv")

write.csv(SDM_cleaned, file = "SDM_cleaned.csv")

#Subset by number of alt answers (num_alt = 1 or 3) and time pressure (time_pres = 0 or 1)
Exp1A<-subset(SDM_cleaned, num_alt==1&time_pres==0)
Exp1B<-subset(SDM_cleaned, num_alt==1&time_pres==1)
Exp1C<-subset(SDM_cleaned, num_alt==3&time_pres==0)
Exp1D<-subset(SDM_cleaned, num_alt==3&time_pres==1)

#Same thing, but for cat_rank 2 taken out
#CatExp1A<-subset(SDM_cleaned_catrank, num_alt==1&time_pres==0)
#CatExp1B<-subset(SDM_cleaned_catrank, num_alt==1&time_pres==1)
#CatExp1C<-subset(SDM_cleaned_catrank, num_alt==3&time_pres==0)
#CatExp1D<-subset(SDM_cleaned_catrank, num_alt==3&time_pres==1)


#Create dataset with cat_rank = 2 dropped
SDM_cleaned_catrank<-subset(SDM_cleaned, cat_rank!=2)
#range(SDM_cleaned_catrank$cat_rank)

#Number of SS
#length(unique(Exp1A[,47])) #39
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
#image(1:7,1,as.matrix(1:7),col=RedPal,xlab="Blues (sequential)",
#      ylab="",xaxt="n",yaxt="n",bty="n")

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

#1A
SsExp1A<-data.frame(unique(Exp1A$UniqueID))
Exp1ACoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1A)))
colnames(Exp1ACoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
for (i in 1:length(unique(Exp1A$UniqueID))) 
{Exp1Atemp<-subset(Exp1A, UniqueID==SsExp1A[i,])
testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Atemp,family=binomial())
tempcoef<-coef(testm)
PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
Exp1ACoef[i,]<-c("1A",unique(Exp1Atemp$ProgID), unique(Exp1Atemp$UniqueID), tempcoef, PSE, Threshold)
}

#1B
SsExp1B<-data.frame(unique(Exp1B$UniqueID))
Exp1BCoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1B)))
colnames(Exp1BCoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
for (i in 1:length(unique(Exp1B$UniqueID))) 
{Exp1Btemp<-subset(Exp1B, UniqueID==SsExp1B[i,])
testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Btemp,family=binomial())
tempcoef<-coef(testm)
PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#print(tempcoef)
Exp1BCoef[i,]<-c("1B", unique(Exp1Btemp$ProgID), unique(Exp1Btemp$UniqueID), tempcoef, PSE, Threshold)
#print(SsExp1B[i,])
}

#1C
SsExp1C<-data.frame(unique(Exp1C$UniqueID))
Exp1CCoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1C)))
colnames(Exp1CCoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
for (i in 1:length(unique(Exp1C$UniqueID))) 
{Exp1Ctemp<-subset(Exp1C, UniqueID==SsExp1C[i,])
testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Ctemp,family=binomial())
tempcoef<-coef(testm)
PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#print(tempcoef)
Exp1CCoef[i,]<-c("1C",unique(Exp1Ctemp$ProgID), unique(Exp1Ctemp$UniqueID), tempcoef, PSE, Threshold)
#print(SsExp1C[i,])
}

#1D
SsExp1D<-data.frame(unique(Exp1D$UniqueID))
Exp1DCoef<-data.frame(matrix(ncol=7,nrow=length(SsExp1D)))
colnames(Exp1DCoef)<-c("Exp","ProgID", "UniqueID", "Intercept", "Slope", "PSE", "Threshold")
for (i in 1:length(unique(Exp1D$UniqueID))) 
{Exp1Dtemp<-subset(Exp1D, UniqueID==SsExp1D[i,])
testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Dtemp,family=binomial())
tempcoef<-coef(testm)
PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
#print(tempcoef)
Exp1DCoef[i,]<-c("1D",unique(Exp1Dtemp$ProgID), unique(Exp1Dtemp$UniqueID), tempcoef, PSE, Threshold)
#print(SsExp1D[i,])
}


ExpCoefs<-rbind(Exp1ACoef,Exp1BCoef, Exp1CCoef, Exp1DCoef)


ExpCoefs[,5]
OR<-exp(as.numeric(ExpCoefs[,5])
        
        ExpCoefs<-cbind(ExpCoefs,OR)
        
        
        write.csv(ExpCoefs, file = "ExpCoefs.csv")
        
        #Logistic regressions by individual, by cat_rank
        #1A
        SsExp1A<-data.frame(unique(Exp1A$UniqueID))
        Exp1ACoef<-data.frame(matrix(ncol=8,nrow=length(SsExp1A)))
        colnames(Exp1ACoef)<-c("Exp","ProgID", "UniqueID", "Catrank" "Intercept", "Slope", "PSE", "Threshold")
        for (i in 1:length(unique(Exp1A$UniqueID))) 
        {Exp1Atemp<-subset(Exp1A, UniqueID==SsExp1A[i,])
        testm<-glm(Switch_to_alt~SubjectvAlt,data=Exp1Atemp,family=binomial())
        tempcoef<-coef(testm)
        PSE<-(log(0.5)-tempcoef[1])/tempcoef[2]
        Threshold<-(log(0.75)-tempcoef[1])/tempcoef[2]
        Exp1ACoef[i,]<-c("1A",unique(Exp1Atemp$ProgID), unique(Exp1Atemp$UniqueID), tempcoef, PSE, Threshold)
        }
        
        
        testmodel<-glm(Switch_to_alt~SubjectvAlt+cat_rank+SubjectvAlt*cat_rank, data=Exp1A, family=binomial())
        summary(testmodel)
        
        # Generalized Linear Mixed-Effects Models (GLMM)
        #### Exp 1A ####
        # Null Model: DV = swtich to alt, Random Effect Intercept of Subject
        Exp1ANullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1A)
        yFE.Exp1ANullModel<-predict(Exp1ANullModel, type = "response", re.form = ~0)
        
        yFull.Exp1ANullModel<-predict(Exp1ANullModel, type = "response")
        yFull<-fitted(Exp1ANullModel)
        
        #Fixef plot: Group level  
        ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt)) + 
          geom_line(aes(y =  yFE.Exp1ANullModel)) 
        
        #Plot with individuals
        ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group=UniqueID, color = UniqueID)) + 
          geom_point() + geom_line(aes(y = yFull)) +
          theme_minimal() +
          theme(panel.border = element_rect(fill = NA)) 
        
        
        #Model 1: Add SubjectvAlt as RE
        Exp1AModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1A)
        
        #Model 2: Add cat_rank as FE  
        Exp1AModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1A)  
        
        #Model 3: Add interaction for SubjectvAlt * cat_rank
        Exp1AModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                             (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1A) 
        
        #Model comparison
        anova(Exp1ANullModel, Exp1AModel1, Exp1AModel2, Exp1AModel3) 
        
        #Model 2 has the best fit
        #Coefficents
        Exp1ACoefs<-coef(Exp1AModel2)
        Exp1Acoefsdataframe<-data.frame(c(Exp1ACoefs))
        
        PSECat1<-(log(0.5)-Exp1Acoefsdataframe[1])/(Exp1Acoefsdataframe[2]+Exp1Acoefsdataframe[3]*1) 
        ThresholdCat1<-(log(0.75)-Exp1Acoefsdataframe[1])/(Exp1Acoefsdataframe[2]+Exp1Acoefsdataframe[3]*1) 
        
        PSECat2<-(log(0.5)-Exp1Acoefsdataframe[1])/(Exp1Acoefsdataframe[2]+Exp1Acoefsdataframe[3]*2)  
        ThresholdCat2<-(log(0.75)-Exp1Acoefsdataframe[1])/(Exp1Acoefsdataframe[2]+Exp1Acoefsdataframe[3]*2)   
        
        PSECat3<-(log(0.5)-Exp1Acoefsdataframe[1])/(Exp1Acoefsdataframe[2]+Exp1Acoefsdataframe[3]*3)  
        ThresholdCat3<-(log(0.75)-Exp1Acoefsdataframe[1])/(Exp1Acoefsdataframe[2]+Exp1Acoefsdataframe[3]*3)   
        
        Exp<-rep("1A",39)
        
        Exp1ACoefsAll<-cbind(Exp,SsExp1A, Exp1Acoefsdataframe,PSECat1, ThresholdCat1, PSECat2, ThresholdCat2, PSECat3, ThresholdCat3)
        
        
        colnames(Exp1ACoefsAll)<-c("Experiment", "UniqueID", "Intercept", "SlopeSstvAlt", "Slopecatrank", 
                                   "Cat1PSE", "Cat1Threshold",  "Cat2PSE", "Cat2Threshold",
                                   "Cat3PSE", "Cat3Threshold")
        
        write.csv(Exp1ACoefsAll, file = "Exp1ACoefsAll.csv")
        
        
        #Exp1 Model1
        
        Exp1Acoefs<-coef(Exp1AModel1)
        Exp1Acoefsdataframe<-data.frame(c(Exp1Acoefs))
        
        Exp<-rep("1A",length(unique(Exp1A$UniqueID)))
        
        PSE1A<-(log(0.5)-Exp1Acoefsdataframe[1])/Exp1Acoefsdataframe[2]  
        Threshold1A<-(log(0.75)-Exp1Acoefsdataframe[1])/Exp1Acoefsdataframe[2] 
        
        Exp1ACoefsAll<-cbind(Exp, Exp1Acoefsdataframe, PSE1A, Threshold1A)
        
        colnames(Exp1ACoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold")  
        
        
        
        
        #### Exp 1B ####   
        Exp1BNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1B)  
        Exp1BModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1B)
        Exp1BModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1B)  
        Exp1BModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                             (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1B) 
        
        
        anova(Exp1BNullModel, Exp1BModel1, Exp1BModel2, Exp1BModel3)   
        
        #Model 1 has the best fit
        
        Exp1Bcoefs<-coef(Exp1BModel1)
        Exp1Bcoefsdataframe<-data.frame(c(Exp1Bcoefs))
        
        Exp<-rep("1B",length(unique(Exp1B$UniqueID)))
        
        PSE1B<-(log(0.5)-Exp1Bcoefsdataframe[1])/Exp1Bcoefsdataframe[2]  
        Threshold1B<-(log(0.75)-Exp1Bcoefsdataframe[1])/Exp1Bcoefsdataframe[2] 
        
        Exp1BCoefsAll<-cbind(Exp, Exp1Bcoefsdataframe, PSE1B, Threshold1B)
        
        colnames(Exp1BCoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold")  
        
        #### Exp 1C ####   
        Exp1CNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1C)  
        Exp1CModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1C)
        Exp1CModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1C)  
        Exp1CModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                             (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1C) 
        
        
        anova(Exp1CNullModel, Exp1CModel1, Exp1CModel2, Exp1CModel3)   
        
        #Model 1 has the best fit
        Exp1Ccoefs<-coef(Exp1CModel1)
        Exp1Ccoefsdataframe<-data.frame(c(Exp1Ccoefs))
        
        Exp<-rep("1C",length(unique(Exp1C$UniqueID)))
        
        PSE1C<-(log(0.5)-Exp1Ccoefsdataframe[1])/Exp1Ccoefsdataframe[2]  
        Threshold1C<-(log(0.75)-Exp1Ccoefsdataframe[1])/Exp1Ccoefsdataframe[2] 
        
        
        Exp1CCoefsAll<-cbind(Exp, Exp1Ccoefsdataframe, PSE1C, Threshold1C)
        
        colnames(Exp1CCoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold")  
        
        
        #### Exp 1D ####   
        Exp1DNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = Exp1D)  
        Exp1DModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1D)
        Exp1DModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1D)  
        Exp1DModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                             (1 + SubjectvAlt | UniqueID), family = binomial, data = Exp1D) 
        
        
        anova(Exp1DNullModel, Exp1DModel1, Exp1DModel2, Exp1DModel3)   
        #Model 1 has the best fit
        Exp1Dcoefs<-coef(Exp1DModel1)
        Exp1Dcoefsdataframe<-data.frame(c(Exp1Dcoefs))
        
        Exp<-rep("1D",length(unique(Exp1D$UniqueID)))
        
        PSE1D<- ((log(0.5)-Exp1Dcoefsdataframe[1])/Exp1Dcoefsdataframe[2])  
        Threshold1D<-(log(0.75)-Exp1Dcoefsdataframe[1])/Exp1Dcoefsdataframe[2] 
        
        Exp1DCoefsAll<-cbind(Exp, Exp1Dcoefsdataframe, PSE1D, Threshold1D) 
        Exp1DCoefsAll<-cbind(Exp1DCoefsAll[,1:4], Exp1DCoefsAll[,6]) #Slope gets duplicated. No idea why. Only at cbind.
        
        colnames(Exp1DCoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold")  
        
        Exp1BCDcoefs<-rbind(Exp1BCoefsAll, Exp1CCoefsAll, Exp1DCoefsAll)
        
        Exp1ABCDcoefs<-rbind(Exp1ACoefsAll, Exp1BCoefsAll, Exp1CCoefsAll, Exp1DCoefsAll) 
        
        write.csv(Exp1BCDcoefs, file = "Exp1BCDcoefsAll.csv")
        write.csv(Exp1ABCDcoefs, file = "Exp1ABCDcoefsAll.csv")
        
        #GLMMs for cat_rank 1 and 3
        #****Need to compare models directly, not just null as baseline****
        #Exp 1A
        #Null model
        CExp1ANullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = CatExp1A)
        
        #Model 1: Add SubjectvAlt as RE
        CExp1AModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1A)
        
        #Model 2: Add cat_rank as FE  
        CExp1AModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1A)  
        
        #Model 3: Add interaction for SubjectvAlt * cat_rank
        CExp1AModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                              (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1A) 
        
        #Model comparison
        anova(CExp1ANullModel, CExp1AModel1, CExp1AModel2, CExp1AModel3) 
        #Model 2 has the best fit
        
        #Exp 1B
        #Null model
        CExp1BNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = CatExp1B)
        
        #Model 1: Add SubjectvAlt as RE
        CExp1BModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1B)
        
        #Model 2: Add cat_rank as FE  
        CExp1BModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1B)  
        
        #Model 3: Add interaction for SubjectvAlt * cat_rank
        CExp1BModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                              (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1B) 
        
        #Model comparison
        anova(CExp1BNullModel, CExp1BModel1, CExp1BModel2, CExp1BModel3) 
        #Model 1 has the best fit.
        
        #Exp 1C
        #Null model
        CExp1CNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = CatExp1C)
        
        #Model 1: Add SubjectvAlt as RE
        CExp1CModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1C)
        
        #Model 2: Add cat_rank as FE  
        CExp1CModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1C)  
        
        #Model 3: Add interaction for SubjectvAlt * cat_rank
        CExp1CModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                              (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1C) 
        
        #Model comparison
        anova(CExp1CNullModel, CExp1CModel1, CExp1CModel2, CExp1CModel3) 
        #Model 1 has the best fit.
        
        #Exp 1D
        #Null model
        CExp1DNullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | UniqueID), family = binomial, data = CatExp1D)
        
        #Model 1: Add SubjectvAlt as RE
        CExp1DModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1D)
        
        #Model 2: Add cat_rank as FE  
        CExp1DModel2<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1D)  
        
        #Model 3: Add interaction for SubjectvAlt * cat_rank
        CExp1DModel3<-glmer(Switch_to_alt ~ SubjectvAlt + cat_rank + SubjectvAlt*cat_rank + 
                              (1 + SubjectvAlt | UniqueID), family = binomial, data = CatExp1D) 
        
        #Model comparison
        anova(CExp1DNullModel, CExp1DModel1, CExp1DModel2, CExp1DModel3) 
        #Model 1 has the best fit.
        
        #Coefficents
        #Exp1A 
        CExp1Acoefs<-coef(CExp1AModel1)
        CExp1Acoefsdataframe<-data.frame(c(CExp1Acoefs))
        Exp<-rep("1A",length(unique(CatExp1A$UniqueID)))
        
        CPSE1A<-(log(0.5)-CExp1Acoefsdataframe[1])/CExp1Acoefsdataframe[2]  
        CThreshold1A<-(log(0.75)-CExp1Acoefsdataframe[1])/CExp1Acoefsdataframe[2] 
        CExp1ACoefsAll<-cbind(Exp, CExp1Acoefsdataframe, CPSE1A, CThreshold1A)
        colnames(CExp1ACoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold")  
        
        #Exp1B 
        CExp1Bcoefs<-coef(CExp1BModel1)
        CExp1Bcoefsdataframe<-data.frame(c(CExp1Bcoefs))
        Exp<-rep("1B",length(unique(CatExp1B$UniqueID)))
        
        CPSE1B<-(log(0.5)-CExp1Bcoefsdataframe[1])/CExp1Bcoefsdataframe[2]  
        CThreshold1B<-(log(0.75)-CExp1Bcoefsdataframe[1])/CExp1Bcoefsdataframe[2] 
        CExp1BCoefsAll<-cbind(Exp, CExp1Bcoefsdataframe, CPSE1B, CThreshold1B)
        colnames(CExp1BCoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold") 
        
        #Exp1C 
        CExp1Ccoefs<-coef(CExp1CModel1)
        CExp1Ccoefsdataframe<-data.frame(c(CExp1Ccoefs))
        Exp<-rep("1C",length(unique(CatExp1C$UniqueID)))
        
        CPSE1C<-(log(0.5)-CExp1Ccoefsdataframe[1])/CExp1Ccoefsdataframe[2]  
        CThreshold1C<-(log(0.75)-CExp1Ccoefsdataframe[1])/CExp1Ccoefsdataframe[2] 
        CExp1CCoefsAll<-cbind(Exp, CExp1Ccoefsdataframe, CPSE1C, CThreshold1C)
        colnames(CExp1CCoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold") 
        
        
        
        #Exp1D 
        CExp1Dcoefs<-coef(CExp1DModel1)
        CExp1Dcoefsdataframe<-data.frame(c(CExp1Dcoefs))
        Exp<-rep("1D",length(unique(CatExp1D$UniqueID)))
        
        CPSE1D<-(log(0.5)-CExp1Dcoefsdataframe[1])/CExp1Dcoefsdataframe[2]  
        CThreshold1D<-(log(0.75)-CExp1Dcoefsdataframe[1])/CExp1Dcoefsdataframe[2] 
        CExp1DCoefsAll<-cbind(Exp, CExp1Dcoefsdataframe, CPSE1D, CThreshold1D)
        colnames(CExp1DCoefsAll)<-c("Experiment", "Intercept", "SlopeSstvAlt", "PSE", "Threshold") 
        CExp1DCoefsAll<-cbind(CExp1DCoefsAll[,1:4], CExp1DCoefsAll[,6])  
        
        
        
        CExp1ABCDcoefs<-rbind(CExp1ACoefsAll, CExp1BCoefsAll, CExp1CCoefsAll, CExp1DCoefsAll) 
        write.csv(CExp1ABCDcoefs, file = "CExp1ABCDcoefs.csv")
        
        
        
        