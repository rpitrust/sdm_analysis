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
require(lme4)
require(boot)
require(geepack)
require(MESS)
require(MASS)

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

#Subset by number of alt answers, num_alt = 1 or 3
SDMalt1<-subset(SDM_cleaned, num_alt==1)
SDMalt3<-subset(SDM_cleaned, num_alt==3)

#Generalized Estimated Equations, mixed effects non-linear model
#DV = final answer correct (1)/ incorrect (0)
#Parameters
#Fixed: Time pressure [More complex models: Category, difficulty, ... ]
#Random: Participant [More complex models: Individual diffs, ...] 

  #Example
  #library(geepack)
  #data(ohio)
  #fit <- geeglm(resp ~ age + smoke + age:smoke, id=id, data=ohio,
  #              family=binomial, corstr="exch", scale.fix=TRUE)
  #QIC(fit)

#Models: Alternate answers = 1:
#Base model w/independent correlation structure (does not take into account repeated measures)
  OneBaseModel<-geeglm(fin_acc~time_pres,data=SDMalt1,id=ProgID, family=binomial)
  Onem1fit<-QIC(OneBaseModel) #Quasi Information Criterion for GEE model objects. Similiar to AIC and BIC, smaller values indicate a better model fit. 
  #         QIC         QICu    Quasi Lik          CIC       params         QICC 
  #11798.878860 11799.533415 -5897.766707     1.672723     2.000000 11798.880268 
  summary(OneBaseModel)
    #               Estimate Std.err Wald  Pr(>|W|) 
    #(Intercept)    0.10520 0.02741 14.73  0.00012 ***
    #  time_pres    0.00733 0.03960  0.03  0.85319 

#Exchangable correlation structure
  OneExchModel<-geeglm(fin_acc~time_pres,data=SDMalt1, id=ProgID,  family=binomial, corstr = "exchangeable") 
  Onem2fit<-QIC(OneExchModel) 

#AR1 correlation structure
  OneAR1Model<-geeglm(fin_acc~time_pres,data=SDMalt1, id=ProgID,  family=binomial, corstr = "ar1") 
  Onem3fit<-QIC(OneAR1Model) 

#Compare model fits, all are about the same
list(Onem1fit,Onem2fit,Onem3fit)

#Models: Alternate answers = 3:  
  ThreeBaseModel<-geeglm(fin_acc~time_pres,data=SDMalt3,id=ProgID, family=binomial)
  Threem1fit<-QIC(ThreeBaseModel)  
    #QIC      QICu      Quasi Lik       CIC    params      QICC 
    #12240.33  12236.25  -6116.13      4.04      2.00  12240.33 
    summary(ThreeBaseModel)
    #               Estimate Std.err Wald  Pr(>|W|) 
    #(Intercept)     0.1990  0.0399 24.82  6.3e-07 ***
    #  time_pres    -0.1046  0.0607  2.97    0.085 . 

#Exchangable correlation structure
  ThreeExchModel<-geeglm(fin_acc~time_pres,data=SDMalt3, id=ProgID,  family=binomial, corstr = "exchangeable") 
  Threem2fit<-QIC(ThreeExchModel) 

#AR1 correlation structure
  ThreeAR1Model<-geeglm(fin_acc~time_pres,data=SDMalt3, id=ProgID,  family=binomial, corstr = "ar1") 
  Threem3fit<-QIC(ThreeAR1Model) 

#Compare model fits, model 2 (exchangable) has the best fit [Four lower in QIC and QICC]
list(Threem1fit,Threem2fit,Threem3fit) 




#Implement bootstrapping

#Final accuracy vs. final confidence
ggplot(SDMalt1, aes(x=fin_conf,y=fin_acc)) + 
  stat_sum(aes(size = ..n.., group = 3)) +
  scale_size_area(max_size=10)


#Binomial Mixed Effects Model using Generalized Estimated Equations
# DV = num of correct items
# Model parameters
#Fixed: Time pressure 
#Random: Participant, individual diffs





#Old code
#Logistic regression plots
  #Final accuracy by stars
  ggplot(SDM_cleaned, aes(stars, fin_acc, group=factor(ProgID))) + 
    geom_point() +
    stat_smooth(method="glm", family = "binomial", SE = F)
  
  #Final accuracy by final confidence
  ggplot(SDM_cleaned, aes(fin_conf, fin_acc, color=factor(ProgID))) + 
    stat_smooth(method="glm", family = "binomial", se = F)

      #Improper logistic regression because respones are not independent 
      logreg_conf_acc<-glm(formula= fin_acc ~ fin_conf, family ="binomial", data=SDM_cleaned)
        #Coefficients
        exp(coef(logreg_conf_acc))
        #Confidence intervals around odds ratio
        exp(cbind(OR = coef(logreg_conf_acc), confint(logreg_conf_acc)))

          #hist(fin_conf)
          #boxplot(fin_conf)
          #Check for response compression; use z-score?

      #Probability of switching answers

#GLMM and model comparison
#
