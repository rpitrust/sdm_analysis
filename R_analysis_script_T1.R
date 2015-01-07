# Jon's R code for T1 analysis
# Last updated 1/7/2015
# R version 3.1.2 
# RStudio Version 0.98.1091
# Zip file: CSVs final clean 4.22.14.zip
# 148 csv files; 10 unusable csv files (12, 24, 31, 36, 53, 57, 75, 80, 106, 143) placed in a separate directory

#Required R Packages
  require(ggplot2)
  require(lme4)
  require(boot)
  require(geepack)
  require(MESS)
  require(MASS)
  require(RColorBrewer)

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

#Bind files into a big data frame
  CSVDataFrame<-do.call(rbind, lapply(filenames, read.csvfiles))

#Sanity check: 148 good csv files (participants) with 120 rows and 45 columns = 17760 rows x 45 columns
  dim(CSVDataFrame)
  head(CSVDataFrame)

#Drop misspelled final answers, fin_acc = 9, but keep fin_acc = 0 or 1
  SDM_cleaned<-subset(CSVDataFrame, fin_acc!=9) #17390 rows
  hist(SDM_cleaned$fin_acc)

#Subject switches to alt answer? Create new variable switch_to_alt (0 or 1)
  for(i in 1:length(SDM_cleaned[,1])) 
      {if(SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i]) SDM_cleaned$Switch_to_alt[i]=1
          else SDM_cleaned$Switch_to_alt[i]=0
      }
  
  #Need to check if initial answer equals alt? Only happens once, ignoring for now
  #sum(SDM_cleaned$Init_ans==SDM_cleaned$Alt1_ans)
  
#Create probability the subject will switch to alt: Alt answer vs. Pop freq of initial answer vs
  SDM_cleaned$SubjectvAlt<- SDM_cleaned$Alt1_prop - SDM_cleaned$Init_prop 
  
#Subset by number of alt answers (num_alt = 1 or 3) and time pressure (time_pres = 0 or 1)
  Exp1A<-subset(SDM_cleaned, num_alt==1&time_pres==0)
  Exp1B<-subset(SDM_cleaned, num_alt==1&time_pres==1)
  Exp1C<-subset(SDM_cleaned, num_alt==3&time_pres==0)
  Exp1D<-subset(SDM_cleaned, num_alt==3&time_pres==1)
    
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
    
  #Exp 1A
  ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
      stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + theme_minimal() + 
      labs(title = "Exp1A", x = "Subject Answer Pop Freq vs Alt Answer Pop Freq", y = "Probability of Switching") +  scale_colour_gradientn(colours=BluePal)
  
  #Exp 1B
  ggplot(Exp1B, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + theme_minimal() + 
    labs(title = "Exp1B", x = "Subject Answer Pop Freq vs Alt Answer Pop Freq", y = "Probability of Switching") +  scale_colour_gradientn(colours=GreenPal)
  
  #Exp 1C
  ggplot(Exp1C, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + theme_minimal() + 
    labs(title = "Exp1C", x = "Subject Answer Pop Freq vs Alt Answer Pop Freq", y = "Probability of Switching") +  scale_colour_gradientn(colours=OrangePal)
  
  #Exp 1D
  ggplot(Exp1D, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + theme_minimal() + 
    labs(title = "Exp1D", x = "Subject Answer Pop Freq vs Alt Answer Pop Freq", y = "Probability of Switching") +  scale_colour_gradientn(colours=PurplePal)

#To-do list
  #Model fitting and comparison using lme4  
  #Infer expertise based on titration? Self-reported confidence?
  #Switching behavior using decision trees?
  
  
  
##########################Old code########################################################################
  ggplot(Exp1A,aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    geom_line(aes(group=ProgID)) +
    facet_grid(~TargetSize) + theme_minimal() +
    theme(panel.border = element_rect(fill = NA))
  
  
  ggplot(Exp1,aes(Accuracy,EstRadPx, group = Subject, color = Subject)) +
    geom_line(aes(group=Subject))
  
  
  
  
  Exp1AOvervall<-glm(Switch_to_alt ~SubjectvAlt, family=binomial(logit), data =  Exp1A)
  PredAlt1Overall<-predict(alt1Overvall)
  
  ggplot(SDMalt1, aes(x = diff , y = fin_acc,color = ProgID)) + geom_point(position = position_jitter(w = 0.05, h=0.05)) + geom_line(aes(y = PredAlt1Overall))  +  theme_minimal()
  
  
  x = SubjectvAlt y = 
  
  
  
  plogis(Alt1logistic)
  
  ggplot(SDMalt1, aes(x = diff , y = fin_acc, group=ProgID,
                   color = ProgID)) + geom_point(position = position_jitter(w = 0.05, h=0.05)) + geom_line(aes(y = PredAlt1Overall))  +  theme_minimal()
    
  
  
  facet_grid(~TargetSize) +


    
    
    
    
    
    
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Generalized Estimated Equations, mixed effects non-linear model
  # Paper describing geepack: http://www.jstatsoft.org/v15/i02/paper
#DV = final answer correct (1)/ incorrect (0)
#Parameters
#Fixed: Time pressure [More complex models: Category, ... ]
#Random: Participant [More complex models: Individual diffs, difficultly, credibility,  ...] 
          
            #Example
            #library(geepack)
            #data(ohio)
            #fit <- geeglm(resp ~ age + smoke + age:smoke, id=id, data=ohio,
            #              family=binomial, corstr="exch", scale.fix=TRUE)
            #QIC(fit)

#Models: Alternate answers = 1:
  #Model 1: Base model w/independent correlation structure 
    OneBaseModel<-geeglm(fin_acc~time_pres,data=SDMalt1,id=ProgID, family=binomial)
    Onem1fit<-QIC(OneBaseModel) #Quasi Information Criterion for GEE model objects. Similiar to AIC and BIC, smaller values indicate a better model fit. 
    #         QIC         QICu    Quasi Lik          CIC       params         QICC 
    #11798.878860 11799.533415 -5897.766707     1.672723     2.000000 11798.880268 
    summary(OneBaseModel)
      #               Estimate Std.err Wald  Pr(>|W|) 
      #(Intercept)    0.10520 0.02741 14.73  0.00012 ***
      #  time_pres    0.00733 0.03960  0.03  0.85319 

  #Model 2: Exchangable correlation structure
    OneExchModel<-geeglm(fin_acc~time_pres,data=SDMalt1, id=ProgID,  family=binomial, corstr = "exchangeable") 
    Onem2fit<-QIC(OneExchModel) 

  #Model 3: AR1 correlation structure
    OneAR1Model<-geeglm(fin_acc~time_pres,data=SDMalt1, id=ProgID,  family=binomial, corstr = "ar1") 
    Onem3fit<-QIC(OneAR1Model) 

  #Compare model fits, all are about the same
    list(Onem1fit,Onem2fit,Onem3fit)

#Models: Alternate answers = 3: 
  #Model 1: Base model w/independent correlation structure 
  ThreeBaseModel<-geeglm(fin_acc~time_pres,data=SDMalt3,id=ProgID, family=binomial)
  Threem1fit<-QIC(ThreeBaseModel)  
    #QIC      QICu      Quasi Lik       CIC    params      QICC 
    #12240.33  12236.25  -6116.13      4.04      2.00  12240.33 
    summary(ThreeBaseModel)
    #               Estimate Std.err Wald  Pr(>|W|) 
    #(Intercept)     0.1990  0.0399 24.82  6.3e-07 ***
    #  time_pres    -0.1046  0.0607  2.97    0.085 . 

  #Model 2: Exchangable correlation structure
    ThreeExchModel<-geeglm(fin_acc~time_pres,data=SDMalt3, id=ProgID,  family=binomial, corstr = "exchangeable") 
    Threem2fit<-QIC(ThreeExchModel) 

  #Model 3: AR1 correlation structure
    ThreeAR1Model<-geeglm(fin_acc~time_pres,data=SDMalt3, id=ProgID,  family=binomial, corstr = "ar1") 
    Threem3fit<-QIC(ThreeAR1Model) 

  #Compare model fits, model 2 (exchangable) has the best fit [Four points lower in QIC and QICC]
    list(Threem1fit,Threem2fit,Threem3fit) 



#Final accuracy vs. final confidence
ggplot(SDMalt1, aes(x=fin_conf,y=fin_acc)) + 
  stat_sum(aes(size = ..n.., group = 3)) +
  scale_size_area(max_size=10)

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

