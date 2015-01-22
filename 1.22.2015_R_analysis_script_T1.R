# Jon's R code for T1 analysis
# Last updated 1/22/2015
# R version 3.1.2 
# RStudio Version 0.98.1091
# Zip file: Complete SDM2.1_1.20.15.zip
# 164 csv files; 
##Potential drops per Dani: CSVs 27, 105, and 112 ***check!***

#Required R Packages
  require(ggplot2)
  require(lme4)
  require(boot)
  require(geepack)
  require(MESS)
  require(MASS)
  require(RColorBrewer)
  require(foreign)

# Import multiple csv files
########## Change folder  ############################################################################
  setwd("M:/R/T1_analysis/1.20.15_DATA")
  readinpath<-"M:/R/T1_analysis/1.20.15_DATA"

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

#Sanity check: 162 csv files (participants) with 123 rows each:  19,926 rows x 47 columns
  dim(CSVDataFrame)
  head(CSVDataFrame)


  
#Create unique ID
  UniqueID<-rep(1:162, each=123)
  CSVDataFrame<-cbind(CSVDataFrame, UniqueID)  
  
 
  #Drop misspelled final answers, fin_acc = 9, but keep fin_acc = 0 or 1
  SDM_cleaned<-subset(CSVDataFrame, fin_acc!=9) #XYZ rows
  #hist(SDM_cleaned$fin_acc)

#Drop init_prop=999 for now
  SDM_cleaned<-subset(CSVDataFrame, Init_prop!=999)
  


#Subject switches to alt answer? Create new variable switch_to_alt (0 or 1). 
  for(i in 1:length(SDM_cleaned[,1])) 
        {if(SDM_cleaned$num_alt[i]==1)
            if(SDM_cleaned$Init_ans[i]!=SDM_cleaned$Fin_ans[i]&&SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i]) 
                  SDM_cleaned$Switch_to_alt[i]=1
         
            else SDM_cleaned$Switch_to_alt[i]=0
        }
  

  for(i in 1:length(SDM_cleaned[,1]))    
         {if(SDM_cleaned$num_alt[i]==3&&SDM_cleaned$Init_ans[i]!=SDM_cleaned$Fin_ans[i])
              if(SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i]||
                 SDM_cleaned$Alt2_ans[i]==SDM_cleaned$Fin_ans[i]||
                  SDM_cleaned$Alt3_ans[i]==SDM_cleaned$Fin_ans[i])
                      SDM_cleaned$Switch_to_alt[i]=1
          
                  else SDM_cleaned$Switch_to_alt[i]=0
        }


#Create probability using pop freqs of alt answer - initial answer
  for(i in 1:length(SDM_cleaned[,1])) 
    {if (SDM_cleaned$num_alt[i]==1) 
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

#Calculate total RT; rescale by dividing by 1000 so model estimation will converge

  SDM_cleaned$TotalRT<-(as.numeric(SDM_cleaned$RT1_keypress) + as.numeric(SDM_cleaned$RT2_Entr) + as.numeric(SDM_cleaned$RT3_conf) + 
                       as.numeric(SDM_cleaned$Alt1_RT) + as.numeric(SDM_cleaned$Alt2_RT) +
                       as.numeric(SDM_cleaned$Alt3_RT) + as.numeric(SDM_cleaned$RT4_fin))/1000
  
#Subset by number of alt answers (num_alt = 1 or 3) and time pressure (time_pres = 0 or 1)
  Exp1A<-subset(SDM_cleaned, num_alt==1&time_pres==0)
  Exp1B<-subset(SDM_cleaned, num_alt==1&time_pres==1)
  Exp1C<-subset(SDM_cleaned, num_alt==3&time_pres==0)
  Exp1D<-subset(SDM_cleaned, num_alt==3&time_pres==1)
      
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
  
  #write.csv(SDM_cleaned, file = "1.21.2015.SDM_cleaned.csv")

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
  
  
  #To-dos
  #Write a fct to do the logistic regs by SS:
  function(uniqueSs, dataset) {}
  
#1B
  
  
  

    
    
    print(unique(Exp1A$UniqueID[i])) 
    
  for(i in 1:length(SDM_cleaned[,1])) 

    testm<-glm(Switch_to_alt~SubjectvAlt,data=(Exp1A$UniqueID==1),family=binomial())
 coef(testm)


  
=======
  multiplot(p1, p2, p3, p4, cols = 2)
  
  #write.csv(SDM_cleaned, file = "1.21.15.SDM_cleaned.csv")


=======
=======
  multiplot(p1, p2, p3, p4, cols = 2)
  
  #write.csv(SDM_cleaned, file = "SDM_cleaned.csv")


# Generalized Linear Mixed-Effects Models (GLMM)
  # Exp 1A
  # Null Model: DV = swtich to alt, Random Effect Intercept of Subject
  Exp1ANullModel<-glmer(Switch_to_alt ~ SubjectvAlt + (1 | ProgID), family = binomial, data = Exp1A)
  yFE.Exp1ANullModel<-predict(Exp1ANullModel, type = "response", re.form = ~0)
  
  yFull.Exp1ANullModel<-predict(Exp1ANullModel, type = "response")
  yFull<-fitted(Exp1ANullModel)
  
  #Fixef plot: Group level  
  ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt)) + 
    geom_line(aes(y =  yFE.Exp1ANullModel)) 
   
  #Plot with individuals
  ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group=ProgID, color = ProgID)) + 
    geom_point() + geom_line(aes(y = yFull)) +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA)) 
  
  
 
    
    
    
    
    
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  
  #Model 1: + Random effect of SubjectvAlt
  Exp1AlModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1| ProgID), family = binomial, data = Exp1A)
  predict(Exp1AlModel1, type = "response")
  
  
  Exp1AlModel1<-glmer(Switch_to_alt ~ SubjectvAlt + (1 + SubjectvAlt| ProgID), family = binomial, data = Exp1A)
  
  ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group=ProgID,
                    color = ProgID)) + geom_point() + geom_line(aes(y = predict(Exp1AlModel1, type = "response"), re.form = 0)) +
    facet_grid(~Q_cat_ID) + theme_minimal() +
    theme(panel.border = element_rect(fill = NA))
  
  
  
  ggplot(Exp1A, aes(x = TotalRT, y = fin_acc, group=ProgID, color = ProgID)) + 
    geom_point() + geom_line(aes(y = preds_overall)) +
    facet_grid(~Q_cat_ID) + theme_minimal() +
    theme(panel.border = element_rect(fill = NA)) 
  
  # Exp 1A
  # Null Model: DV = final accuracy, Random Effect Intercept of Subject
  Exp1ANullModel<-glmer(fin_acc ~ (1 | ProgID), family = binomial, data = Exp1A)
  
  #Model 1: +Fixed effect of switch to alt answer 
  Exp1AModel1<-glmer(fin_acc ~ Switch_to_alt + (1 | ProgID), family = binomial, data = Exp1A)
  
  #Model 2: +Random effect of Total RT 
  Exp1AModel2<-glmer(fin_acc ~ Switch_to_alt + (1 + TotalRT | ProgID), family = binomial, data = Exp1A)
  #coef(Exp1AModel2)  
  #Compare model fits
  anova(Exp1ANullModel,Exp1AModel1, Exp1AModel2)
  
  #Graph Model 1
  Exp1A$preds_overall <- predict(Exp1AModel1, re.form = ~0)
  
  ggplot(Exp1A, aes(x = TotalRT, y = fin_acc, group=ProgID, color = ProgID)) + 
    geom_point() + geom_line(aes(y = preds_overall)) +
    facet_grid(~Q_cat_ID) + theme_minimal() +
    theme(panel.border = element_rect(fill = NA))
  
  #Example ggplot figure 
  #     ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
  #       geom_line() +
  #       ggtitle("Growth curve for individual chicks")
  
  
  #To-do list
  #Model comparison using lme4: 
  #1. Start with Probability of Switching as the DV?
  #2. Accuracy as DV. Factors: Probability of Switching, Pop frequency, RT, category, question difficulty, confidence, self-reported expertise
  #Infer expertise based on titration? Self-reported confidence?
  #Switching behavior using decision trees?
  
  
  
  
  
  
  ##########################Old code########################################################################
  #GLM by participant: http://stackoverflow.com/questions/2970443/break-dataframe-into-subsets-by-factor-values-send-to-function-that-returns-glm  
  #listomodels<-dlply(Exp1A, .(ProgID), function(ProgID) glm(Switch_to_alt~SubjectvAlt, family=binomial(logit), data=Exp1A, subset=ProgID==ProgID))
  
  
  
  
  
  
  ggplot(Exp1A,aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    geom_line(aes(group=ProgID, group = ProgID))
  
  
  +
    facet_grid(~TargetSize) + theme_minimal() +
    theme(panel.border = element_rect(fill = NA))
  
  
  ggplot(Exp1,aes(Accuracy,EstRadPx, group = Subject, color = Subject)) +
    geom_line(aes(group=Subject))
  
  
  ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    geom_line(aes(y=predict(Exp1AOvervall), group = ProgID))
  
  
  
  Exp1AOvervall<-glm(Switch_to_alt ~SubjectvAlt, family=binomial(logit), data =  Exp1A) 
  
  unique(Exp1A$ProgID)
  length(Exp1A$ProgID)
  
  Exp1AOvervall<-glm(Switch_to_alt ~SubjectvAlt, family=binomial(logit), data =  Exp1A , subset=ProgID==605)
  Create list of concatanated models?
  
  
  PredAlt1Overall<-predict(Exp1AOvervall)
  
  ggplot(Exp1,aes(Accuracy,EstRadPx, group = Subject, color = Subject)) +
    geom_line(aes(group=Subject)) +
    facet_grid(~TargetSize) + theme_minimal() +
    theme(panel.border = element_rect(fill = NA))
  
  
  ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
    geom_line(aes(group = ProgID))
  
  
  
  
  
  
  ggplot(SDMalt1, aes(x = diff , y = fin_acc,color = ProgID)) + geom_point(position = position_jitter(w = 0.05, h=0.05)) + geom_line(aes(y = PredAlt1Overall))  +  theme_minimal()
  
  
  x = SubjectvAlt y = 
    
    
    
    plogis(Alt1logistic)
  
  ggplot(SDMalt1, aes(x = diff , y = fin_acc, group=ProgID,
                      color = ProgID)) + geom_point(position = position_jitter(w = 0.05, h=0.05)) + geom_line(aes(y = PredAlt1Overall))  +  theme_minimal()
  
  
  
  facet_grid(~TargetSize) +
    
    
    
