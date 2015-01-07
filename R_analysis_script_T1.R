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
  require(foreign)

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
  #hist(SDM_cleaned$fin_acc)

#Subject switches to alt answer? Create new variable switch_to_alt (0 or 1). 
  for(i in 1:length(SDM_cleaned[,1])) 
      {if (SDM_cleaned$num_alt[i]==1&&SDM_cleaned$Init_ans[i]!=SDM_cleaned$Fin_ans[i]) 
          if(SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i])   SDM_cleaned$Switch_to_alt[i]=1
       else SDM_cleaned$Switch_to_alt[i]=0
       
       if (SDM_cleaned$num_alt[i]==3&&SDM_cleaned$Init_ans[i]!=SDM_cleaned$Fin_ans[i])
          if(SDM_cleaned$Alt1_ans[i]==SDM_cleaned$Fin_ans[i]||
             SDM_cleaned$Alt2_ans[i]==SDM_cleaned$Fin_ans[i]||
             SDM_cleaned$Alt3_ans[i]==SDM_cleaned$Fin_ans[i])   SDM_cleaned$Switch_to_alt[i]=1
        else SDM_cleaned$Switch_to_alt[i]=0
        }
  
#Create probability using pop freqs of alt answer - initial answer 
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
  
  #Customized minimal theme
  custom_minimal_theme <- theme_minimal() + theme(text = element_text(size= 10), legend.position="none", 
                                                  axis.text.x = element_text(size=rel(1.2)), axis.text.y = element_text(size=rel(1.2)))
  #Change for the session: 
    #theme_set(theme_grey(base_size = 18))                                                 
  
  #Exp 1A
  p1 <- ggplot(Exp1A, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
          geom_smooth(method = 'glm', family = 'binomial', se = FALSE) +     custom_minimal_theme + 
          labs(title = "Exp1A: One Alt under no Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
          scale_colour_gradientn(colours=BluePal) 
      
  #Exp 1B
  p2 <- ggplot(Exp1B, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
          stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + custom_minimal_theme  + 
          labs(title = "Exp1B: One Alt under Time Pressue", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
          scale_colour_gradientn(colours=OrangePal)
    
  #Exp 1C
  
  p3 <- ggplot(Exp1C, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
          stat_smooth(method = 'glm', family = 'binomial', se = FALSE) + custom_minimal_theme  + 
          labs(title = "Exp1C: Three Alt under no Time Pressure", x = "Population Frequency: Alt - Subject", y = "Probability of Switching") +
          scale_colour_gradientn(colours=GreenPal)
    
  #Exp 1D
  p4 <- ggplot(Exp1D, aes(x = SubjectvAlt, y = Switch_to_alt, group = ProgID, color = ProgID)) +
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
  
  #write.csv(SDM_cleaned, file = "SDM_cleaned.csv")
  
  #Example ggplot figure 
  #     ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
  #       geom_line() +
  #       ggtitle("Growth curve for individual chicks")
      
  
#To-do list
  #Model comparison using lme4: 
    #1. Start with Probability of Switching as the DV
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

