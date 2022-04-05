#the package used to create a formal summary table 
library(stargazer)

#reading in the data
lifeExpect1 = read.csv("../Life_Expectancy_Data1.csv")

#First a few quick summaries of the data
dim(lifeExpect1) # this tells us there are 217 rows and 29 columns
attributes(lifeExpect1)

#next looking at the variables more specifically
str(lifeExpect1)
summary(lifeExpect1)

#we can create a nice output table for each variable using the stargazer package
stargazer(lifeExpect1, type = "html", out = "../lifeExpectSummary.html")

#to confirm, all of our predictor variables (besides country name and country code) seem to be continious
#lets to a check on how many levels there are per variables
sapply(lifeExpect1,function(x) length(unique(x)))

#excluding re_energy/EG.FEC.RNEW.ZS (which is column with all missing), we can see that continent would be worth turing into
#a factor within R
table(lifeExpect1$Continent)

#we can see that one value is left blank, lets look at the country name
lifeExpect1[lifeExpect1$Continent == "",]$Country.Name
#we can see that it's the Solomon Islands, therefore it's continent should be 'Australia/Oceania'
lifeExpect1[lifeExpect1$Continent == "",]$Continent = "Australia/Oceania"

#now lets look at the table again
table(lifeExpect1$Continent)

#good amount of observations per category, we shall keep all, time to convert
lifeExpect1$Continent = as.factor(lifeExpect1$Continent)

#the variable SP.POP.TOTL is in scientific notation format, we should convert this before going on so all continuous variables are
#in the same format, this is done by changing the option of how R represents these variables
options(scipen = 999)

#The codebook/legend in the coursework tells us that SP.DYN.LE00.IN (SP.DYN.LE00.IN) is Life expectancy at birth, total (years)
#Therefore SP.DYN.LE00.IN (SP.DYN.LE00.IN) is the variable we are interested in predicting/ the dependent variable
summary(lifeExpect1$SP.DYN.LE00.IN)
var(complete.cases(lifeExpect1$SP.DYN.LE00.IN))

#creating a histogram of life expectancy (SP.DYN.LE00.IN) with bins of size 5 each.
hist(lifeExpect1$SP.DYN.LE00.IN,
     xlab = "Life Expectancy at Birth (years)",
     main = "Histogram of Life Expectancy at Birth",
     ylim = c(0,60),
     labels = TRUE)

#From the above histogram we can see that the mode bin is 75 - 80.

#going forward, we should remove country name and country code from our dataset of variables to analyze since they
#provide little to no usage in predicting future countries (a factor/categorical variable which has 1 unique value per
#observation provides little/no insight for predictions)
lifeExpect1.cnames = subset(lifeExpect1, select = c(Country.Name,Country.Code))
lifeExpect1 = subset(lifeExpect1, select = -c(Country.Name,Country.Code))

#after removing those vars, lets investigate the relationship between the dependent variable, SP.DYN.LE00.IN, and all predictor variables
#to do this, we will write a function which we will loop all the variables though
relationCheck = function(data){
  
  #removing the NA' values from the data/getting only the complete cases
  data = na.omit(data)
  
  #create model and summary
  model = lm(SP.DYN.LE00.IN ~ ., data = data)
  print(summary(model))
  
  #we then get the relevant plots for the model to evaluate
  plot(data[[names(data)[2]]],data[[names(data)[1]]],
       xlab = names(data)[2],
       ylab = names(data)[1])
  abline(model,col=2,lwd=2)
  
  #Get plots for each linear model, uncomment to see all (produces alot of plots)
  #plot(model)
  
}

#Here we set a graphical setting to ask for user input before
#each graph. 
####Uncomment this if running this section to see all graphs!####
#par(ask = T)

#We then make a temp dataframe that doesn't include Continent and 
#EG.FEC.RNEW.ZS since it has no values
loopLifeExpect = subset(lifeExpect1,select = -c(Continent, EG.FEC.RNEW.ZS))

#preforming a loop on lifeExpect so that we can view each continuous variables
for (i in 1:ncol(loopLifeExpect)){
  if (i != 1) {
    relationCheck(subset(lifeExpect1, select = c(names(loopLifeExpect)[1],names(loopLifeExpect)[i])))
  }
}

#remove the variable to avoid confusion in later parts
rm(loopLifeExpect)

#From these graphs we can see that the following variables have
#a good linear relationship with Life Expectancy
#We should look to keep these in a linear model
  #EG.ELC.ACCS.ZS
  #SP.DYN.IMRT.IN
  #SP.DYN.CBRT.IN
  #SE.PRM.CMPT.ZS
  #SP.POP.GROW

#before modelling , we need to scale the continuous variables (excluding the dependant variable), can achieve this using a for loop
for (i in names(lifeExpect1)) {
  if (is.numeric(lifeExpect1[[i]]) & i != "SP.DYN.LE00.IN") {
    lifeExpect1[[i]] = as.vector(scale(lifeExpect1[[i]]))
  }
}

#now lets do a boxplot to check for outliers (excluding the dependent variable)
boxplot(subset(lifeExpect1, select = -c(Continent, SP.DYN.LE00.IN)))

#Using these boxplots and the previous linear graphs, we have
#Some outliers we should remove to get better fits
  #SH.HIV.INCD.14, remove row (20000+)
  #SE.PRM.UNER, remove rows (1500000+)
  #EN.POP.DNST, remove rows (5000+)
  #SP.POP.TOTL, remove rows (1200000000+)
  #SH.XPD.CHEX.PC.CD, remove rows (~8000+)
  #NY.GDP.MKTP.KD.ZG, remove row (19+)
  #NY.GDP.PCAP.CD, remove rows (100000+)
  #SH.HIV.INCD, remove row (150000+)
  #SI.POV.LMIC, remove row (60+)

summary(lifeExpect1)
#From having a look at the summary, we can see that the some of the variables have a high amount of missing data
#SH.HIV.INCD.14; c_hiv ( Children (ages 0-14) newly infected with HIV): NA = 127
#SE.PRM.CUAT.ZS; pri_edu (Educational attainment, at least completed primary): NA = 181
#SE.TER.CUAT.BA.ZS; bcs_edu (Educational attainment, at least Bachelor's or equivalent): NA = 179
#SE.ADT.LITR.ZS; lit_rate  (Literacy rate, adult total): NA = 192
#EG.FEC.RNEW.ZS; re_energy (Renewable energy consumption): NA = 217 (All is missing!)
#SI.POV.LMIC; pov_head ( Poverty headcount ratio at \$3.20 a day): NA = 195

#in part 2, we will tackle these missing values accordingly