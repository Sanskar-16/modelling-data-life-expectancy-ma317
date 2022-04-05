source('part-1.R') 

# library imports
library(mice)

# summarising the analysis
attributes(lifeExpect1)
str(lifeExpect1)
head(lifeExpect1)
summary(lifeExpect1)

# removing these as all/most of the values are null(cut-ff threshold - >50%, ideally should be >30%)
lifeExpect1$SH.HIV.INCD.14  <- NULL
lifeExpect1$SE.PRM.UNER  <- NULL # might be important
# lifeExpect1$SE.PRM.CUAT.ZS  <- NULL  
lifeExpect1$SE.TER.CUAT.BA.ZS   <- NULL
lifeExpect1$SE.PRM.CMPT.ZS   <- NULL # since we removed $SE.PRM.UNER
lifeExpect1$SE.ADT.LITR.ZS  <- NULL
lifeExpect1$FR.INR.RINR  <- NULL
# lifeExpect1$SL.UEM.TOTL.NE.ZS  <- NULL might be imp
lifeExpect1$EG.FEC.RNEW.ZS <- NULL
lifeExpect1$SH.H2O.SMDW.ZS  <- NULL
lifeExpect1$SH.HIV.INCD  <- NULL
lifeExpect1$SI.POV.LMIC <- NULL
summary(lifeExpect1)

# Little EDA
# creating a table format for the missing values, md.pattern visualises the table giving insight
# into the number of missing values for every column
dim(lifeExpect1)
md.pattern(lifeExpect1, rotate.names = TRUE)

# summing up the number of null values for all the columns
colSums(is.na(lifeExpect1))

# mice imputation
my_imp = mice(lifeExpect1, m=5, 
              method = c("", "cart","cart","cart","cart","cart",
                         "cart","cart","cart","cart","cart","cart","cart",
                         "cart","cart","cart","cart"),
              maxit=10)

# summary to inspect and assign the mean value for further inspection
summary(lifeExpect1$SP.DYN.LE00.IN)

meann <- as.numeric(format(round(summary(lifeExpect1$SP.DYN.LE00.IN)[4], 2)))

# empty vector to hold difference values later
a <- c()

my_imp$imp$SP.DYN.LE00.IN

# shows the mean values of the different variants of the imputed columns and 
# assigns difference for further evaluation
for(i in 1:5){
  diff <- as.numeric(mean(my_imp$imp$SP.DYN.LE00.IN[,i])- meann)
  print(
    sprintf("%d column, mean:%.3f, diff:%.3f", i, mean(my_imp$imp$SP.DYN.LE00.IN[,i]), diff)
  )
  a[i] = diff
}

min = which(a == min(a))
cat("The least differnce between the mean the the imputed values is shown in column:", min, '\n')

final_clean_ds = complete(my_imp, min)




