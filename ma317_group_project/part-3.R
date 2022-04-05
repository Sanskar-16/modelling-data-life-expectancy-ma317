source('part-2.R') 

# library imports
library(corrplot)
library(olsrr)

tol_vif_plot <- function(pred_vars) {
  #linear model between SP.DYN.LE00.IN (life expectancy) and all other predictive variables in the dataset
  model = lm(pred_vars$SP.DYN.LE00.IN ~ ., data=pred_vars)
  
  #gets the tolerance and TIF values of the model
  tol_vif_values <- ols_vif_tol(model)
  
  #displays the TIF and  Tolerance values
  tol_vif_values
  
  #creates a new barplot for the TIF values
  barplot(tol_vif_values$VIF, names.arg=tol_vif_values$Variables, las=2, main = "VIF Values", ylim = c(0, 20))
  
  #adds dashed lines to bar chart
  abline(h = 5, lwd = 3, lty = 2,col = 'red')
  abline(h = 10, lwd = 3, lty = 2,col = 'red')
  
  #creates a new barplot for the Tolerance values
  barplot(tol_vif_values$Tolerance, names.arg=tol_vif_values$Variables, las=2, main = "Tolerance Values", ylim = c(0, 0.5))
  
  #adds dashed line to bar chart
  abline(h = 0.1, lwd = 3, lty = 2,col = 'red')
}

par(mfrow=c(1,1))

#create subsets, one of the predictive variables, one without
pred_vars <- subset (final_clean_ds, select = -c(Continent))
other_vars <- subset (final_clean_ds, select = c(Continent))

#set all missing values to 0
pred_vars[is.na(pred_vars)] <- 0

#get correlation matrix for all predictive variables
cor_matrix <-cor(pred_vars)

#get a plot which shows the correlation matrix (titles on left and top, text color black, text size = 0.7)
cor_plot <- corrplot.mixed(cor_matrix, tl.pos = "lt", lower.col = "black", number.cex = 0.7)

#the variables with the highest correlation are NY.ADJ.NNTY.KD.ZG and NY.ADJ.NNTY.PC.KD.ZG
cor(lifeExpect1$NY.ADJ.NNTY.KD.ZG, lifeExpect1$NY.ADJ.NNTY.PC.KD.ZG, method = "pearson", use = "complete.obs")

#The very high correlation between some variables suggests there may be some collinearity in our data, especially when looking at NY.ADJ.NNTY.KD.ZG and NY.ADJ.NNTY.PC.KD.ZG

tol_vif_plot(pred_vars)

#when looking at our VIF values, we have two values above 10 which is not within tolerance, NY.ADJ.NNTY.KD.ZG and NY.ADJ.NNTY.PC.KD.ZG
#when we look at the corresponding Tolerance stats, they are both below 0.1, which is also not within tolerance.

#we choose to remove NY.ADJ.NNTY.PC.KD.ZG as it is the average value per capita, compared to the total amount.
pred_vars <- subset (pred_vars, select = -c(NY.ADJ.NNTY.PC.KD.ZG))

tol_vif_plot(pred_vars)

#combining our variables
final_clean_ds <- cbind(other_vars, pred_vars)
