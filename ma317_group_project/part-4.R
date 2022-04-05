source('part-3.R') 

reg_data <- final_clean_ds

summary(final_clean_ds)

# reg_data <- final_clean_ds[!names(final_clean_ds) %in% c("Country.Code", "Country.Name", "Continent")]
reg_data <- final_clean_ds[!names(final_clean_ds) %in% c("Country.Code", "Country.Name")]


library('MASS')
library('leaps')
library('caret')

attach(reg_data, warn.conflicts = F)

index <- createDataPartition(SP.DYN.LE00.IN , p=0.8, times = 1, list = F)

reg_data_train <- reg_data[index,]
reg_data_test <- reg_data[-index,]

nrow(reg_data_train)
nrow(reg_data_test)

full_model <- lm(SP.DYN.LE00.IN ~ ., data = reg_data_train, x = T)

null_model <- lm(SP.DYN.LE00.IN ~ 1, data = reg_data_train)

# looks like 
# Continent
# SP.DYN.IMRT.IN
# SP.POP.GROW
# EN.POP.DNST
# SH.XPD.CHEX.PC.CD
# SP.DYN.CBRT.IN 

# are the significant variables, taking a first look at the full model
# interpreting lm summary https://stats.stackexchange.com/questions/5135/interpretation-of-rs-lm-output
summary(full_model)

# interpreting anova https://stats.stackexchange.com/questions/115304/interpreting-output-from-anova-when-using-lm-as-input
anova(full_model)

library(leaps) # step

# our data is not that large and the number of predictors is also manageable, we
# can use exhaustive search to find the best model given a metric

# exhaustive sear via Mallow's Cp ==============================================
X <- full_model$x
y <- reg_data_train$SP.DYN.LE00.IN

all_models <- leaps(X, y, int = F, strictly.compatible = F, method="Cp")

# source: lab 3
plot(all_models$size, all_models$Cp, log = "y", xlab = "|M|",
     ylab = expression(c[p]), ylim = c(1,200))
lines(all_models$size, all_models$size)

min_cp_val <- all_models$Cp == min(all_models$Cp)
min(all_models$Cp)
min_cp <- all_models$which[min_cp_val, ]
min_cp

model_full_cp <- lm(SP.DYN.LE00.IN ~ Continent + EG.ELC.ACCS.ZS + 
                         SP.DYN.IMRT.IN + SP.POP.GROW + EN.POP.DNST + 
                         SH.XPD.CHEX.PC.CD + SP.DYN.CBRT.IN,
                       data = reg_data_train)
summary(model_full_cp)
anova(model_full_cp)
# testing wrapper methods ======================================================

model_step_backward <- step(full_model, method = "backward")
summary(model_step_backward)

model_step_forward <- step(null_model, method = "forward",
              scope = 
                # the predictors signified as significant by the lm full_model 
                # summary
                ~ Continent + SP.DYN.IMRT.IN + SP.POP.GROW + 
                EN.POP.DNST + SH.XPD.CHEX.PC.CD + SP.DYN.CBRT.IN,
                data = reg_data_train)

summary(model_step_forward)

# Here I am testing out the forwards/backwards step-wise algorithm introduced
# in the lecture
motel_step_both <- stepAIC(full_model, directoin = "both") # worse than step forward
summary(motel_step_both)

# evaluation ===================================================================

summary(full_model)
summary(model_full_cp)
summary(model_step_backward)
summary(model_step_forward)
summary(motel_step_both)

anova(model_full_cp, full_model)
anova(model_step_backward, full_model)
anova(model_step_forward, full_model)
anova(motel_step_both, full_model)

reg_data_test$pred_red_SP.DYN.LE00.IN <- predict(model_full_cp, reg_data_test)
reg_data_test$pred_ful_SP.DYN.LE00.IN <- predict(full_model, reg_data_test)

RMSE(reg_data_test$pred_red_SP.DYN.LE00.IN, reg_data_test$SP.DYN.LE00.IN)
# 1.472737
R2(reg_data_test$pred_red_SP.DYN.LE00.IN, reg_data_test$SP.DYN.LE00.IN)
# 0.9570669

RMSE(reg_data_test$pred_ful_SP.DYN.LE00.IN, reg_data_test$SP.DYN.LE00.IN)
# 1.482689
R2(reg_data_test$pred_ful_SP.DYN.LE00.IN, reg_data_test$SP.DYN.LE00.IN)
# 0.9569355

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(full_model)

