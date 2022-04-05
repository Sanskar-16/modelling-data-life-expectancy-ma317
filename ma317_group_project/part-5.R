source('part-4.R') 

# load libraries
library(car)
library(stats)

par(mfrow=c(1,1))

# checking the data and dimensions 
head(final_clean_ds)
dim(final_clean_ds)

# one-way ANOVA ( differences in average life expectancy across the continents)
life_expectancy <- final_clean_ds$SP.DYN.LE00.IN
continent <- final_clean_ds$Continent

# shows the average life expectancy across each continent
group.means<-tapply(life_expectancy, continent, mean)
group.means

# boxplot to show the comparison of life expectancy within each continent
boxplot(life_expectancy~continent,main='Comparing the life expectancy across continents',
        xlab='Continent', col="light gray", ylab = "Life expectancy (age in years)",)

# factor variables
# final_clean_ds$Continent <- factor(final_clean_ds$Continent, levels=c("Africa","Asia","Australia/Oceania", "Europe", "North America", "South America"))

anova1way<-aov(life_expectancy~as.factor(continent),data=final_clean_ds)
summary(anova1way)

# reject the null hypothesis because our P value in our ANOVA table is significant at the 0.001 level.

# post-hoc tests
cat("Bonferroni post-hoc test","\n")

# Bonferroni post-hoc test
pairwise.t.test(final_clean_ds$SP.DYN.LE00.IN, final_clean_ds$Continent, p.adj = "bonferroni")

cat("\n","Tukey post-hoc test","\n")

# Tukey post-hoc test
tukey.continent<-TukeyHSD(anova1way)

plot(tukey.continent)

# As seen, the North-America - Asia is outside of the mean

# one-way ANOVA assumptions
final_clean_ds$residuals1<-anova1way$residuals
par(mfrow=c(1,2))

# standardised residuals histogram
hist(final_clean_ds$residuals1, main="Standardised residuals-histogram",xlab="Standardised residuals")

# qq plot
qqnorm(final_clean_ds$residuals1,pch=19)
qqline(final_clean_ds$residuals1)

# Shapiro test
shapiro.test(final_clean_ds$residuals1)

# Shapiro test shows the data is/n't normally distributed

# Bartlett test
bartlett.test(final_clean_ds$residuals1~as.numeric(final_clean_ds$Continent))

# levene Test
leveneTest(life_expectancy~factor(final_clean_ds$Continent))

# welche one-way ANOVA
continent.welchtest<-oneway.test(life_expectancy~Continent,data=final_clean_ds)
continent.welchtest

