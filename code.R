data <- read.csv(file.choose(), header = TRUE) # use the csv diamonds.csv
attach(data)
names(data)
View(data)
summary(data)

# delete the zeros since we see min x,y,z have 0 from the summary (possible errors of measure)

df <- data[,c("x","y","z")]
zeros <- any(df == 0, na.rm =FALSE)
if (zeros){
  cat("There are zeros in the columns x,y,z\n")
  zeros_pos <- which(df==0, arr.ind = TRUE) # which find the indices and arr.ind tells to give as output an array or matrix
  cat("Positions of zeros are\n")
  print(zeros_pos)
}
library(dplyr)
zeros_pos_clean <- zeros_pos[,"row"]
dataclean <- slice(data, -zeros_pos_clean)


# now we deal with the categorical variables, we first get which categories we have
listofclarity <- unique(dataclean$clarity)
listofcut <- unique(dataclean$cut)
listofcolor <- unique(dataclean$color)

library(fastDummies)

# hence we transform the categorical variables into dummy ones
dataclean_dum <- dummy_cols(dataclean, select_columns = c("cut","clarity","color"), remove_selected_columns = TRUE, remove_first_dummy = TRUE) 

# check if there are other NA that causes problems
sum(is.na(dataclean_dum)) 

# now we look for outliers:
boxplot(dataclean_dum$carat, outline = TRUE,ylab = "Carat")
outliers_ca <- boxplot.stats(dataclean_dum$carat)$out
points(rep(1, length(outliers_ca)), outliers_ca, col = "red", pch=1)

boxplot(dataclean_dum$depth, outline = TRUE, ylab = "Depth")
outliers_de <- boxplot.stats(dataclean_dum$depth)$out
points(rep(1, length(outliers_de)), outliers_de, col = "red", pch=1)

boxplot(dataclean_dum$table, outline = TRUE, ylab = "Table")
outliers_ta <- boxplot.stats(dataclean_dum$table)$out
points(rep(1, length(outliers_ta)), outliers_ta, col = "red", pch=1)

boxplot(dataclean_dum$price, outline = TRUE, ylab = "Price")
outliers_pr <- boxplot.stats(dataclean_dum$price)$out
points(rep(1, length(outliers_pr)), outliers_pr, col = "red", pch=1)


# So the red points are the outliers
data_up_noout <- subset(dataclean_dum, !(carat %in% outliers_ca))
data_up_noout <- subset(dataclean_dum, !(depth %in% outliers_de))
data_up_noout <- subset(dataclean_dum, !(table %in% outliers_ta))
data_up_noout <- subset(dataclean_dum, !(price %in% outliers_pr))

# now that the outliers are out, we look for correlation between covariates
cor_matrix <- cor(data_up_noout)
library(corrplot)
corrplot(cor_matrix, 
         method = "color",
         tl.col = "black",   
         tl.srt = 45,       
         main = "Correlation Heatmap" 
)
# So we clearly see that x,y,z are not independent so we can use a unique information to represent the 3, i.e. the volume
# Then, the carat and x,y,z are not independent and so we drop directly the column volume that we would create as said before

# So now we delete the columns: x,y,z and the first column X because it's juts indexing
data_up <- data_up_noout[,-c(1,6,7,8)]
corn_matrix <- cor(data_up)
corrplot(corn_matrix, 
         method = "color",
         tl.col = "black",   
         tl.srt = 45,       
         main = "Correlation Heatmap" 
)

#now we look for correlations between attributes and price

plot(data_up$carat, data_up$price, xlab = "Carat", ylab = "Price") # notice squared relation
plot(data_up$table, data_up$price, xlab = "Table", ylab = "Price")
plot(data_up$depth, data_up$price, xlab = "Depth", ylab = "Price")

# Now we perform the Multiple Linear Regression

colnames(data_up)[8]<- "cut_VeryGood" # Just renaming the column 9 to avoid the nasty space
mod <- lm(price ~ carat + I(carat^2) + depth + table + cut_Good + cut_Ideal + cut_Premium + cut_VeryGood + clarity_IF +clarity_SI1 + clarity_SI2 + clarity_VS1 + clarity_VS2 + clarity_VVS1 + clarity_VVS2 + color_E + color_F + color_G + color_H + color_I + color_J, data = data_up) 

# summary
pred <- predict(mod, data_up)
res <- residuals(mod)
is.numeric(res)  #just creating variables that will be useful, and making sure that are numbers
summary(mod)

coefficients <- coef(mod)
print(coefficients)
plot(pred, data_up$price, xlab = "Predicted prices", ylab = "Prices")
abline(a=0, b=1, col = "red")


# Histogram of residuals
qqnorm(res)
qqline(res)
hist(res, xlim = range(res), col = "lightblue", main = "Residuals Histogram", xlab = "Residuals")
# in practice we are overestimating the prices since e_i's are negative
# Moreover we have fat tails that suggests the non-normality (later we perform the test to check this on the final model)

# analysis of normality of errors ei N(0,sigma squared)
plot(fitted(mod), res, xlab="Fitted", ylab = "Residuals") # as fitted increases the variance increases so we take a concave transformation coxbox
abline(h=0, col = "red")

# so now we try to take the transformation of the Y

mod_new <- lm((price)^(1/3) ~ carat + I(carat^2) + depth + table + cut_Good + cut_Ideal + cut_Premium + cut_VeryGood + clarity_IF + clarity_SI1 + clarity_SI2 + clarity_VS1 + clarity_VS2 + clarity_VVS2 + color_E + color_F + color_G + color_H + color_I + color_J, data = data_up) 

# summary of the new regression
pred <- predict(mod_new, data_up)
res_new <- residuals(mod_new)
is.numeric(res_new)
summary(mod_new)

coefficients <- coef(mod_new)
print(coefficients)
plot(pred, (data_up$price)^(1/3))
abline(a=0, b=1, col = "red")


# Histogram of residuals
qqnorm(res_new)
qqline(res_new)
hist(res_new) 

# analysis of normality of errors ei N(0,sigma squared)
plot(fitted(mod_new), res) 
abline(h=0, col = "red")

# now we check for interactions effects of categorical variables with ANOVA
anova(lm((price)^(1/3) ~ cut_Ideal*clarity_VVS1*color_E + cut_Premium*clarity_VVS2*color_F + cut_VeryGood*clarity_VS1*color_G + cut_Good*clarity_SI1*color_I, data = data_up))

interaction.plot( # interaction plot for clarity_VVS1 and cut_Ideal
  x.factor = data_up$clarity_VVS1,
  trace.factor = data_up$cut_Ideal,
  response = data_up$price,
  type = "b", 
  col = c("red", "blue"),  
  pch = c(1, 2),  
  main = "Interaction Plot",
  xlab = "clarity_VVS1",
  ylab = "Prices",
  legend = TRUE
)

# Now we perform a step down model selection
mod_last1 <- lm((price)^(1/3) ~ carat + I(carat^2) + depth + table + cut_Good + cut_Ideal + cut_Premium + cut_VeryGood + clarity_IF + clarity_SI1 + clarity_SI2 + clarity_VS1 + clarity_VS2 + clarity_VVS2 + color_E + color_F + color_G + color_H + color_I + color_J + cut_Ideal*clarity_VVS1 + clarity_VVS2*color_F + cut_VeryGood*clarity_VS1 + cut_VeryGood*color_G + cut_Good*clarity_SI1 + cut_VeryGood*clarity_VS1*color_G, data = data_up)
mod_last2 <- lm((price)^(1/3) ~ carat + I(carat^2) + depth + table + cut_Good + cut_Ideal + cut_Premium + cut_VeryGood + clarity_IF + clarity_SI1 + clarity_SI2 + clarity_VS1 + clarity_VS2 + clarity_VVS2 + color_E + color_F + color_G + color_H + color_I + color_J + cut_Ideal*clarity_VVS1 + clarity_VVS2*color_F + cut_VeryGood*clarity_VS1 + cut_Good*clarity_SI1 + cut_VeryGood*clarity_VS1*color_G, data = data_up)
mod_last3 <- lm((price)^(1/3) ~ carat + I(carat^2) + depth + table + cut_Good + cut_Ideal + cut_Premium + cut_VeryGood + clarity_IF + clarity_SI1 + clarity_SI2 + clarity_VS1 + clarity_VS2 + clarity_VVS2 + color_E + color_F + color_G + color_H + color_I + color_J + cut_Ideal*clarity_VVS1 + clarity_VVS2*color_F + cut_VeryGood*clarity_VS1 + cut_Good*clarity_SI1, data = data_up)
mod_last <- lm((price)^(1/3) ~ carat + I(carat^2) + depth + table + cut_Good + cut_Ideal + cut_Premium + cut_VeryGood + clarity_IF + clarity_SI1 + clarity_SI2 + clarity_VS1 + clarity_VS2 + clarity_VVS2 + color_E + color_F + color_G + color_H + color_I + color_J + cut_Ideal*clarity_VVS1 + clarity_VVS2*color_F + cut_VeryGood*clarity_VS1, data = data_up)

summary(mod_last)
pred <- predict(mod_last, data_up)
res_new <- residuals(mod_last)
is.numeric(res_new)
coefficients <- coef(mod_last)
print(coefficients)
plot(pred, (data_up$price)^(1/3), xlab="Predicted Prices", ylab="Prices") #scaled at price^1/3 
abline(a=0, b=1, col = "red",lwd = 3.5)

# Histogram of residuals
qqnorm(res_new)
qqline(res_new)
hist(res_new, col="lightblue")
# analysis of normality of errors ei N(0,sigma squared)
plot(fitted(mod_last), res, xlab="Fitted", ylab="Residuals")
abline(h=0, col="red")

# Kolmogorov-Smirnov test confirms that a statistically significant deviation from normality exists in the residuals
set.seed(42)
is.numeric(data_up$price)
mean <- mean(data_up$price)
sd <- sd(data_up$price)
sampledata <- rnorm(length(data_up$price), mean = mean, sd = sd)
res_test <- ks.test(sampledata, data_up$price)
print(res_test)

#BIC for the models obtained with step-down, model selection
BIC(mod_last1, mod_last2, mod_last3, mod_last)

