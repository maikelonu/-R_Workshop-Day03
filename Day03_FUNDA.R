# //////////////////////////////////////////////////////////////////////////////////
# INSTITUTO TECNOLOGICO DE COSTA RICA
# Escuela de Ingenieria en Construccion
# https://www.tec.ac.cr
# Day 03. Machine Learning and Regression

# M.Sc. Eng. Maikel Mendez M
# Water Resources + GIS + DataScience
# Instituto Tecnologico de Costa Rica
# https://www.tec.ac.cr
# https://orcid.org/0000-0003-1919-141X
# https://www.scopus.com/authid/detail.uri?authorId=51665581300
# https://scholar.google.com/citations?user=JnmSVFYAAAAJ&hl=en
# https://www.youtube.com/c/maikelmendez
# https://twitter.com/MaikelMendezM
# https://github.com/maikelonu
# //////////////////////////////////////////////////////////////////////////////////

# ////////////////////////////////////////////////////////
# INFO:
# ////////////////////////////////////////////////////////
# 1_Regression is a supervised learning method, which is employed to model and analyze 
# the relationship between a dependent (response) variable and one or more independent (predictor) variables.
#
# 2_One can use regression to build a prediction model, which can first be
# used to find the best fitted model with minimum squared errors of the fitted values.
# 
# 3_The fitted model can then be further applied to data for continuous value predictions.
# ////////////////////////////////////////////////////////

# Workspace is cleared
#rm(list = ls())

# Working directory is selected
#setwd("C:/DATOS/FUNDA_R/R_Workshop/Day03")
setwd("C:/Users/TEC/Google Drive/FUNDA_R/R_Workshop/Day03")

# CRAN libraries are requested
require(car)
require(corrplot)
require(DescTools)
require(effects)
require(corrgram)
require(ggplot2)
require(pastecs)
require(visreg)

# Performance of a 2009-Nissan Tiida 1.6L
df.tiida <- read.table("tiida.txt",header=T,sep="\t",quote="")

# View {utils} function is requested
# View(df.tiida)

# Desc {DescTools} function is requested
Desc(df.tiida, plotit = TRUE)

# names function is requested
names(df.tiida)

# corrgram {corrgram} function is applied over relevant data.frames
corrgram(df.tiida, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Nissan Tiida-2009 Performance")

# corrgram {corrgram} function is applied over relevant data.frames
corrgram(df.tiida, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Nissan Tiida-2009 Performance") 

# /////////////////////////////////////////////////
# Using corrplot library functions
# /////////////////////////////////////////////////

# Computing correlation matrix
M <- cor(df.tiida[, 3:8])
head(round(M,2))

# Visualization method "circle"
corrplot(M, method="circle")

# Visualization method "pie"
corrplot(M, method="pie")

# Visualization method "colour"
corrplot(M, method="color")

#Display the correlation coefficient :
corrplot(M, method="number")

# /////////////////////////////////////////////////
# Fitting a linear regression model with lm
# /////////////////////////////////////////////////

# A ggplot object is created
ggplot(aes(x = vol_GAL, y = dist_km), data = df.tiida) + 
  geom_point(color = "red", size = 4.5, alpha = 0.90) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Modelo lm, Distancia versus Volumen") +
  xlab("Volumen (GAL)") +
  ylab("Distancia (km)") +
  theme_bw(base_size = 22.0)

# A lm {stats} model is created
lmfit <- lm(dist_km ~ vol_GAL, data = df.tiida)

# visreg {visreg} function is requested
visreg(lmfit, partial = TRUE)

# effect {effects}
plot(allEffects(lmfit))

# A ggplot object is created
ggplot(aes(x = vol_GAL, y = dist_km), data = df.tiida) +
  geom_point(color = "red", size = 3.0, alpha = 0.90) +
  geom_smooth(size = 0.85,alpha = 0.35,method = lm, fullrange = TRUE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Modelo lm, Distancia versus Volumen") +
  xlab("Volumen (GAL)") +
  ylab("Distancia (km)") +
  theme_bw(base_size = 22.0)

# A summary {base} function is requested 
summary(lmfit)

# formula y = A + Bx, where A is the intercept while the slope, B, describes 
# the change in y when x changes
# For instance:
# dist_km = 56.21 + 57.70*vol_GAL
# In other words, there is an increase of 57.70 units of dist_km for
# every 1 unit of vol_GAL

# /////////////////////////////////////////////////
# lm SECTIONS:
# /////////////////////////////////////////////////
# 
# Residuals section:
# it provides a quick summary (min, 1Q, median, 3Q, max) of the distribution.
# 
# Coefficients section: each coefficient is a Gaussian random variable
# Estimate represents the mean distribution of the variable
# Std.Error displays the standard error of the variable
# the t value is Estimate divided by Std.Error
# the p value indicates the probability of getting a value larger than the t value
#
# Residual standard error outputs the standard deviation of residuals
#
# The degree of freedom indicates the differences between the observation 
# in training samples and the # number used in the model
#
# Multiple R-squared is obtained by dividing the sum of squares.
#
# Adjusted R-squared uses an unbiased estimate, 
# and will be slightly less than multiple R-squared

# /////////////////////////////////////////////////

# A str {base} is requested 
str(lmfit)

# lmfit coefficients are requested
lmfit$coefficients

# lmfit residual are requested
lmfit$residuals
sd(lmfit$residuals)

# shapiro.test {stats} Normality Test is applied
# if p-value > 0.05 then normality stands true, meaning that
# the variable is parametric
shapiro.test(lmfit$residuals)

# lmfit fitted.values are requested
lmfit$fitted.values

# A data.frame is created
df.residuals <- as.data.frame(lmfit$residuals)

# names {base} functions is requested
names(df.residuals) <- c("residuals")

# names {base} functions is requested
names(df.residuals)

# A ggplot2 object is created
# ggplot(aes(x = X, y = Y), data = data.frame) + ....
ggplot(aes(y = ..density.., x = df.residuals$residuals), data = df.residuals) +
  geom_histogram(colour = "gray", fill = "yellow", size = 0.75, alpha = 0.95) +
  geom_density(size = 0.75, alpha = 0.95) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Histograma + Densidad. Residuales (lmfit)") +
  xlab("Residuales (km)") +
  ylab("Densidad (-)") +
  theme_grey(base_size = 22.0)

# A summary function is requested
summary(df.residuals)

# A vector is created
vector01 <- df.residuals$residuals/sd(df.residuals$residuals)

# A summary function is requested
summary(vector01)

# A summary function is requested
summary((vector01)^0.5)

# /////////////////////////////////////////////////
# Generating a diagnostic plot of a fitted model
# /////////////////////////////////////////////////

# A full lm-model diagnostic plot is requested
oldpar.lm <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(lmfit)
par(oldpar.lm)

# RESIDUALS VERSUS FITTED VALUES
# Residuals represent the vertical distance from a point to the regression line
# If all points fall exactly on the regression line, all residuals will fall exactly on the dotted gray line.
# The red line within the plot is a smooth curve with regard to residuals
# [SUGGESTS HOMOGENEITY,LINEARITY, CONSTANT VARIANCE (VARIABLES INDEPENDANCE)]
#
# QQ-PLOT (NORMAL OF RESIDUALS)
# This plot verifies the assumption that residuals were normally distributed.
# Thus, if the residuals were normally distributed, they should lie exactly on the gray dash line
# [SUGGESTS NORMALITY]
#
# SCALE-LOCATION PLOT
# It measures the square root of the standardized residuals against the fitted value
# Therefore, if all dots lie on the regression line, the value of y should be close to zero.
# Since it is assumed that the variance of residuals does not change the distribution substantially,
# if the assumption is correct, the red line should be relatively flat.
# [SUGGESTS THAT VARIANCE HOMOGENEITY HOLDS TRUE]
#
# STANDARDIZED RESIDUALS VERSUS LEVERAGE
# The leverage is a measurement of how each data point influences the regression.
# It is a measurement of the distance from the centroid of regression and level of isolation
# [SUGGESTS THAT VARIANCE IS SENSITIVE TO OUTLIERS]
# 
# COOK'S DISTANCE
# It measures how regression would change if a single point is deleted.
# Cook's distance is affected by HIGH leverage and LARGE residuals.
# For a perfect fit regression, the red line should be close to the dashed
# line with no points over 0.5 in Cook's distance contour.

# /////////////////////////////////////////////////
# Using linear regression to predict unknown values
# /////////////////////////////////////////////////
# 
# With a fitted regression model, we can apply the model to predict unknown values.
# For regression models, we can express the precision of prediction with a
# prediction interval and a confidence interval.

# predict {stats} function is applied over lm model
df.predict.pred <- round(as.data.frame(predict(lmfit, interval = "prediction")),3)
df.predict.conf <- round(as.data.frame(predict(lmfit, interval = "confidence")),3)
df.predict.resi <- round(as.data.frame(lmfit$residuals),3)

# names {base} function is applied over relevant data.frames
names(df.predict.pred) <- c("fit_pred", "lwr_pred", "upr_pred")
names(df.predict.conf) <- c("fit_conf", "lwr_conf", "upr_conf")
names(df.predict.resi) <- c("residuals")

# cbind {base} function is applied over relevant data.frames
df.tiida <- cbind(df.tiida,
                  df.predict.resi,
                  df.predict.pred,
                  df.predict.conf)

# Assign values to be predicted into data.frame df.newdata
df.newdata <- data.frame(vol_GAL = c(5.45,8.32,9.12))

# Compute the prediction result using the confidence interval with level set as 0.95
predict(lmfit, df.newdata, interval = "confidence", level = 0.95)

# Compute the prediction result using this prediction interval
predict(lmfit, df.newdata, interval = "predict", level = 0.95)

# Prediction intervals and confidence intervals ARE NOT not the same thing !!!
# 
# A prediction interval is an interval associated with a random variable
# yet to be observed, with a specified probability of the random variable
# lying within the interval (POPULATION).
# 
# A confidence interval is an interval associated with a parameter and 
# is a frequentist concept (MODEL).
# The parameter is assumed to be non-random but unknown, and the 
# confidence interval is computed from data. 

# ///////////////////////////////////////////
# Normality probabilities
# ///////////////////////////////////////////
names(df.tiida)

# A ggplot2 object is created
# ggplot(aes(x = X, y = Y), data = data.frame) + ....
ggplot() +
  geom_boxplot(aes(y = performance_km_Gal,x = vehicle),data=df.tiida,colour = '#0000ff',size = 0.75,outlier.colour = '#ff0000',outlier.shape = 16,outlier.size = 3.5) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Nissan Tiida Performance") +
  xlab("Vehicle") +
  ylab("Performance (km/GAL)") +
  theme_bw(base_size = 22.0)

# shapiro.test {stats} Normality Test is applied
# if p-value > 0.05 then normality stands true, meaning that
# the variable is parametric
shapiro.test(df.tiida$performance_km_Gal)

# Outliers are removed from data.frame
df.tiida2 <- subset(df.tiida, performance_km_Gal < 80) 

# shapiro.test {stats} Normality Test is applied
# if p-value > 0.05 then normality stands true, meaning that
# the variable is parametric
shapiro.test(df.tiida2$performance_km_Gal)

# A lm {stats} model is created
lmfit.out = lm(dist_km ~ vol_GAL, data = df.tiida2)

# A summary function is requested
summary(lmfit.out)

# Desc {DescTools} function is requested
Desc(df.tiida2, plotit = TRUE)

# shapiro.test {stats} Normality Test is applied
# if p-value > 0.05 then normality stands true, meaning that
# the variable is parametric
shapiro.test(df.tiida2$performance_km_Gal)

# Normality parameters are calculated
mean.tiida <- mean(df.tiida2$performance_km_Gal)
sd.tiida <- sd(df.tiida2$performance_km_Gal)

# 67% probability
mean.tiida - 1*sd.tiida # lower limit
mean.tiida + 1*sd.tiida # upper limit  

# 95% probability
mean.tiida - 2*sd.tiida # lower limit
mean.tiida + 2*sd.tiida # upper limit

# pnorm function is applied to various independant values to
# find cumulative probabilities
pnorm(55, mean = mean.tiida, sd = sd.tiida, lower.tail = FALSE)
pnorm(60, mean = mean.tiida, sd = sd.tiida, lower.tail = FALSE)
pnorm(65, mean = mean.tiida, sd = sd.tiida, lower.tail = FALSE)
pnorm(70, mean = mean.tiida, sd = sd.tiida, lower.tail = FALSE)

# ///////////////////////////////////////////////////////
# Working with Quartet {car} Four Regression Datasets
# ///////////////////////////////////////////////////////

# CRAN datasets are loaded
data(Quartet)

# View {utils} function is requested
View(Quartet)

# Desc {DescTools} function is requested
Desc(Quartet, plotit = TRUE)

# names function is requested
names(Quartet)

# corrgram {corrgram} function is applied over relevant data.frames
corrgram(Quartet, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Quartet")

# Computing correlation matrix
M2 <- cor(Quartet)
head(round(M2,2))

# Display the correlation coefficient :
corrplot(M2, method="number")

# A ggplot object is created
ggplot(aes(x = x, y = y1), data = Quartet) + 
  geom_point(color = "red", size = 4.5, alpha = 0.90) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Modelo lm, x versus y1") +
  xlab("x") +
  ylab("y1") +
  theme_grey(base_size = 22.0)

# A lm {stats} model is created
lmfit2 = lm(y1 ~ x, data = Quartet)

# visreg {visreg} function is requested
visreg(lmfit2, partial = TRUE)

# effect {effects}
plot(allEffects(lmfit2))

# A ggplot object is created
ggplot(aes(x = x, y = y1), data = Quartet) +
  geom_smooth(size = 0.85,alpha = 0.35,method = lm,fullrange = TRUE) +
  geom_point(color = "red", size = 4.5, alpha = 0.90) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Modelo lm, x versus y1") +
  xlab("x") +
  ylab("y1") +
  theme_grey(base_size = 22.0)

# A summary {base} function is requested 
summary(lmfit2)

# shapiro.test {stats} Normality Test is applied
# if p-value > 0.05 then normality stands true, meaning that
# the variable is parametric
shapiro.test(lmfit2$residuals)

# A full lm-model diagnostic plot is requested
oldpar.lm <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(lmfit2)
par(oldpar.lm)

# ///////////////////////////////////////////////////////
# Fitting a polynomial regression model with lm
# ///////////////////////////////////////////////////////

# Some predictor variables and response variables may have a non-linear 
# relationship, and their relationship can be modeled as an nth order polynomial.

# A ggplot object is created
ggplot(aes(x = x, y = y2), data = Quartet) + 
  geom_point(color = "red", size = 4.5, alpha = 0.90) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Modelo lm, x versus y2") +
  xlab("x") +
  ylab("y2") +
  theme_grey(base_size = 22.0)

# A lm {stats} model is created
lmfit3 = lm(Quartet$y2 ~ poly(Quartet$x,2))

# A summary {base} function is requested 
summary(lmfit3)

# We can illustrate the second order polynomial regression model in formula, y = A + Bx + C(x^2)
# where A is the intercept while B and C, illustrate regression coefficients

# A ggplot object is created
ggplot(aes(x = x, y = y2), data = Quartet) + 
  geom_smooth() +
  geom_point(color = "red", size = 4.5, alpha = 0.90) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  ggtitle("Modelo lm, x versus y2") +
  xlab("x") +
  ylab("y2") +
  theme_grey(base_size = 22)