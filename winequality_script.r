#load data
winequality.red <- read.csv("~/winequality-red.csv")
View(winequality.red)

#Stepwise with linear terms
null_lm = with(winequality.red, lm(quality~1))
full_lm = with(winequality.red, lm(quality~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol))
step(full_lm,scope=list(lower=null_lm,upper=full_lm))
my_lm = with(winequality.red,lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol))
summary(my_lm)

#Stepwise with second order terms
full_lm = with(winequality.red, lm(quality~poly(fixed.acidity,2) + poly(volatile.acidity,2) + poly(citric.acid,2) + poly(residual.sugar,2) + poly(chlorides,2) + poly(free.sulfur.dioxide,2) + poly(total.sulfur.dioxide,2) + poly(density,2) + poly(pH,2) + poly(sulphates,2) + poly(alcohol,2)))
step(full_lm,scope=list(lower=null_lm,upper=full_lm))
my_lm = with(winequality.red, lm(quality~poly(fixed.acidity,2) + poly(volatile.acidity,2) + poly(chlorides,2) + poly(free.sulfur.dioxide,2) + poly(total.sulfur.dioxide,2) + poly(density,2) + poly(pH,2) + poly(sulphates,2) + poly(alcohol,2)))
summary(my_lm)

#Compares linear and second order model
my_lm2 = with(winequality.red, lm(quality~poly(fixed.acidity,2) + poly(volatile.acidity,2) + poly(chlorides,2) + poly(free.sulfur.dioxide,2) + poly(total.sulfur.dioxide,2) + poly(density,2) + poly(pH,2) + poly(sulphates,2) + poly(alcohol,2)))
my_lm = with(winequality.red,lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol))
anova(my_lm, my_lm2)

#Create binary variable good, do stepwise regresison
good = ifelse(winequality.red$quality >= 7, 1,0)
winecopy = data.frame(winequality.red,good)
null_lm = with(winecopy, glm(good~1, family='binomial'))
full_lm = with(winecopy, glm(good ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,family='binomial'))
step(full_lm,scope=list(lower=null_lm,upper=full_lm))
mylogm = with(winecopy,glm(formula = good ~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + total.sulfur.dioxide + density + sulphates + alcohol, family = "binomial"))
summary(mylogm)

#plotting to look for any trends
with(winequality.red, plot(quality~fixed.acidity))
with(winequality.red, plot(quality~volatile.acidity))
with(winequality.red, plot(quality~citric.acid))
with(winequality.red, plot(quality~residual.sugar))
with(winequality.red, plot(quality~chlorides))
with(winequality.red, plot(quality~free.sulfur.dioxide))
with(winequality.red, plot(quality~total.sulfur.dioxide))
with(winequality.red, plot(quality~density))
with(winequality.red, plot(quality~pH))
with(winequality.red, plot(quality~sulphates))
with(winequality.red, plot(quality~alcohol))

#collinearity
cor(winequality.red[1:11])

#binary dependent variable, logistic model
with(winequality.red, plot(good~fixed.acidity))
with(winequality.red, plot(good~volatile.acidity))
with(winequality.red, plot(good~citric.acid))
with(winequality.red, plot(good~residual.sugar))
with(winequality.red, plot(good~chlorides))
with(winequality.red, plot(good~free.sulfur.dioxide))
with(winequality.red, plot(good~total.sulfur.dioxide))
with(winequality.red, plot(good~density))
with(winequality.red, plot(good~pH))
with(winequality.red, plot(good~alcohol))