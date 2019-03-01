### Residuals and the coefficient of determination

### Residuals:  y - y-hat
###             y - XB

data(iris)

head(iris)

### Establish the model
mod = lm(Sepal.Length ~ Petal.Length, data=iris)

### Get the residuals for the model
summary(mod$residuals)

### From this we find r^2 = Coef. of determination = 0.76
### This is the proportion of variance in the model that is 
###'explained' by the model.
###
###

### "Proof" of r-squared = SSR / SST
RSS_1 = sum( mod$residuals^2 ) 

### Deriving residuals (and other variable from first principles)
y = iris$Sepal.Length
x = iris$Petal.Length
beta_hat = mod$coefficients
beta0 = beta_hat[1]
beta1 = beta_hat[2]
yhat = beta0 + beta1*x

resid = y - yhat
RSS_2 = sum(resid^2)


### Getting sum of squares total

SYY = sum( (y - mean(y))^2)

### Deriving r-squared

R2 = 1 - RSS_1 / SYY


df = mod$df.residual   ### n - 2 in this case, for 2 beta values
n = length(mod$residuals)
rsq_adj =  1 - (   RSS_1 / SYY    *  (n-1)/df    )
rsq_adj



### So using this regression, the 'leftover' uncertainty 
### Of the values has been reduced by 76%

### Side note: R^2 is the  (Pearson) correlation coefficient squared
cor(x,y)
cor(x,y)^2




######################### PART 2: Mutliple regression

### Using lm(), predict Sepal.Length as a function of everything else
mod2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)

### Footnote, when building a model in a function like lm() or glm() or glmnet(), 
### y ~ x means "Y as a function of X"
### y ~ x1 + x2 + x3 means "Y as a function of a linear combination of x1, x2 and x3"

### Check data
summary(mod2)


### Goal: Derive beta coefficents from the least-squares formula
### (XTX)^-1 * XTy

X = cbind(1, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
y = iris$Sepal.Length

### solve() means matrix inverse
### t() means transpose
### %*% means matrix multiplication

betas = solve( t(X) %*% X) %*% t(X) %*% y
betas

### Get the fitted values from these betas
yhat  = X %*% betas
yhat[1:5]

### For comparison
mod$fitted.values[1:5]


## Finding the r-squared (coefficient of determination)
resid = y - yhat
RSS = sum(resid^2)

SYY = sum( (y - mean(y))^2)

rsq = 1 - RSS / SYY
rsq

### Find the adjusted r-squared as well
df = mod2$df.residual   ### n - 4 in this case, for 4 beta values
n = length(mod2$residuals)
rsq_adj =  1 - (   RSS / SYY    *  (n-1)/df    )
rsq_adj


######## Part 3:  Using the summary output from lm()

### Establish the model
mod = lm(Sepal.Length ~ Petal.Length, data=iris)

### Get the residuals for the model
summary(mod)


## t-score is estimate / SE(estimate)

4.30660 / 0.07839  ##compare this to t = 54.94

T = 0.40892 / 0.01889
T

### The F-ststatic is T-squared (in simple regression)
T^2 


### Predict new values
x_new = data.frame(6.0) ### 6.0 cm for new petal length
names(x_new) = "Petal.Length"

## Produce the fitted response value for the new explanatory X
## Give the standard error of this fit
## Give the prediction interval of this fitted value too
predict(mod, newdata=x_new, se.fit=TRUE, interval="prediction")


## Compare to the confidence interval, 
## which for the mean of all such new values of that level
predict(mod, newdata=x_new, se.fit=TRUE, interval="confidence")
