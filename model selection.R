install.packages('datasets')
library(datasets)
data(mtcars)
mtcars


#forward selection
n = dim(mtcars)[1]
f <- ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb
m0 <- lm(mpg ~ 1, mtcars)
#(a)
m.forward <- step(m0, direction="forward",scope = f,k=2)
#(b)
m.forward <- step(m0, direction="forward",scope = f,k=log(n))

#backward selection
m1 <- update(m0, f)
#(c)
m.backward <- step(m1, scope =  c(lower = ~ 1), direction="backward", k=2)
#(d)
m.backward <- step(m1, scope =  c(lower = ~ 1), direction="backward", k=log(n))
