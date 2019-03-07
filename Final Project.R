######cleaning data
library(feather)
players = "C:/Users/jaych/Desktop/Final.feather"
player = read_feather(players)
player <- player[, -c(1:18,28:32,77:92,94:184)]
player <- player[complete.cases(player$rs),]
RS <- player[, -c(1,2,6,10:43, 45:54)]
RS = subset(RS, RS$rs > 75)
names(RS) = c("PAC","SHO","PAS","DRI","PHY","IR","rs")
########
pairs(~ ., data = RS)
library(corrplot)
M = cor(RS)
corrplot.mixed(M, upper = "color", lower.col = "black")

#####full model
attach(RS)
M0 = lm(rs~IR+PAC+SHO+DRI+PAS+PHY+PAC*SHO+PAC*PAS+PAC*DRI+PAC*PHY+PAC*IR+SHO*PAS+SHO*DRI+SHO*PHY+SHO*IR+PAS*DRI+PAS*PHY+DRI*PHY+DRI*IR+PHY*IR, data = RS)
summary(M0)
plot(M0)

#####Model Selection
M1 = lm(rs ~ 1, data = RS)
n = dim(RS)[1]
f <- ~ IR+PAC+SHO+DRI+PAS+PHY+PAC*SHO+PAC*PAS+PAC*DRI+PAC*PHY+PAC*IR+SHO*PAS+SHO*DRI+SHO*PHY+SHO*IR+PAS*DRI+PAS*PHY+DRI*PHY+DRI*IR+PHY*IR
#AIC forward
RS.AIC.forward <- step(M1, direction = "forward", scope = list(lower=~1, upper=f))
#Step:  AIC=145.7
#rs ~ SHO + IR + PHY + PAC + SHO:IR + SHO:PAC + IR:PHY + SHO:PHY + IR:PAC

#AIC backward
RS.AIC.backward <- step(M0,scope=list(lower=~1, upper=f), direction="backward")
#Step:  AIC=136.91
#rs ~ IR + PAC + SHO + DRI + PAS + PHY + PAC:SHO + SHO:DRI + SHO:PHY + IR:SHO + PAS:PHY

#AIC both
RS.AIC.both <- step(M0,scope=list(lower=~1, upper=f), direction="both",data=RS)
#Step:  AIC=136.91
#rs ~ IR + PAC + SHO + DRI + PAS + PHY + PAC:SHO + SHO:DRI + SHO:PHY + IR:SHO + PAS:PHY

#BIC forward
RS.BIC.forward <- step(M1, direction="forward",scope = list(lower=~1, upper=f),k=log(n))
#Step:  AIC=176.21
#rs ~ SHO + IR + PHY + PAC + SHO:IR + SHO:PAC + IR:PHY

#BIC backward
RS.BIC.backward <- step(M0,scope=list(lower=~1, upper=f), direction="backward", k=log(n))
#Step:  AIC=174.61
#rs ~ IR + PAC + SHO + DRI + PHY + PAC:SHO + SHO:DRI + SHO:PHY + IR:SHO

#BIC both
RS.BIC.both = step(M0, scope=list(lower=~1, upper=f), direction="both", k=log(n))
#Step:  AIC=174.61
#rs ~ IR + PAC + SHO + DRI + PHY + PAC:SHO + SHO:DRI + SHO:PHY + IR:SHO

M.AIC = lm(rs ~ IR + PAC + SHO + DRI + PAS + PHY + PAC:SHO + SHO*DRI + SHO*PHY + IR*SHO + PAS*PHY, data = RS)
M.BIC = lm(rs ~ IR + PAC + SHO + DRI + PHY + PAC*SHO + SHO*DRI + SHO*PHY + IR*SHO)

plot(M0)
hist(M0$residuals)
qqnorm(M0$residuals)
qqline(M0$residuals)

summary(M.BIC)
plot(M.AIC)

summary(M0)