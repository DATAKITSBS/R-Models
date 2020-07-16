library(lpSolve)

## Objective function: To maximize: -2000x1 + 5000x2; subject to constraints: 100 ≤ x1 ≤ 200, 80 ≤ x2 ≤ 170, x1 + x2 = 200
# where x1 and x2 are any non-negative variables.

obj <- c(-2000,5000)
con1 <- matrix(c(1,0,1,0,0,1,0,1,1,1), nrow = 5, byrow = TRUE)
dir <- c("<=",">=",">=","<=",">=")
rhs <- c(200,100,80,170,200)
coeff <- rbind(c(1,0),c(0,1))
const1 <- c(200,170)
const2 <- c(100,80)


lp("max",objective.in = obj,const.mat = con1,const.dir = dir,const.rhs = rhs)

lp("max",objective.in = obj,const.mat = con1,const.dir = dir,const.rhs = rhs)$solution

#Plotting the feasible region
library(intpoint)
solve2dlp(t=1, c = obj, bm = const1, bM = const2, m = coeff, M=coeff)