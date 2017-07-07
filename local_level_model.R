library("dlm")

data <- read.table("data/Chapter_1/UKdriversKSI.txt", quote="\"", skip=1)
taxi <- ts(log(data), start=c(1969, 1), frequency=12)

##### 確定的ローカルレベルモデル #####

##### モデル手作り #####
buildObj <- function(parm){
    dlm(FF = matrix(c(1), 1, 1),
        V = matrix(exp(parm[1]), 1, 1),
        GG = matrix(c(1), 1, 1),
        W = matrix(0, 1, 1),
        m0 = c(0),
        C0 = matrix(c(1e+07),1,1)
        )
}
fit <- dlmMLE(taxi,
              parm=c(log(var(taxi))),
              buildObj,
              )
obj <- buildObj(fit$par)
filt <- dlmFilter(taxi, obj)

##### dlmModPolyを使う #####
buildObj2 <- function(parm){
    dlmModPoly(order=1, dV=exp(parm[1]), dW=0)
}
fit2 <- dlmMLE(taxi,
               parm=c(log(var(taxi))),
               buildObj2,
              )
obj2 <- buildObj2(fit2$par)
filt2 <- dlmFilter(taxi, obj2)

##### 結果表示 #####
print("手作り")
drop(obj$W)
obj$V
print("MODPOLY")
obj2$W
obj2$V


# プロット
pdf(file = "result_local_level_model.pdf")
par(fin=c(7,5))
plot(taxi, type="l", lwd=2)
lines(dropFirst(filt$m), col=3, lwd=1)
plot(taxi, type="l", lwd=2)
lines(dropFirst(filt2$m), col=2, lwd=1)

##### 確率的ローカルレベルモデル #####

##### モデル手作り #####
buildObj <- function(parm){
    dlm(FF = matrix(c(1), 1, 1),
        V = matrix(exp(parm[1]), 1, 1),
        GG = matrix(c(1), 1, 1),
        W = matrix(exp(c(parm[2])), 1, 1),
        m0 = c(0),
        C0 = matrix(c(1e+07),1,1)
        )
}
fit <- dlmMLE(taxi,
              parm=c(log(var(taxi)), 0.1),
              buildObj,
              )
obj <- buildObj(fit$par)
filt <- dlmFilter(taxi, obj)


##### dlmModPolyを使う #####
buildObj2 <- function(parm){
    dlmModPoly(order=1, dV=exp(parm[1]), dW=exp(parm[2]))
}
fit2 <- dlmMLE(taxi,
               parm=c(log(var(taxi)), 0.1),
               buildObj2,
           )
obj2 <- buildObj2(fit2$par)
filt2 <- dlmFilter(taxi, obj2)


##### 結果表示 #####
print("手作り")
drop(obj$W)
obj$V
print("MODPOLY")
obj2$W
obj2$V


# プロット
par(fin=c(7,5))
plot(taxi, type="l", lwd=2)
lines(dropFirst(filt$m), col=3, lwd=1)
plot(taxi, type="l", lwd=2)
lines(dropFirst(filt2$m), col=2, lwd=1)


##### ノルウェイ #####
data <- read.table("data/Chapter_2/NorwayFinland.txt", quote="\"", skip=1)
norway <- ts(log(data[2]), start=c(1970))

fit_nor <- dlmMLE(norway,
                  parm=c(log(var(norway)), 0.1),
                  buildObj,
                  )
obj_nor <- buildObj(fit_nor$par)
filt_nor <- dlmFilter(norway, obj_nor)

print("norway")
obj_nor$V
obj_nor$W

plot(norway, type="l", lwd=2)
lines(dropFirst(filt_nor$m), col=2, lwd=1)
dev.off()
