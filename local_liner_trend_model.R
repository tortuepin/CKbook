library("dlm")

data <- read.table("data/Chapter_1/UKdriversKSI.txt", quote="\"", skip=1)
taxi <- ts(log(data), start=c(1969, 1), frequency=12)

##### 確定的レベルと確定的傾き
build_sL_sD_Obj <- function(parm){
    dlm(FF = matrix(c(1, 0), 1, 2),
        V = matrix(exp(parm[1]), 1, 1),
        GG = matrix(c(1, 1, 0, 1), 2, 2, byrow=T),
        W = matrix(c( 0, 0, 0, 0), 2, 2, byrow=T),
        m0 = c(0, 0),
        C0 = matrix(c(1e+07, 0, 0, 1e+07), 2, 2)
        )
}
sL_sD_fit <- dlmMLE(taxi,
                    parm=c(log(var(taxi))),
                    build_sL_sD_Obj,
                    )
sL_sD_obj <- build_sL_sD_Obj(sL_sD_fit$par)
sL_sD_filt <- dlmFilter(taxi, sL_sD_obj)

# 結果
print("確定的レベルと確定的傾き")
drop(sL_sD_obj$V)
drop(sL_sD_obj$W)

pdf(file = "result_local_liner_trend_model.pdf")
par(fin=c(7,5))
plot(taxi, type="l", lwd=2)
lines(dropFirst(sL_sD_filt$m[,1]), col=3, lwd=1)

##### 確率的レベルと確率的傾き
build_pL_pD_Obj <- function(parm){
    dlm(FF = matrix(c(1, 0), 1, 2),
        V = matrix(exp(parm[1]), 1, 1),
        GG = matrix(c(1, 1, 0, 1), 2, 2, byrow=T),
        W = matrix(c(exp(parm[2]), 0, 0, exp(parm[3])), 2, 2, byrow=T),
        m0 = c(0, 0),
        C0 = matrix(c(1e+07, 0, 0, 1e+07),2, 2)
        )
}
pL_pD_fit <- dlmMLE(taxi,
              parm=c(log(var(taxi)), 0.1, 0.1),
              build_pL_pD_Obj,
              )
pL_pD_obj <- build_pL_pD_Obj(pL_pD_fit$par)
pL_pD_filt <- dlmFilter(taxi, pL_pD_obj)

# 結果
print("確率的レベルと確率的傾き")
drop(pL_pD_obj$V)
drop(pL_pD_obj$W)

par(fin=c(7,5))
plot(taxi, type="l", lwd=2)
lines(dropFirst(pL_pD_filt$m[,1]), col=3, lwd=1)

##### 確率的レベルと確定的傾き
build_pL_sD_Obj <- function(parm){
    dlm(FF = matrix(c(1, 0), 1, 2),
        V = matrix(exp(parm[1]), 1, 1),
        GG = matrix(c(1, 1, 0, 1), 2, 2, byrow=T),
        W = matrix(c(exp(parm[2]), 0, 0, 0), 2, 2, byrow=T),
        m0 = c(0, 0),
        C0 = matrix(c(1e+07, 0, 0, 1e+07),2, 2)
        )
}
pL_sD_fit <- dlmMLE(taxi,
              parm=c(log(var(taxi)), 0.1),
              build_pL_sD_Obj,
              )
pL_sD_obj <- build_pL_sD_Obj(pL_sD_fit$par)
pL_sD_filt <- dlmFilter(taxi, pL_sD_obj)

# 結果
print("確率的レベルと確定的傾き")
drop(pL_sD_obj$V)
drop(pL_sD_obj$W)

par(fin=c(7,5))
plot(taxi, type="l", lwd=2)
lines(dropFirst(pL_sD_filt$m[,1]), col=3, lwd=1)


##### フィンランド 確率的レベル確率的傾き
data <- read.table("data/Chapter_2/NorwayFinland.txt", quote="\"", skip=1)
finland <- ts(log(data[3]), start=c(1970))

fit_fin <- dlmMLE(finland,
                  parm=c(log(var(finland)), 0.1, 0.1),
                  build_pL_pD_Obj,
                  )
obj_fin <- build_pL_pD_Obj(fit_fin$par)
filt_fin <- dlmFilter(finland, obj_fin)

print("Finland:確率的/確率的")
drop(obj_fin$V)
drop(obj_fin$W)

plot(finland, type="l", lwd=2)
lines(dropFirst(filt_fin$m[,1]), col=2, lwd=1)

##### フィンランド 確定的レベル確率的傾き

build_sL_pD_Obj <- function(parm){
    dlm(FF = matrix(c(1, 0), 1, 2),
        V = matrix(exp(parm[1]), 1, 1),
        GG = matrix(c(1, 1, 0, 1), 2, 2, byrow=T),
        W = matrix(c( 0, 0, 0, exp(parm[2])), 2, 2, byrow=T),
        m0 = c(0, 0),
        C0 = matrix(c(1e+07, 0, 0, 1e+07), 2, 2)
        )
}


fit_fin2 <- dlmMLE(finland,
                  parm=c(log(var(finland)), 0.1),
                  build_sL_pD_Obj,
                  )
obj_fin2 <- build_sL_pD_Obj(fit_fin2$par)
filt_fin2 <- dlmFilter(finland, obj_fin2)

print("Finland:確定的/確率的")
drop(obj_fin2$V)
drop(obj_fin2$W)

plot(finland, type="l", lwd=2)
lines(dropFirst(filt_fin2$m[,1]), col=2, lwd=1)
dev.off()
