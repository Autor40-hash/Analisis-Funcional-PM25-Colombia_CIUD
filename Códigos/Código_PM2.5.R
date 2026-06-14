
## Library -----------------------

library(mvtnorm)
library(pcaPP)
library(fda.usc)
library(fda)
library(BSDA)
library(fdANOVA)
library(car)
library(dplyr)
library(devtools)
library(ggpubr)
#library(normtest) 
#library(moments) 
#library(fanovaGraph) ## Función-
#install.packages("fanovaGraph")
library(GET)
library(ggplot2)
library(readxl)
library(naniar)
library(fs)
library(tidyverse)
library(imputeTS)
library(lubridate)
library(mice)
library(missForest)
library(Hmisc)
library(VIM)
library(zoo)
library(reshape2)



## Datos
datos <- read_excel("PARTICULAS PM25 BOGOTA MEDELLIN 2023.xlsx", sheet = "PM2.5")
datos1 <- as.data.frame(datos[, -c(1, 17, 25)])

## NAs
as.data.frame(miss_var_summary(datos1))

## concentraciones de PM25
pm25_columns <- datos1[, 1:20]
pm25_columns <- pm25_columns[, -c(12, 14, 8, 10)] #W Eliminamos algunas columnas

## 
names(pm25_columns) <- make.names(names(pm25_columns))

## Imputación usando MICE (Multivariate Imputation by Chained Equations)
bog_MC <- mice(pm25_columns[, 1:8], m = 5, method = 'pmm', maxit = 50, seed = 500)
med_MC <- mice(pm25_columns[, 9:11], m = 5, method = 'pmm', maxit = 50, seed = 500)
cal_MC <- mice(pm25_columns[, 12:16], m = 5, method = 'pmm', maxit = 50, seed = 500)

bog2 <- complete(bog_MC)
med2 <- complete(med_MC)
cal2 <- complete(cal_MC)

head(bog2)

### ---------------------

datos2 <- as.data.frame(cbind(bog2, med2, cal2))
datos2 <- datos2[nrow(datos2):1, ]
summary(datos3)

matplot(datos2, type = "l", main = "", col = 1)
matplot(datos2[, 1:8], type = "l", col = 1)
matplot(datos2[, 9:11], type = "l", col = 1)
matplot(datos2[, 12:16], type = "l", col = 1)


###--------------------------------------
# Suavizado 
###-------------------------------------

y <- as.matrix(datos2)
t = 1:nrow(datos)

Ks <- seq(10, 80, by = 5)
lambdas <- 10^seq(-6, 6, length.out = 25)

## -----------------------------------
gcv_m <- matrix(
  NA,
  nrow = length(Ks),
  ncol = length(lambdas)
)

rownames(gcv_m) <- paste0("K_", Ks)
colnames(gcv_m) <- round(lambdas, 6)


## Buscar
for(i in 1:length(Ks)){
  bases_i <- create.bspline.basis(
    rangeval = range(t),
    nbasis = Ks[i],
    norder = 4
  )
  for(j in 1:length(lambdas)){
    fdParobj <- fdPar(
      fdobj = bases_i,
      Lfdobj = 2,
      lambda = lambdas[j]
    )
    ajuste <- smooth.basis(
      argvals = t,
      y = y,
      fdParobj = fdParobj
    )
    gcv_m[i, j] <- sum(as.numeric(ajuste$gcv))
  }
}


### Menor GCV ----------------------
min_gcv <- min(gcv_m)
pos <- which(gcv_m == min_gcv, arr.ind = TRUE)

m_K <- Ks[pos[1]]
m_lambda <- lambdas[pos[2]]

m_K
m_lambda
min_gcv

## - ---------------------------------------------
## Modelo ----------------------------------------

base_f <- create.bspline.basis(rangeval = range(t),nbasis = m_K,norder = 4)
fdPar_f <- fdPar(fdobj = base_f,Lfdobj = 2,lambda = m_lambda)
curvas <- smooth.basis(argvals = t, y = y,fdParobj = fdPar_f)
curvasfd <- curvas$fd


plot(curvasfd, col = 1)
matplot(datos2, type = "l")

#---------------------------
# Zonas 
#-----------------------------

# Bogotá
# Medellín
# Cali
names(datos2)
plot(curvasfd[12])
curvasfd <- curvasfd[-12] ## _Eliminamos

bogota <- curvasfd[1:8]
medellin <- curvasfd[9:11]
cali <- curvasfd[12:15]


## Suavizado comparación con datos
matplot(bog2[nrow(bog2):1,][1], type = "l")
lines(bogota[1], col = "red")

##

par(mfrow = c(1, 3))
#plot(curvas, col = 1, main = "", xlab = "Día", ylab = "PM2.5 (µg/m³)")
plot(bogota, col = 9, main = "Bogotá", xlab = "Día", ylab = "PM2.5 (µg/m³)")
plot(medellin, col = 9, main = "Medellín", xlab = "Día", ylab = "PM2.5 (µg/m³)")
plot(cali, col = 9, main = "Cali", xlab = "Día", ylab = "PM2.5 (µg/m³)")

par(mfrow = c(1, 1))

##- -----------------------------------------------------
## Curvas medias y SD ----------------------------------------
## -----------------------------------------------------

par(mfrow = c(1, 2))
plot(curvasfd, lty = 1, col = rgb(0.5, 0.5, 0.5, 0.3), ylim = c(0, 80), main = "Curvas Media PM2.5", xlab = "Día", ylab = "PM2.5 (µg/m³)")
lines(mean.fd(bogota), lty = 1, lwd = 2, col = 2, ylim = c(5, 65), main = "Curvas Media PM2.5", xlab = "Día", ylab = "PM2.5 (µg/m³)")
lines(mean.fd(medellin), lty = 1, lwd = 2, col = 3)
lines(mean.fd(cali), lty = 1, lwd = 2, col = 4); grid()
lines(medellin[2], lty = 1, lwd = 2, col = 9) ## Curva más extrema hubicada en Medellín

legend("topright", legend = c("Bogotá", "Medellín", "Cali"), col = c(2, 3, 4), lty = 1, lwd = 2, cex = 0.7)#0.5

## Desviación Estándar
plot(std.fd(bogota), lty = 1, lwd = 2, col = 2, ylim = c(0, 25), main = "Desviación Estándar PM2.5", xlab = "Día", ylab = "Desviación (µg/m³)")
lines(std.fd(medellin), lty = 1, lwd = 2, col = 3)
lines(std.fd(cali), lty = 1, lwd = 2, col = 4); grid()
legend("topright", legend = c("Bogotá", "Medellín", "Cali"), col = c(2, 3, 4), lty = 1, lwd = 2,cex = 0.5)
par(mfrow = c(1, 1))



## -------------------------------------------------------
## ANOVA para datos funcionales:
## Datos partículas suspendidas de PM2.5
## -------------------------------------------------------
ncol(cal1)

fdG1 = rep('Bogotá', 8)
fdG2 = rep ('Medellín', 3)
fdG3 = rep('Cali', 4)
zona <- as.factor(c(fdG1,fdG2,fdG3))

## fdata
fdata <- fdata(curvasfd)

set.seed(123)
result = fanova.onefactor(fdata, zona, nboot=50, plot=T)
quantile(result$wm, probs = 0.95)

##------------------------------------
## Library(fdANOVA)
## FANOVA TEST
## -----------------------------------

set.seed(123)
(fanova <- fanova.tests(x = datos2[,-12], group.label = zona))
fanova

#-------------------------------------------
# ANOVA funcional: graph.fanova_GET
# Rank envelope test 
#-------------------------------------------



set.seed(232)
resultC <- graph.fanova(
  nsim = 2999,
  curve_set = fdata,
  groups = zona,
  variances = "unequal",
  contrasts = TRUE, 
  typeone = "fdr"
)


plot(resultC) + 
  ggplot2::labs(x = "Día")+theme_minimal()


#--------------------------------
# Boxplot funcional
#--------------------------------


## Bogota --------------------------------------------------
com1 <- fbplot(datos2[,c(1:8)], method = "MBD", ylim = c(0, 90), plot = TRUE)
atipicas <- c(2, 6)

col_normales <- rgb(0.65, 0.65, 0.65, 0.35)
col_colina <- "#377EB8"
col_mochuelo <- "#E41A1C"

### Gráfico base
plot( bogota[-atipicas],lty = 1,ylim = c(0, 90),col = col_normales,
  lwd = 1,main = "Bogotá",xlab = "Día", ylab = expression(PM2.5~(mu*g/m^3)),
  bty = "l",cex.lab = 1.2,cex.axis = 1.1)

## Atípicas
lines(bogota[2], col = col_colina, lwd = 3)
lines(bogota[6], col = col_mochuelo, lwd = 3)
grid(col = "grey90",lty = 1,lwd = 0.8)
legend("topright",legend = c("Curvas observadas","Colina","Rural Mochuelo"
  ),col = c(col_normales, col_colina, col_mochuelo),lty = 1,
  lwd = c(1.5, 3, 3),cex = 1.4, bg = "white",box.lty = 0
)




## Medellín ---------------------------------------------------

com2 <- fbplot(datos3[,c(9:11)], method = "MBD", ylim = c(0, 90), plot = TRUE)

atipica_med <- 3
col_normales <- rgb(0.65, 0.65, 0.65, 0.35)
col_atipica <- "#E41A1C"

## Gráfico base
plot( medellin[-atipica_med],lty = 1,ylim = c(0, 90), col = col_normales,
  lwd = 1,main = "Medellín",xlab = "Día", ylab = expression(PM2.5~(mu*g/m^3)),
  bty = "l",cex.lab = 1.2,cex.axis = 1.1 )
lines(medellin[atipica_med],col = col_atipica,lwd = 3)

grid(col = "grey90",lty = 1,lwd = 0.8)
legend( "topright",legend = c("Curvas observadas","Politécnico Jaime Isaza"
  ),col = c(col_normales, col_atipica),lty = 1,lwd = c(1.5, 3),cex = 1.4,
  bg = "white", box.lty = 0)


## Cali ------------------------------------------------

com3 <- fbplot(datos2[,c(13:16)], method = "MBD", ylim = c(0, 90), plot = TRUE)

atipicas_cali <- c(1, 4)
col_normales <- rgb(0.65, 0.65, 0.65, 0.35)
col_laflora <- "#377EB8"
col_pance <- "#E41A1C"

## Gráfico base
plot(cali[-atipicas_cali],lty = 1,ylim = c(0, 90),col = col_normales,
  lwd = 1,main = "Cali",xlab = "Día",ylab = expression(PM2.5 ~(mu*g/m^3)),
  bty = "l",cex.lab = 1.2,cex.axis = 1.1)

lines(cali[1],col = col_laflora,lwd = 3)
lines(cali[4],col = col_pance,lwd = 3)
grid(col = "grey95", lty = 1,lwd = 0.8)
legend( "topright",legend = c("Curvas observadas","La Flora", "Pance"
  ),col = c(col_normales,col_laflora,col_pance),lty = 1,
  lwd = c(1.5, 3, 3), cex = 1.4,bg = "white", box.lty = 0)




