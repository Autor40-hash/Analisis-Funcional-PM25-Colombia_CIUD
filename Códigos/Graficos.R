### -------------------------------------
## Graficos 
## ---------------------------------------

## --------------------------------------
## Fígura 1 - Zona de estudio
## ------------------------------------_-

## --------------------------------------
## Fígura 2 - Curvas completas 
## ------------------------------------_-

# png("Curvas_Compl_Figura_2.png",width = 13, height = 6, units = "in", res = 600 )
# pdf("Curvas_Compl_Figura_2.pdf", width = 13,height = 6)


par(mfrow = c(1, 3),cex.main = 2,cex.lab = 1.5,cex.axis = 1.4)
## Bogotá ----------------------
plot(bogota, col = 9, main = "Bogotá", ylim = c(0,80), xlab = "Día", ylab = "PM2.5 (µg/m³)")

## Medellín -------------------
plot(medellin, col = 9, main = "Medellín", ylim = c(0,80),  xlab = "Día", ylab = "PM2.5 (µg/m³)")

## Cali ------------------------
plot(cali, col = 9, main = "Cali", ylim = c(0,80),  xlab = "Día", ylab = "PM2.5 (µg/m³)")

par(mfrow = c(1, 1))


# dev.off()


### -----------------------------------------------------
##  Figura 3.
## ------------------------------------------------------


# pdf("Curvas_Descrip_Figura_3.pdf",width = 12,height = 6)
# png("Curvas_Descrip_Figura_3.png",width = 12,height = 6,units = "in",res = 600)


par(mfrow = c(1, 2), mar = c(4.5, 4.5, 2, 1))

### --- Panel (a): Curvas funcionales y medias ---
plot(curvasfd, lty = 1, col = rgb(0.5, 0.5, 0.5, 0.3), ylim = c(0, 120),main = "",xlab = "Día", ylab = expression("PM2.5"~(µg/m^3)))
lines(mean.fd(bogota), lty = 1, lwd = 2.5, col = "#E41A1C")
lines(mean.fd(medellin), lty = 1, lwd = 2.5, col = "#377EB8")
lines(mean.fd(cali), lty = 1, lwd = 2.5, col = "#4DAF4A")
lines(medellin[2], lty = 2, lwd = 2.5, col = "#984EA3")

abline(h = 15, col = "black", lty = 3, lwd = 1.5)    ## Ideal diario
abline(h = 75, col = "darkred", lty = 3, lwd = 1.5)  ## Meta intermedia alta

legend("topright",legend = c("Curvas individuales", 
                  "Media Bogotá", 
                  "Media Medellín", 
                  "Media Cali", 
                  "Altavista: Medellín",
                  "Ideal diario (15 µg/m³)",
                  "Meta intermedia (75 µg/m³)"),
       col = c(rgb(0.5, 0.5, 0.5, 0.3), "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
               "black", "darkred"),lty = c(1, 1, 1, 1, 2, 3, 3),lwd = c(1, 2.5, 2.5, 2.5, 2.5, 1.5, 1.5),
       cex = 0.75, bg = "white", box.lty = 0)

mtext("(a)", side = 1, line = 3.5, adj = 0.02, cex = 1.5)

### --- Panel (b): Desviación estándar ---
plot(std.fd(bogota), lty = 1, lwd = 2.5, col = "#E41A1C", ylim = c(0, 25),main = "",xlab = "Día", ylab = "Desviación estándar (µg/m³)")
lines(std.fd(medellin), lty = 1, lwd = 2.5, col = "#377EB8")
lines(std.fd(cali), lty = 1, lwd = 2.5, col = "#4DAF4A")

grid()
legend("topright",legend = c("Bogotá", "Medellín", "Cali"),
       col = c("#E41A1C", "#377EB8", "#4DAF4A"),lty = 1,lwd = 2.5,cex = 0.75, bg = "white", box.lty = 0)

mtext("(b)", side = 1, line = 3.5, adj = 0.02, cex = 1.5)

par(mfrow = c(1, 2))

# dev.off()




### -----------------------------------------------------
##  Figura 4. - Grafico F-anova
## ------------------------------------------------------

set.seed(123)
result = fanova.onefactor(fdata, zona, nboot = 50, plot = TRUE)
# quantile(result$wm, probs = 0.95)

# pdf("Fanova_zonas_Figura_4.pdf",width = 10,height = 5)
# png("Fanova_zonas_Figura_4.png",width = 10,height = 5,units = "in",res = 600)

par(mfrow = c(1,3),mar = c(5,5,5,2),oma = c(0,0,2,0),cex.main = 1.2,
  cex.lab = 1.1,cex.axis = 1)
set.seed(123)
result2 = fanova.onefactor(fdata,zona,nboot = 50,plot = TRUE)

# dev.off()


### -----------------------------------------------------
##  Figura 5. - Comparaciones multiples 
## ------------------------------------------------------


# pdf("Comparaciones_M_Figura_5.pdf",width = 11,height = 4.5)
# png("Comparaciones_M_Figura_5.png", width = 11,height = 4.5,units = "in",res = 600)

set.seed(232)

resultC <- graph.fanova(nsim = 2999,curve_set = fdata,groups = zona,variances = "unequal",contrasts = TRUE, typeone = "fdr")
plot(resultC) + ggplot2::labs(x = "Día")+theme_minimal()

# dev.off()



### -----------------------------------------------------
##  Figura 5. - Curvas atípicas
## ------------------------------------------------------

# png("Curvas_Atípicas_Figura_6.png",width = 14,height = 6,units = "in",res = 600)
# pdf("Curvas_Atípicas_Figura_6.pdf",width = 14,height = 6)

par(mfrow = c(1, 3),mar = c(5, 6, 4, 2), cex.main = 2,cex.lab = 1.5,cex.axis = 1.4)

## Bogotá  --------------------------------------

com1 <- fbplot(datos2[,c(1:8)], method = "MBD", ylim = c(0, 90), plot = TRUE)
colnames(datos2[,c(1:8)])
colnames(bog2[atipicas],)

atipicas <- c(2, 6)
col_normales <- rgb(0.65, 0.65, 0.65, 0.35)
col_colina <- "#377EB8"
col_mochuelo <- "#E41A1C"

plot(bogota[-atipicas], lty = 1,ylim = c(0, 90),  col = col_normales,lwd = 1,
  main = "Bogotá",xlab = "Día",ylab = expression(PM2.5~(mu*g/m^3)),bty = "l",
  cex.lab = 1.2,cex.axis = 1.1)
lines(bogota[2], col = col_colina, lwd = 3)
lines(bogota[6], col = col_mochuelo, lwd = 3)

grid(col = "grey90",lty = 1,lwd = 0.8)
legend("topright",legend = c("Curvas observadas","Colinas","Rural Mochuelo"  ),
  col = c(col_normales, col_colina, col_mochuelo),lty = 1,
  lwd = c(1.5, 3, 3),cex = 1.4, bg = "white",box.lty = 0)


## --------------------------------------------------------
## Medellín  ----------------------------------------

com2 <- fbplot(datos3[,c(9:11)], method = "MBD", ylim = c(0, 90), plot = TRUE)
atipica_med <- 3
col_normales <- rgb(0.65, 0.65, 0.65, 0.35)
col_atipica <- "#E41A1C"

plot(medellin[-atipica_med],lty = 1,ylim = c(0, 90),col = col_normales,
  lwd = 1,main = "Medellín",xlab = "Día",ylab = expression(PM2.5~(mu*g/m^3)),
  bty = "l",cex.lab = 1.2,cex.axis = 1.1)

lines(medellin[atipica_med],col = col_atipica,lwd = 3)
grid(col = "grey90",lty = 1,lwd = 0.8)
legend("topright",legend = c("Curvas observadas","Politécnico Jaime Isaza"),
  col = c(col_normales, col_atipica),lty = 1,lwd = c(1.5, 3),cex = 1.4,
  bg = "white",box.lty = 0)

### --------------------------------------------------------
### Cali  -----------------------------------------

com3 <- fbplot(datos2[,c(13:16)], method = "MBD", ylim = c(0, 90), plot = TRUE)
atipicas_cali <- c(1, 4)
col_normales <- rgb(0.65, 0.65, 0.65, 0.35)
col_laflora <- "#377EB8"
col_pance <- "#E41A1C"

plot(cali[-atipicas_cali],
  lty = 1,ylim = c(0, 90),col = col_normales,
  lwd = 1,main = "Cali",xlab = "Día",ylab = expression(PM2.5 ~(mu*g/m^3)),
  bty = "l",cex.lab = 1.2,cex.axis = 1.1)


lines(cali[1],col = col_laflora,lwd = 3)
lines(cali[4],col = col_pance,lwd = 3)
grid(col = "grey95",lty = 1,lwd = 0.8)
legend("topright",legend = c("Curvas observadas","La Flora","Pance"),
  col = c(col_normales,col_laflora,col_pance),lty = 1,
  lwd = c(1.5, 3, 3),cex = 1.4,bg = "white",box.lty = 0)


par(mfrow = c(1, 1))

# dev.off()



## BOXPLOTS
## Bogota
com1 <- fbplot(datos2[,c(1:8)], method = "MBD", ylim = c(0, 90), plot = TRUE)

## Medellín
com2 <- fbplot(datos3[,c(9:11)], method = "MBD", ylim = c(0, 90), plot = TRUE)

## Cali
com3 <- fbplot(datos2[,c(13:16)], method = "MBD", ylim = c(0, 90), plot = TRUE)

