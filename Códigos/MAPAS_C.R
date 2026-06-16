
##=========================================
## CALI MAPAS 
##=========================================


library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(grid)



## Descomprimir el archivo en una carpeta llamada "medellin_shp"
unzip("mc_comunas.zip", exdir = "cali_shp")
list.files("cali_shp", pattern = "\\.shp$")
comunas_cali <- st_read("cali_shp/mc_comunas.shp")

names(comunas_cali)
plot(st_geometry(comunas_cali), border = "black", main = "Comunas de Cali")


## Filtrar para excluir aquellos nombres que contienen "Corregimiento"
comunasc <- comunas_cali %>%
  filter(!grepl("comuna", nombre))

## Visualizar la lista de comunas filtradas
comunasc

ggplot(comunasc) +
  geom_sf(fill = "lightblue", color = "darkblue", size = 0.2) +
  labs(title = "Comunas de Medellín",
       caption = "Fuente: Datos Abiertos de Medellín") +
  theme_minimal()



### ------------------------

cal2 <- complete(cal_MC)


cal22 <- cal2 %>%
  mutate(Dia = 1:n(),
         Mes = case_when(
           Dia <= 31 ~ "Enero",
           Dia <= 59 ~ "Febrero",
           Dia <= 90 ~ "Marzo",
           Dia <= 120 ~ "Abril",
           Dia <= 151 ~ "Mayo",
           Dia <= 181 ~ "Junio",
           Dia <= 212 ~ "Julio",
           Dia <= 243 ~ "Agosto",
           Dia <= 273 ~ "Septiembre",
           Dia <= 304 ~ "Octubre",
           Dia <= 334 ~ "Noviembre",
           TRUE ~ "Diciembre"
         ))


promedios_mensualesC <- cal22 %>%
  group_by(Mes) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
calpromedios <- as.data.frame(promedios_mensualesC)

## ---------------------------------------------------


maps <- read_excel("PARTICULAS PM25 BOGOTA MEDELLIN 20231.xlsx",sheet = 3)
cali_est <- maps %>%filter(Municipio == "Cali")
cali_sf <- st_as_sf(cali_est,coords = c("Longitud", "Latitud"),crs = 4326)
cali_sf <- st_transform(cali_sf,st_crs(comunas_cali))


## _--------------------------------------------------------
lista_buf1 <- list()
lista_buf2 <- list()
lista_buf3 <- list()
lista_buf4 <- list()
lista_pts  <- list()

for(i in 1:nrow(calpromedios)){
  
  tmp <- cali_sf
  
  tmp$mes <- calpromedios$Mes[i]
  
  tmp$PM25 <- c(
    calpromedios$LA_.FLORA_cali[i],
    calpromedios$CASCAJAL_Cali[i],
    calpromedios$UNIVALLE_Cali[i],
    calpromedios$PANCE_Cali[i]
  )
  
  b1 <- st_buffer(tmp, 100)
  b2 <- st_buffer(tmp, 200)
  b3 <- st_buffer(tmp, 500)
  b4 <- st_buffer(tmp, 900)
  
  b1$PM25 <- tmp$PM25
  b2$PM25 <- tmp$PM25
  b3$PM25 <- tmp$PM25
  b4$PM25 <- tmp$PM25
  
  b1$mes <- tmp$mes
  b2$mes <- tmp$mes
  b3$mes <- tmp$mes
  b4$mes <- tmp$mes
  
  lista_buf1[[i]] <- b1
  lista_buf2[[i]] <- b2
  lista_buf3[[i]] <- b3
  lista_buf4[[i]] <- b4
  lista_pts[[i]]  <- tmp
}

buf1_long <- do.call(rbind, lista_buf1)
buf2_long <- do.call(rbind, lista_buf2)
buf3_long <- do.call(rbind, lista_buf3)
buf4_long <- do.call(rbind, lista_buf4)

cali_pts_long <- do.call(rbind, lista_pts)


orden_meses <- c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto",
  "Septiembre", "Octubre", "Noviembre", "Diciembre")

cali_pts_long <- cali_pts_long |> dplyr::mutate(mes = factor(trimws(mes), levels = orden_meses))

buf1_long <- buf1_long |>
  dplyr::mutate(mes = factor(trimws(mes), levels = orden_meses))
buf2_long <- buf2_long |>
  dplyr::mutate(mes = factor(trimws(mes), levels = orden_meses))
buf3_long <- buf3_long |>
  dplyr::mutate(mes = factor(trimws(mes), levels = orden_meses))
buf4_long <- buf4_long |>
  dplyr::mutate(mes = factor(trimws(mes), levels = orden_meses))




### ----  MAPA CALI --------------------##


coords_cali_pm25 <- cbind(
  st_coordinates(cali_pts_long),
  st_drop_geometry(cali_pts_long)
)

p_cali <- ggplot() +
  geom_sf(data = buf4_long, aes(fill = PM25), alpha = 0.70,
    color = "black",linewidth = 0.2) +
  geom_sf(data = buf3_long, aes(fill = PM25), alpha = 0.40,color = NA) +
  geom_sf(data = buf2_long, aes(fill = PM25), alpha = 0.80,color = NA) +
  geom_sf(data = comunasc,fill = NA,color = "black",linewidth = 0.2) +
  geom_sf(data = buf4_long, aes(fill = PM25), alpha = 0.80,color = NA) +
  geom_sf(data = buf3_long, aes(fill = PM25), alpha = 0.80, color = NA) +
  geom_sf(data = buf2_long,aes(fill = PM25),alpha = 0.80, color = NA) +
  geom_point( data = coords_cali_pm25,aes(X, Y, fill = PM25),shape = 21,
    size = 15,color = "black",stroke = 0.2, alpha = 0.9 ) +
  geom_text(data = coords_cali_pm25,aes(X, Y, label = round(PM25)),
    size = 7, fontface = "plain", color = "black" ) +
  
  facet_wrap(~mes, ncol = 6) +
  scale_fill_gradientn(
    colours = c(
      "#008000",  ## ≤10
      "#7FBF00",  ## 10-20
      "#D9EF00",  ## 20-30
      "#FFE000",  ## 30-40
      "#FF8C00",  ## 40-70
      "#FF2A00"   ## ≥70
    ),
    values = scales::rescale(c(0, 10, 20, 30, 40, 70)),limits = c(0, 70),
    oob = scales::squish,name = expression(PM2.5~"("*mu*"g/"*m^3*")")) +
  coord_sf(datum = NA) +
  theme_minimal() +
  
  theme(panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white",
      color = "black",linewidth = 0.5),
    
    strip.text = element_text(face = "bold",color = "black",size = 20),
    panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey90"),
    
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    panel.spacing = unit(0.3, "lines"),
    legend.position = "right",
    
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, "cm")
  )

p_cali



## Cali
ggsave("Mapa_Cali_Figura_9.pdf",plot = p_cali,width = 20, height = 12,
  units = "in")

ggsave("Mapa_Cali_Figura_9.png",plot = p_cali,width = 20,
  height = 12,units = "in", dpi = 600)




##=========================================
## BOGOTÁ MAPAS 
##=========================================


unzip("loca.zip", exdir = "loca")
loca_sf <- st_read("loca/loca.shp")
plot(loca_sf["LocNombre"])





bog2 <- complete(bog_MC)
bog22 <- bog2 %>%
  mutate(Dia = 1:n(),
         Mes = case_when(
           Dia <= 31 ~ "Enero",
           Dia <= 59 ~ "Febrero",
           Dia <= 90 ~ "Marzo",
           Dia <= 120 ~ "Abril",
           Dia <= 151 ~ "Mayo",
           Dia <= 181 ~ "Junio",
           Dia <= 212 ~ "Julio",
           Dia <= 243 ~ "Agosto",
           Dia <= 273 ~ "Septiembre",
           Dia <= 304 ~ "Octubre",
           Dia <= 334 ~ "Noviembre",
           TRUE ~ "Diciembre"
         ))

promedios_mensuales <- bog22 %>% group_by(Mes) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
bogpromedios <- as.data.frame(promedios_mensuales)


bog_est <- maps %>% filter(Municipio == "Bogotá")
bog_sf <- st_as_sf( bog_est,coords = c("Longitud", "Latitud"),crs = 4326)

bog_sf <- st_transform(bog_sf, 3116)
loca_sf <- loca_sf %>%filter(LocNombre != "SUMAPAZ")
loca_sf <- st_transform(loca_sf, 3116)


## -----------------------------------------------------------
estaciones <- c("CIUDAD.BOLÍVAR_BOG_PM25","EL_JAZMÍN_BOG_PM25",
  "BOLIVIA_BOG_PM25", "USME_BOG_PM25","COLINA_BOG_PM25",
  "LAS_FERIAS_BOG_PM25","RURAL_MOCHUELO_BOG_PM25","ALTO_RENDIMIENTO_BOG_PM25")


lista_buf1 <- list()
lista_buf2 <- list()
lista_buf3 <- list()
lista_buf4 <- list()
lista_pts  <- list()

for(i in 1:nrow(bogpromedios)){
  tmp <- bog_sf
  tmp$Mes <- bogpromedios$Mes[i]
  tmp$PM25 <- as.numeric(
    bogpromedios[i, estaciones]
  )
  b1 <- st_buffer(tmp, 400)
  b2 <- st_buffer(tmp, 1000)
  b3 <- st_buffer(tmp, 1600)
  b4 <- st_buffer(tmp, 2800)
  b1$Mes <- tmp$Mes
  b2$Mes <- tmp$Mes
  b3$Mes <- tmp$Mes
  b4$Mes <- tmp$Mes
  b1$PM25 <- tmp$PM25
  b2$PM25 <- tmp$PM25
  b3$PM25 <- tmp$PM25
  b4$PM25 <- tmp$PM25
  lista_buf1[[i]] <- b1
  lista_buf2[[i]] <- b2
  lista_buf3[[i]] <- b3
  lista_buf4[[i]] <- b4
  lista_pts[[i]]  <- tmp
}

buf1_long <- do.call(rbind, lista_buf1)
buf2_long <- do.call(rbind, lista_buf2)
buf3_long <- do.call(rbind, lista_buf3)
buf4_long <- do.call(rbind, lista_buf4)
pts_long  <- do.call(rbind, lista_pts)

orden_meses <- c("Enero","Febrero","Marzo","Abril", "Mayo","Junio","Julio","Agosto",
  "Septiembre","Octubre","Noviembre","Diciembre")

pts_long$Mes  <- factor(pts_long$Mes, levels = orden_meses)
buf1_long$Mes <- factor(buf1_long$Mes, levels = orden_meses)
buf2_long$Mes <- factor(buf2_long$Mes, levels = orden_meses)
buf3_long$Mes <- factor(buf3_long$Mes, levels = orden_meses)
buf4_long$Mes <- factor(buf4_long$Mes, levels = orden_meses)

# ------------------------------------------------------



### ----  MAPA BOGOTÁ --------------------##

library(ggrepel)
coords_bogota_pm25 <- cbind(  st_coordinates(pts_long), st_drop_geometry(pts_long))



p_bogota <- ggplot() +
  
  geom_sf(data = buf4_long, aes(fill = PM25), alpha = 0.70, color = "black",linewidth = 0.2 ) +
  geom_sf(data = buf3_long, aes(fill = PM25), alpha = 0.40, color = NA) +
  geom_sf(data = loca_sf, fill = NA,color = "black",linewidth = 0.2 ) +
  geom_sf(data = buf4_long, aes(fill = PM25),alpha = 0.80,color = NA) +
  geom_sf(data = buf3_long, aes(fill = PM25), alpha = 0.80,color = NA  ) +
  geom_sf(data = buf2_long, aes(fill = PM25), alpha = 0.80, color = NA) +
  geom_point( data = coords_bogota_pm25,aes(X, Y, fill = PM25), shape = 21,
  size = 13,color = NA, stroke = 0.2, alpha = 0.9)  +
  geom_text(data = coords_bogota_pm25,aes(X,Y,label = round(PM25) ),
  size = 5.5, fontface = "plain", color = "black") +
  facet_wrap(~Mes, ncol = 6) +
  
  scale_fill_gradientn(
    colours = c(
      "#008000",  ## ≤10
      "#7FBF00",  ## 10-20
      "#D9EF00",  ## 20-30
      "#FFE000",  ## 30-40
      "#FF8C00",  ## 40-70
      "#FF2A00"   ## ≥70
    ),
    values = scales::rescale(
      c(0, 10, 20, 30, 40, 70)
    ),limits = c(0, 70),oob = scales::squish,name = expression(PM2.5~"("*mu*"g/"*m^3*")") ) +
  
  coord_sf(datum = NA) +
  theme_minimal() +
  
  theme(panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black",
      linewidth = 0.5 ),
    
    strip.text = element_text(face = "bold",color = "black",size = 20 ),
    
    panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.text = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank(), panel.spacing = unit(0.3, "lines"),
    legend.position = "right",legend.title = element_text(face = "bold",size = 13 ),
    legend.text = element_text(size = 12)) + 
    theme(legend.position = "right",legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),legend.key.size = unit(1.5, "cm")
    )


p_bogota

### Bogotá
ggsave("Mapa_bogota_Figura_7.pdf",plot = p_bogota,width = 20,
  height = 12,units = "in")

ggsave( "Mapa_bogota_Figura_7.png", plot = p_bogota,width = 20,
  height = 12,units = "in", dpi = 600 )



##=========================================
## MEDELLÍN MAPAS 
##=========================================

unzip("Comunas y Corregimientos de Medellín.zip", exdir = "medellin_shp")
list.files("medellin_shp", pattern = "\\.shp$")
comunas_med <- st_read("medellin_shp/LimiteComunaCorregimiento_2014.shp")
plot(comunas_med["NOMBRE"])


comunas <- comunas_med %>% filter(!grepl("Corregimiento", NOMBRE))
ggplot(comunas) + geom_sf(fill = "lightblue", color = "darkblue", size = 0.2) +
  labs(title = "Comunas de Medellín",caption = "Fuente: Datos Abiertos de Medellín") +
  theme_minimal()


maps <- read_excel("PARTICULAS PM25 BOGOTA MEDELLIN 20231.xlsx",sheet = 3)


med2 <- complete(med_MC)
med22 <- med2 %>%
  mutate(Dia = 1:n(),
         Mes = case_when(
           Dia <= 31 ~ "Enero",
           Dia <= 59 ~ "Febrero",
           Dia <= 90 ~ "Marzo",
           Dia <= 120 ~ "Abril",
           Dia <= 151 ~ "Mayo",
           Dia <= 181 ~ "Junio",
           Dia <= 212 ~ "Julio",
           Dia <= 243 ~ "Agosto",
           Dia <= 273 ~ "Septiembre",
           Dia <= 304 ~ "Octubre",
           Dia <= 334 ~ "Noviembre",
           TRUE ~ "Diciembre"
         ))


promedios_mensualesM <- med22 %>% group_by(Mes) %>% summarise(across(everything(), mean, na.rm = TRUE))
medpromedios <- as.data.frame(promedios_mensualesM)
locsM <- comunas$NOMBRE
med_est <- maps %>% filter(Municipio == "Medellín")
med_sf <- st_as_sf(med_est, coords = c("Longitud", "Latitud"),crs = 4326)
med_sf <- st_transform(med_sf, st_crs(comunas_med))




### ---------------------------------------------------------

lista_buf1 <- list()
lista_buf2 <- list()
lista_buf3 <- list()
lista_buf4 <- list()
lista_pts  <- list()

estaciones <- c("ÉXITO_SAN_ANT_MED_PM25","ALTAVISTA_MED_PM25","POLITÉCNICO_JAIME_ISAZA_MED_PM25")

for(i in 1:nrow(medpromedios)){
  
  tmp <- med_sf
  tmp$mes <- medpromedios$Mes[i]
  tmp$PM25 <- as.numeric(medpromedios[i, estaciones])
  
  b1 <- st_buffer(tmp, 100)
  b2 <- st_buffer(tmp, 200)
  b3 <- st_buffer(tmp, 500)
  b4 <- st_buffer(tmp, 900)
  
  for(b in list(b1, b2, b3, b4)){
    b$PM25 <- tmp$PM25
    b$mes <- tmp$mes
  }
  
  lista_buf1[[i]] <- b1
  lista_buf2[[i]] <- b2
  lista_buf3[[i]] <- b3
  lista_buf4[[i]] <- b4
  lista_pts[[i]]  <- tmp
}


buf1_long <- do.call(rbind, lista_buf1)
buf2_long <- do.call(rbind, lista_buf2)
buf3_long <- do.call(rbind, lista_buf3)
buf4_long <- do.call(rbind, lista_buf4)
pts_long  <- do.call(rbind, lista_pts)


orden_meses <- c("Enero", "Febrero", "Marzo", "Abril","Mayo", "Junio", "Julio", "Agosto",
  "Septiembre", "Octubre", "Noviembre", "Diciembre")

pts_long$mes  <- factor(pts_long$mes, levels = orden_meses)
buf1_long$mes <- factor(buf1_long$mes, levels = orden_meses)
buf2_long$mes <- factor(buf2_long$mes, levels = orden_meses)
buf3_long$mes <- factor(buf3_long$mes, levels = orden_meses)
buf4_long$mes <- factor(buf4_long$mes, levels = orden_meses)


### ----  MAPA MEDELLÍN -------------------- ##



coords_medellin_pm25 <- cbind(st_coordinates(pts_long),st_drop_geometry(pts_long))


p_medellin <- ggplot() +
    geom_sf(data = buf4_long, aes(fill = PM25), alpha = 0.70,color = "black",linewidth = 0.2) +
    geom_sf(data = buf3_long, aes(fill = PM25), alpha = 0.40, color = NA) +
    geom_sf(data = buf2_long, aes(fill = PM25), alpha = 0.80,color = NA) +
    geom_sf(data = buf1_long, aes(fill = PM25),alpha = 1,color = NA) +
  
  geom_sf(data = comunas_med,fill = NA, color = "black",linewidth = 0.2 ) +
  geom_sf(data = buf4_long, aes(fill = PM25), alpha = 0.80, color = NA) +
  geom_sf(data = buf3_long, aes(fill = PM25), alpha = 0.80, color = NA) +
  geom_sf(data = buf2_long, aes(fill = PM25), alpha = 0.80, color = NA) +
  
  geom_sf(data = buf1_long, aes(fill = PM25), alpha = 1,color = NA)+
  geom_point( data = coords_medellin_pm25,aes(X, Y, fill = PM25), shape = 21,
    size = 15,color = "black",stroke = 0.2, alpha = 0.9 ) +
  geom_text(data = coords_medellin_pm25,aes(X, Y, label = round(PM25)), size = 7,
    fontface = "plain",color = "black") +
  facet_wrap(~mes, ncol = 6) +
  scale_fill_gradientn(
    colours = c(
      "#008000",  ## ≤10
      "#7FBF00",  ## 10-20
      "#D9EF00",  ## 20-30
      "#FFE000",  ## 30-40
      "#FF8C00",  ## 40-70
      "#FF2A00"   ## ≥70
    ),
    values = scales::rescale(
      c(0, 10, 20, 30, 40, 70)
    ),limits = c(0, 70),oob = scales::squish, name = expression(PM2.5~"("*mu*"g/"*m^3*")"))  +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black",linewidth = 0.5),
    strip.text = element_text(face = "bold",color = "black",size = 20 ),
    panel.border = element_rect(color = "black",fill = NA,linewidth = 0.5),
    
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey90"),
    
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    panel.spacing = unit(0.3, "lines"),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, "cm")
  )

p_medellin


## Medellín
ggsave( "Mapa_Medellin_Figura_8.pdf", plot = p_medellin,
  width = 20, height = 9, units = "in")

ggsave( "Mapa_Medellin_Figura_8.png", plot = p_medellin,
  width = 20, height = 9, units = "in", dpi = 600)




##=========================================
## ÁREA DE ESTUDIO 
##=========================================
 

Mbogota = loca_sf_filtrado
Mmedellin = comunas
Mcali = comunasc


maps <- read_excel(
  "PARTICULAS PM25 BOGOTA MEDELLIN 20231.xlsx",
  sheet = 3
)

## -----------------------------------------

ggplot(Mbogota) + geom_sf(fill = "white", color = "black", size = 0.3) +
  labs(title = "", subtitle = "", caption = "Bogotá") + theme_minimal()


ggplot(Mmedellin) + geom_sf(fill = "white", color = "black", size = 0.2) +
  labs(title = "", caption = "Medellín") + theme_minimal()


ggplot(Mcali) + geom_sf(fill = "white", color = "black", size = 0.2) +
  labs(title = "", caption = "Cali") +theme_minimal()



maps_sf <- st_as_sf(maps, coords = c("Longitud", "Latitud"),crs = 4326)
est_bogota <- maps_sf %>% filter(Municipio == "Bogotá") %>% st_transform(st_crs(Mbogota))
est_medellin <- maps_sf %>% filter(Municipio == "Medellín") %>% st_transform(st_crs(Mmedellin))
est_cali <- maps_sf %>% filter(Municipio == "Cali") %>% st_transform(st_crs(Mcali))



## -----------------------------------
## Bogotá 
##-------------------------------------
library(ggspatial)
coords_bogota <- cbind(est_bogota,st_coordinates(est_bogota))
coords_bogota$ID <- seq_len(nrow(coords_bogota))
coords_bogota$Leyenda <- paste0(coords_bogota$ID, "  ", coords_bogota$Estación)


p_bogota <- ggplot() +
  geom_sf(data = Mbogota, fill = "grey92",color = "black",linewidth = 0.2 ) +
  geom_sf(data = coords_bogota, aes(shape = Leyenda), size = 8,fill = "white",color = "grey9",stroke = 0.6) +
  geom_sf(data = coords_bogota, aes(shape = Leyenda), size = 5,fill = "grey98",color = "white",stroke = 0.2) +
  
  geom_text(data = coords_bogota, aes(X, Y, label = ID),size = 4.5, family = "sans",color = "black") +
  scale_shape_manual( values = rep(21, nrow(coords_bogota)),name = "       Estaciones") +
  
  labs( title = "Bogotá", x = NULL, y = NULL) +
  guides( shape = guide_legend(override.aes = list(shape = NA),keywidth = 0, keyheight = 0) ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold",size = 20 ),
    panel.border = element_rect(colour = "black", fill = NA,linewidth = 0.2),
    panel.grid.major = element_line(colour = "grey90",linewidth = 0.3 ),
    panel.grid.minor = element_blank(),
    axis.text = element_text( size = 12,colour = "black"),
    axis.ticks = element_line(colour = "black"),
    
    axis.title = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold",size = 13 ),
    legend.text = element_text(size = 13)) +
  coord_sf() +
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1 ) 
        )

p_bogota

ggsave(filename = "Mapa_Bogota.png",plot = p_bogota,width = 8,
  height = 6, dpi = 600)




## ----------------------------
## Medellín
##------------------------------

coords_medellin <- cbind( est_medellin, st_coordinates(est_medellin))
coords_medellin$ID <- seq_len(nrow(coords_medellin))
coords_medellin$Leyenda <- paste0(coords_medellin$ID, "  ", coords_medellin$Estación)



p_medellin <- ggplot() +
  
  geom_sf(data = Mmedellin, fill = "grey92",color = "black",linewidth = 0.2) +
  geom_sf(data = coords_medellin,  aes(shape = Leyenda),size = 8, fill = "white",
    color = "grey9",stroke = 0.6) +
  geom_sf(data = coords_medellin, aes(shape = Leyenda),size = 5,fill = "grey98",
    color = "white",stroke = 0.2) +
  geom_text(data = coords_medellin,aes(X, Y, label = ID),size = 4.5,family = "sans",color = "black"  ) +
  scale_shape_manual(values = rep(21, nrow(coords_medellin)),name = "       Estaciones" ) +
  
  labs(title = "Medellín",x = NULL, y = NULL) +
  guides( shape = guide_legend(override.aes = list(shape = NA),keywidth = 0,
      keyheight = 0)) +
  theme_minimal() +
  theme(
  plot.title = element_text(hjust = 0.5,face = "bold",size = 20),
  panel.border = element_rect(colour = "black",fill = NA,linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey90",linewidth = 0.3),
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 12,colour = "black"),axis.ticks = element_line(colour = "black"),
  axis.title = element_blank(),legend.position = "right",
  legend.title = element_text(face = "bold",size = 13),
  legend.text = element_text(size = 13)) +
  coord_sf() +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)
  )


p_medellin

## 
ggsave(filename = "Mapa_Medellin.png",plot = p_medellin,width = 8,height = 6,dpi = 600)






## ----------------------------
## Cali
##------------------------------

coords_cali <- cbind(est_cali,st_coordinates(est_cali))
coords_cali$ID <- seq_len(nrow(coords_cali))
coords_cali$Leyenda <- paste0(coords_cali$ID, "  ", coords_cali$Estación)




p_cali <- ggplot() +
  
  geom_sf(data = Mcali,fill = "grey92", color = "black",linewidth = 0.2) +
  geom_sf(data = coords_cali, aes(shape = Leyenda), size = 8, fill = "white",color = "grey9",stroke = 0.6) +
  geom_sf(data = coords_cali,  aes(shape = Leyenda),size = 5, fill = "grey98",color = "white",stroke = 0.2) +
  geom_text(data = coords_cali,  aes(X, Y, label = ID),  size = 4.5,family = "sans",color = "black") +
  
  scale_shape_manual( values = rep(21, nrow(coords_cali)),name = "       Estaciones") +
  
  labs( title = "Cali", x = NULL,y = NULL) +
  guides( shape = guide_legend( override.aes = list(shape = NA),keywidth = 0, keyheight = 0 )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 20 ),
  panel.border = element_rect(colour = "black", fill = NA,linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey90",linewidth = 0.3),
  panel.grid.minor = element_blank(),
    axis.text = element_text(  size = 12, colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold",size = 13 ),
    legend.text = element_text(size = 13)) +
  coord_sf() +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)
  )


p_cali

ggsave(filename = "Mapa_Cali.png",plot = p_cali,width = 8,height = 6, dpi = 600)

## _------------ MAPAS ------------------##

library(patchwork)
p_tres <- (p_bogota + p_medellin + p_cali) +plot_layout(ncol = 3)
ggsave("Mapas_Ciudades_Figura_1.png", plot = p_tres,width = 18,height = 7,dpi = 600)
ggsave("Mapas_Ciudades_Figura_1.pdf",plot = p_tres,width = 18,height = 7)

