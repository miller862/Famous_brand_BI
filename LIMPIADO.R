library(tidyverse)
library(readxl)
library(stringr)
library(xlsx)
library(reshape2)
library(knitr)
library(kableExtra)
library(sf)
library(stringi)
library(viridis)
library(RColorBrewer)
library(kableExtra)
library(scales)
library(patchwork)
library(treemap)

## limpieza de datos
tanda<-read_excel("tandas/JULIOPRUEBA.xlsx") %>% 
  rename(nombreviejo="Nombre del producto",
         unidades="Cantidad del producto",
         instagram="Notas del comprador",
         envio_medio="Medio de envío",
         Provincia="Provincia o estado",
         orden="Número de orden",
         tipo_pago="Medio de pago",
         CP="Código postal",
         envio_costo="Costo de envío",
         DNI_CUIT="DNI / CUIT",
         nombre="Nombre del comprador",
         nombre_envio="Nombre para el envío",
         celu="Teléfono",
         celu_envio="Teléfono para el envío",
         precio_unitario="Precio del producto",
         subtotal="Subtotal de productos",
         altura="Número",
         calle="Dirección"
         ) %>% 
  select(-"Estado de la orden",-"Estado del pago",-"Estado del envío",-Moneda,-Descuento,
         -País,-"Cupón de descuento",-"Notas del vendedor",
         -"Fecha de pago",-"Fecha de envío",-TIPO,-SKU,-"Código de tracking del envío",
         -"Identificador de la transacción en el medio de pago",-"Producto Físico",
         -"Persona que registró la venta",-"...34",-"Identificador de la orden")

tanda2 <- tanda %>% 
  mutate(tipo = case_when(
    str_detect(nombreviejo, "Universidades") ~ "UNI",
    str_detect(nombreviejo, "Universida") ~ "UNI",
    str_detect(nombreviejo, "Buzos universitar") ~ "UNI",
    str_detect(nombreviejo, "Buzos Univer") ~ "UNI",
    str_detect(nombreviejo, "Casas") ~ "HP",
    str_detect(nombreviejo, "feed") ~ "FEED",
    str_detect(nombreviejo, "personalizado") ~ "PERSO"
  ))

tanda3<-tanda2 %>% 
  mutate(universidad = case_when(
    str_detect(nombreviejo, "UNLP") ~ "UNLP",
    str_detect(nombreviejo, "UBA") ~ "UBA",
    str_detect(nombreviejo, "UTN") ~ "UTN",
    str_detect(nombreviejo, "UNC") ~ "UNC",
    str_detect(nombreviejo, "UNAJ") ~ "UNAJ",
    str_detect(nombreviejo, "UNL") ~ "UNL",
    str_detect(nombreviejo, "UNR") ~ "UNR",
    str_detect(nombreviejo, "UNA") ~ "UNA",
    str_detect(nombreviejo, "UNQ") ~ "UNQ",
    str_detect(nombreviejo, "UNSAM") ~ "UNSAM",
    str_detect(nombreviejo, "UNLAM") ~ "UNLAM",
    str_detect(nombreviejo, "UNMDP") ~ "UNMDP",
    str_detect(nombreviejo, "UNLA") ~ "UNLA",
    str_detect(nombreviejo, "UNNE") ~ "UNNE",
    str_detect(nombreviejo, "UNER") ~ "UNER",
    str_detect(nombreviejo, "UNCO") ~ "UNCO",
    str_detect(nombreviejo, "UNT") ~ "UNT",
    str_detect(nombreviejo, "UNICEN") ~ "UNICEN",
    str_detect(nombreviejo, "UNCUYO") ~ "UNCUYO",
    str_detect(nombreviejo, "UNM") ~ "UNM",
    str_detect(nombreviejo, "UNSL") ~ "UNSL",
    str_detect(nombreviejo, "UNLZ") ~ "UNLZ",
    str_detect(nombreviejo, "UNTREF") ~ "UNTREF",
    str_detect(nombreviejo,"UNDAV")~ "UNDAV"
  ))

tanda4<-tanda3 %>% 
  mutate(modelo = case_when(
    str_detect(nombreviejo, "Oversize") ~ "OVERSIZE",
    str_detect(nombreviejo, "VINTAGE") ~ "VINTAGE"
  ))

tanda5<-tanda4 %>% 
  mutate(color = case_when(
    str_detect(nombreviejo, "PETROLEO") ~ "PETROLEO",
    str_detect(nombreviejo,"gris petroleo")~ "PETROLEO",
    str_detect(nombreviejo, "CREMITA") ~ "CREMITA",
    str_detect(nombreviejo, "cremita")~ "CREMITA",
    str_detect(nombreviejo, "gris melange") ~ "MELANGE",
    str_detect(nombreviejo, "Blanco") ~ "BLANCO",
    str_detect(nombreviejo, "Negro") ~ "NEGRO",
    str_detect(nombreviejo, "ingles") ~ "INGLES",
    str_detect(nombreviejo, "Lila") ~ "LILA",
    str_detect(nombreviejo, "Celeste") ~ "CELESTE",
    str_detect(nombreviejo, "Azul francia") ~ "FRANCIA"
  ))

tanda6<-tanda5 %>% 
  mutate(hilo = case_when(
    str_detect(nombreviejo, "Hilo cremita") ~ "cremita",
    str_detect(nombreviejo, "Azul petroleo") ~ "azul_petroleo",
    str_detect(nombreviejo, "El color de mi") ~ "color_uni",
    str_detect(nombreviejo, "si ") ~ "metalizado"
  ))

tanda7<-tanda6 %>% 
  mutate(talle = case_when(
    str_detect(nombreviejo, "talle M") ~ "M",
    str_detect(nombreviejo, "talle S") ~ "S",
    str_detect(nombreviejo, "talle L") ~ "L",
    str_detect(modelo, "VINT") ~ "VINTAGE"
  ))

tanda8<-tanda7 %>% 
  mutate(capybol = case_when(
    str_detect(nombreviejo, "capucha") ~ "CAPUCHA",
    .default = "NO"
  ))

tanda9<-tanda8 %>% 
  mutate(
    casa = case_when(
      str_detect(nombreviejo, "Gry") ~ "GRYFFINDOR",
      str_detect(nombreviejo, "Rav") ~ "RAVENCLAW",
      str_detect(nombreviejo, "Huff") ~ "HUFFLEPUFF",
      str_detect(nombreviejo, "Sly") ~ "SLYTHERIN"
      ))

tanda10<-tanda9 %>% 
  mutate(altura=as.integer(altura),
         talle=as.factor(talle),
         modelo=as.factor(modelo),
         color=as.factor(color),
         tipo=as.factor(tipo),
         capybol=as.factor(capybol),
         universidad=as.factor(universidad),
         Ciudad=tolower(stri_trans_general(Ciudad, "Latin-ASCII")),
         Localidad=tolower(stri_trans_general(Localidad, "Latin-ASCII")),
         Provincia=tolower(stri_trans_general(Provincia, "Latin-ASCII")))  #aca paso todas las localidades a minuscula y sin tilde

final <- tanda10 %>%
  group_by(orden) %>%
  mutate(
    Localidad = ifelse(is.na(Localidad), first(Localidad[!is.na(Localidad)]), Localidad),
    Ciudad = ifelse(is.na(Ciudad), first(Ciudad[!is.na(Ciudad)]), Ciudad),
    Provincia = ifelse(is.na(Provincia), first(Provincia[!is.na(Provincia)]), Provincia)
  ) %>%
  ungroup()

## levanto el excel de contabilidad para crear las tablas relacionadas
SUELDOS<-read.xlsx2("tablas_contables/CONTABILIDAD_JULIO.xlsx",2)
COSTURA<-read.xlsx2("tablas_contables/CONTABILIDAD_JULIO.xlsx",3)
COSTOS_FIJOS<-read.xlsx2("tablas_contables/CONTABILIDAD_JULIO.xlsx",4)
## exploracion

## DETECTANDO ERRORES POR INGRESO DE PRODUCTOS AD HOC
final %>% 
  filter(capybol=="CAPUCHA" & modelo=="VINTAGE")

### SUMA DE BUZOS TOTALES
sum(final$unidades)

### CANTIDADES POR TIPO DE BUZO
JULIO_VENTAS <- final %>%
  summarise(Q_pedidos = n_distinct(orden),
            Q_buzos = sum(unidades, na.rm = TRUE),
            ingresos = sum(Total, na.rm = TRUE),
            correoarg=sum(envio_costo, na.rm = T),
            motomensajeria = sum(na.omit(envio_costo[envio_medio == "Moto mensajería (CABA)"])))



JULIO_VENTAS
final %>% 
  group_by(tipo) %>% 
  summarise(cantidad=sum(unidades))
            
### CALCULADORA DE TELA
#SOLO CANTIDADES
calcu_simple<-final %>% 
  group_by(color) %>% 
  summarise(cantidad=n(),
            Kg=(cantidad*700)/1000,
            rollos=round(Kg/22,2)) %>% 
  arrange(-Kg)

#CANTIDADES Y COSTO
calcu_completa<-final %>% 
  group_by(color) %>% 
  summarise(cantidad=n(),
            Kg=(cantidad*700)/1000,
            rollos=round(Kg/22,2),
            costoT=Kg*10000,
            rib=(Kg*10)/100,
            costoR=round(rib*12790),
            costoF=round(costoT+costoR))%>%
  arrange(-costoF)

sum(calcu_completa$costoF)
## LISTA PARA PRODUCCION

listado<-final %>% 
  group_by(capybol,talle,color) %>% 
  summarise(cantidad=sum(unidades))

# Convertir la tabla a formato ancho
tabla_ancha <- dcast(listado, color + talle ~ capybol, value.var = "cantidad")

# Mostrar la tabla de manera organizada usando kable
kable(tabla_ancha, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)


### BUZOS UNIVERSITARIOS POR COLOR Y POR TALLE (SIN DISCRIMINAR CAP Y BOL)

final%>% 
  filter(tipo=="UNI") %>% 
  group_by(color,talle) %>% 
  summarise(cantidad=sum(unidades)) %>% 
  view()

"""
ESTE CODIGO NO DEBE SER USADO YA QUE NO CALCULA PORCENTAJES NI TOTALES EN BASE A LAS UNIDADES
SINO EN BASE A LOS REGISTROS 

table(final$color,final$talle)
prop.table(table(final$color,final$talle),1)
"""
##TABLA CRUZADA PARA COLOR Y TALLE IGNORANDO capybol Y PARA TODOS LOS BUZOS

# Agrupar por color y talle y sumar las unidades
tabla_absoluta_colorytalle <- final %>%
  group_by(color, talle) %>%
  summarise(unidades = sum(unidades, na.rm = TRUE)) %>%
  pivot_wider(names_from = talle, values_from = unidades, values_fill = list(unidades = 0))

# Calcular la tabla de valores porcentuales en base a las filas

tabla_porcentual_colorytalle <- tabla_absoluta_colorytalle %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ percent(. / total, accuracy = 1))) %>%
  select(-total)



## ANALISIS ESPACIAL

### Pedidos por provincia

pedidos_x_provincia <-final %>%     ### aca conservamos la distincion buenos aires, gran buenos aires
  group_by(Provincia) %>% 
  summarise(pedidos=n()) %>% 
  arrange(-pedidos) %>% 
  rename(provincia=Provincia)



pedidos_x_provincia%>% 
  kable() %>% 
  kable_styling(full_width = F)

provincias<-st_read("geo_provincias/pxpciadatosok.shp") %>%
  mutate(provincia=tolower(stri_trans_general(provincia, "Latin-ASCII")))

pedidos_x_provincia <- pedidos_x_provincia %>% 
  mutate(provincia = ifelse(provincia == "capital federal", "ciudad autonoma de buenos aires", provincia)) %>%
  mutate(pedidos = ifelse(provincia == "buenos aires", 
                          pedidos + sum(pedidos[provincia == "gran buenos aires"]), 
                          pedidos)) %>%
  filter(provincia != "gran buenos aires") %>% 
  right_join(provincias %>% select(provincia,geometry),by="provincia")


##HACER ESTE MAPA FUE UN PARTO PORQUE NO PODIA SINCRONIZAR LAS ESCALAS DE COLORES. EL PROBLEMA ERA QUE LA TABLA CABA AL TENER UN SOLO REGISTRO, NO SITUA A CABA EN SU LUGAR CORRECTO EN LA ESCAL SINO QUE SE SITUA EN EL MEDIO DEL ESPECTRO. HAY QUE DEFINIR LOS LIMITES DE LA ESCALA

# Obtener el rango de valores de pedidos
valor_max <- max(pedidos_x_provincia$pedidos, na.rm = TRUE)

# Definir breaks personalizados basados en la distribución de los datos
breaks <- c(1, 2, 4, 8, 15, 30, 60, 120, 250, 500, 800)

# Definir la paleta de colores viridis
colors <- viridis(n = length(breaks) - 1)

# Mapa principal
mapa_arg <- ggplot(data = pedidos_x_provincia) +
  geom_sf(aes(fill = pedidos, geometry = geometry)) + 
  scale_fill_viridis_c(trans = "log", breaks = breaks, option = "plasma", limits = c(1, valor_max)) + 
  labs(title = "PEDIDOS POR PROVINCIA",
       subtitle = "Argentina",
       fill = "Pedidos",
       caption = "Elaborado por Manuel Miller") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.caption = element_text(face = "italic"),
    axis.text = element_blank(), 
    axis.ticks = element_blank()
  )

# Datos de la Ciudad Autónoma de Buenos Aires
caba <- pedidos_x_provincia %>% filter(provincia == "ciudad autonoma de buenos aires")

# Mapa ampliado de CABA
mapa_caba <- ggplot(data = caba) +
  geom_sf(aes(fill = pedidos, geometry = geometry)) + 
  scale_fill_viridis_c(trans = "log", breaks = breaks, option = "plasma", limits = c(1, valor_max)) + 
  labs(title = "Ciudad Autónoma de Buenos Aires",
       fill = "Pedidos") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 6.67),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),  
    axis.ticks = element_blank()
  )

# Combinar mapas
mapa_combinado <- mapa_arg + 
  inset_element(
    mapa_caba,
    left = 0.5, bottom = 0.5, right = 0.85, top = 0.85
  )

mapa_combinado


###RANKING DE CLIENTES
TOP10CLIENTES<- final %>%
  group_by(instagram) %>% 
  summarise(total_gastado =sum(Total)) %>% 
  arrange(-total_gastado) %>% 
  left_join(final %>% select(instagram, nombre), by = "instagram") %>%
  distinct() %>% 
  select(nombre,everything()) %>% 
  head(10)

##ENVIOS

###MOTO MENSAJERIA
final %>% filter(envio_medio=="Moto mensajería (CABA)") %>% 
  summarise(
    total_de_envios=n(),
    sueldo_tio = sum(envio_costo)
  ) %>% 
  view()
  


##plots
#levanto este shape que tiene los departamentos del pais
deptos<-st_read("geo_deptos/pxdptodatosok.shp")

deptos$departamen <- tolower(stri_trans_general(deptos$departamen, "Latin-ASCII")) ##todo a minuscalua y sin acentos

deptos_BA<-deptos %>%        #en esta tabla guardo los partidos de BA
  filter(provincia=="Buenos Aires") %>% 
  arrange(departamen)

comunas_CABA<-deptos %>% #en esta guardo las comunas de caba
  filter(provincia=="Ciudad Autónoma de Buenos Aires")

##baje estos otros hsape que pueden servir mas adelante
shapepais<-st_read("geo_pais/pxlocdatos.shp")
shapebsas<-st_read("geo_bsas/Buenos_Aires_con_datos.shp")
st_crs(shapepais)


shapebsas$link <- substr(shapebsas$link, 1, 8)

localidades_ba <- shapebsas %>%
  group_by(link) %>%
  summarize(geometry = st_union(geometry)) 

plot(localidades_ba["geometry"])   #<-  localidades bsas
plot(shapebsas["geometry"])        #<- radios censales
plot(deptos["geometry"])           #<-    Deptos arg
plot(deptos_BA["geometry"])        # <- deptos BSAS
plot(comunas_CABA["geometry"])    # <-comunas caba
codigosbsas<-read_excel("codigosbsas.xlsx") %>% 
  rename("link"=toponimo_i) %>% 
  mutate(link=as.character(link))

#exploracion
matrizba<-final %>%
  filter(Provincia=="buenos aires"| Provincia=="gran buenos aires")
unique(matrizba$Provincia)
unique(matrizba$Localidad)
unique(matrizba$Ciudad)
unique(final$Provincia)
unique(deptos_BA$departamen)


pedidos_ba<-matrizba %>% 
  group_by(Ciudad) %>% 
  summarise(pedidos=n()) %>% 
  arrange(-pedidos) %>% 
  rename("departamen"=Ciudad)


result <- left_join(deptos_BA,pedidos_ba, by = "departamen")
plot(result["geometry"])

###MAPA DE PARTIDOS
ggplot(data = result) +
  geom_sf(aes(fill = pedidos))+ 
  scale_fill_viridis_c(trans = "log", breaks=c(0,3,10,20,50,100,120),option = "inferno") + 
  labs(title = "PEDIDOS POR PARTIDO",
       subtitle = "Provincia de Buenos Aires",
       fill = "Pedidos",
       caption = "Elaborado por Manuel Miller")+
  theme_void()

ggplot(data = result) +
  geom_sf(aes(fill = pedidos)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), trans = "log", breaks = c(0,1,2, 3, 10, 20, 50, 100, 120)) + 
  labs(title = "PEDIDOS POR PARTIDO",
       subtitle = "Provincia de Buenos Aires",
       fill = "Pedidos",
       caption = "Elaborado por Manuel Miller") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),  # Ajustar el ancho de la barra de la leyenda
    legend.key.height = unit(0.5, "cm"),  # Ajustar la altura de la barra de la leyenda
    plot.caption = element_text(face = "italic")
  )
localidades_ba<-  localidades_ba%>%
  left_join(codigosbsas, by="link")

reuslt2<-left_join(localidades_BA,pedidos_ba, by = "departamen")
result2<-na.omit(result2)

result3<-matrizba
  filter(Provincia=="gran buenos aires")%>% 
    group_by(Ciudad) %>% 
    summarise(pedidos=n()) %>% 
    arrange(-pedidos) %>% 
    rename("departamen"=Ciudad) %>% 
    left_join(deptos_BA, by = "departamen")
    
