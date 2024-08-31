

##carga de librerias
library(tidyverse)
library(readxl)
library(stringr)
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
library(openxlsx)
library(rlang)
## limpieza de datos

#levanto, quito columnas, renombro columnas
tanda<-read_xlsx("tandas/JULIOPRUEBA.xlsx") %>% 
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

#detecto el tipo
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

#detecto universidades
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

#detecto el modelo
tanda4<-tanda3 %>% 
  mutate(modelo = case_when(
    str_detect(nombreviejo, "Oversize") ~ "OVERSIZE",
    str_detect(nombreviejo, "VINTAGE") ~ "VINTAGE"
  ))
#detecto el color
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

#detecto el hilo
tanda6<-tanda5 %>% 
  mutate(hilo = case_when(
    str_detect(nombreviejo, "Hilo cremita") ~ "cremita",
    str_detect(nombreviejo, "Azul petroleo") ~ "azul_petroleo",
    str_detect(nombreviejo, "El color de mi") ~ "color_uni",
    str_detect(nombreviejo, "si ") ~ "metalizado"
  ))

#detecto el talle
tanda7<-tanda6 %>% 
  mutate(talle = case_when(
    str_detect(nombreviejo, "talle M") ~ "M",
    str_detect(nombreviejo, "talle S") ~ "S",
    str_detect(nombreviejo, "talle L") ~ "L",
    str_detect(modelo, "VINT") ~ "VINTAGE"
  ))

#detecto capybol
tanda8<-tanda7 %>% 
  mutate(capybol = case_when(
    str_detect(nombreviejo, "capucha") ~ "CAPUCHA",
    .default = "NO"
  ))

#detecto casa de HP
tanda9<-tanda8 %>% 
  mutate(
    casa = case_when(
      str_detect(nombreviejo, "Gry") ~ "GRYFFINDOR",
      str_detect(nombreviejo, "Rav") ~ "RAVENCLAW",
      str_detect(nombreviejo, "Huff") ~ "HUFFLEPUFF",
      str_detect(nombreviejo, "Sly") ~ "SLYTHERIN"
      ))

#cambio tipos de datos y saco tildes y mayusculas de las ubicaciones
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

#copio las ciudades, provincias y localidades en los registros que vienen en blanco
final <- tanda10 %>%
  group_by(orden) %>%
  mutate(
    Localidad = ifelse(is.na(Localidad), first(Localidad[!is.na(Localidad)]), Localidad),
    Ciudad = ifelse(is.na(Ciudad), first(Ciudad[!is.na(Ciudad)]), Ciudad),
    Provincia = ifelse(is.na(Provincia), first(Provincia[!is.na(Provincia)]), Provincia)
  ) %>%
  ungroup()

JULIO_VENTAS <- final %>%
  summarise(Q_pedidos = n_distinct(orden),
            Q_buzos = sum(unidades, na.rm = TRUE),
            ingresos = sum(Total, na.rm = TRUE))



JULIO_VENTAS %>% 
  kable() %>% 
  kable_styling()

TOP10CLIENTES<- final %>%
  group_by(instagram) %>% 
  summarise(total_gastado =sum(Total)) %>% 
  arrange(-total_gastado) %>% 
  left_join(final %>% select(instagram, nombre), by = "instagram") %>%
  distinct() %>% 
  select(nombre,everything()) %>% 
  head(10)

TOP10CLIENTES %>% 
  kable() %>% 
  kable_styling()

final %>% 
  group_by(tipo) %>% 
  summarise(cantidad=sum(unidades)) %>% 
  arrange(-cantidad) %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left")

### BUZOS UNIVERSITARIOS POR COLOR Y POR TALLE (SIN DISCRIMINAR CAP Y BOL)

final%>% 
  filter(tipo=="UNI") %>% 
  group_by(color,talle) %>% 
  summarise(cantidad=sum(unidades))%>% 
  kable() %>% 
  kable_styling()

ranking_uni<-final %>% 
  group_by(universidad) %>% 
  summarise(cantidad=sum(unidades)) %>% 
  arrange(-cantidad) %>% 
  na.omit()

treemap(ranking_uni,
            index="universidad",
            vSize="cantidad",
            type="index",
            palette = "Paired",
            title= "Grafico Treemap de universidades"
            )





ranking_uni_plot<-ggplot(ranking_uni, aes(x = reorder(universidad,-cantidad), y = cantidad, fill = cantidad))+
  geom_bar(stat ="identity")+
  geom_text(aes(label = cantidad), vjust =-0.5, color ="black", size =3.5)+# Agregar etiquetas encima de las barras
  scale_fill_viridis(discrete =FALSE)+
  labs(x ="Universidad", y ="Cantidad", title ="Ranking de Universidades")+
  theme_void()+
  theme(
    axis.text.x = element_text(angle =45, hjust =1),
    plot.title = element_text(size =16, face ="bold", margin = margin(b =20)),# Aumentar espacio debajo del título
    plot.margin = margin(t =10, r =10, b =30, l =10))# Ajustar márgenes del gráfico)

ranking_uni_plot

##TABLA CRUZADA PARA COLOR Y TALLE IGNORANDO capybol Y PARA TODOS LOS BUZOS

# Agrupar por color y talle y sumar las unidades
tabla_absoluta_colorytalle <- final %>%
  group_by(color, talle) %>%
  summarise(unidades = sum(unidades, na.rm = TRUE)) %>%
  pivot_wider(names_from = talle, values_from = unidades, values_fill = list(unidades = 0))

# Calcular la tabla de valores porcentuales


tabla_porcentual_colorytalle <- tabla_absoluta_colorytalle %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric)))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ percent(. / total, accuracy = 1))) %>%
  select(-total)

tabla_absoluta_colorytalle %>% 
  kable() %>% 
  kable_styling(full_width = F)

tabla_porcentual_colorytalle%>% 
  kable() %>% 
  kable_styling(full_width = F)

# Vector de colores basado en la lista proporcionada
colores_tela <-c("PETROLEO"="#003d34","CREMITA"="#f5e4e2","NA"="#d2b5b4","INGLES"="#c0d6e4","NEGRO"="#000000","LILA"="#b17e9f","BLANCO"="#ffffff","FRANCIA"="#e3d9ff","MELANGE"="#d0cfcf","CELESTE"="#00bfff")

### CALCULADORA DE TELA
calcu_simple<-final %>% 
  group_by(color) %>% 
  summarise(cantidad=n(),
            Kg=(cantidad*700)/1000,
            rollos=round(Kg/22,2)) %>% 
  arrange(-Kg)

calcu_simple%>% 
  kable() %>% 
  kable_styling()

#CANTIDADES Y COSTO
calcu_completa<-final %>% 
  group_by(color) %>% 
  summarise(cantidad=n(),
            Kg=(cantidad*700)/1000,
            rollos=round(Kg/22,2),
            costoT=Kg*10000,
            rib=(Kg*20)/100,
            costoR=round(rib*12790),
            costoF=round(costoT+costoR))%>%
  arrange(-costoF)

calcu_completa %>% 
  kable() %>% 
  kable_styling()

listado<-final %>% 
  group_by(capybol,talle,color) %>% 
  summarise(cantidad=sum(unidades))

# Convertir la tabla a formato ancho
tabla_ancha <- dcast(listado, color + talle ~ capybol, value.var = "cantidad")

#INTENTO FINAL DE UNA BUENA TABLA CRUZADA.
AUXCAP<-final %>%
  filter(capybol == "CAPUCHA") %>%
  select(color, talle, unidades) %>%
  group_by(talle, color) %>%
  summarise(cantidad = sum(unidades), .groups = 'drop') %>%
  pivot_wider(names_from = talle, values_from = cantidad) %>% 
  rename(s="S",
         m="M",
         l="L",
         vintage="VINTAGE")

AUXNO<-final %>%
  filter(capybol == "NO") %>%
  select(color, talle, unidades) %>%
  group_by(talle, color) %>%
  summarise(cantidad = sum(unidades), .groups = 'drop') %>%
  pivot_wider(names_from = talle, values_from = cantidad)

nueva_tabla<-AUXNO %>%
  left_join(AUXCAP,by="color")

listado %>% 
  kable() %>%
  kable_styling(full_width = T,position = "center") %>%
  scroll_box(width = "400px" ,height = "400px", fixed_thead = TRUE)

# Mostrar la tabla de manera organizada usando kable
kable(tabla_ancha, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = T)%>%
  scroll_box(width = "400px", height = "400px", fixed_thead = TRUE)

kbl(nueva_tabla)%>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  add_header_above(c(" " = 1, "Sin capucha y bolsillo" = 4, "con Capucha y bolsillo" = 4))

produ_HP <- final %>%
  filter(tipo == "HP") %>%
  mutate(talle = as.character(talle))

produ_HP <- table(produ_HP$casa, produ_HP$talle)

kbl(produ_HP)%>%
  row_spec(1, bold = T, color = "black", background = "#FF6961") %>% 
  row_spec(2, bold = T, color = "black", background = "#FDFD96") %>%
  row_spec(3, bold = T, color = "black", background = "#B3D4FC") %>% 
  row_spec(4, bold = T, color = "black", background = "#77DD77") %>%
    kable_styling(full_width = F,position = "left")


tabla_hilo<-final %>% 
  filter(tipo!="HP") %>% 
  group_by(hilo) %>% 
  summarise(n_buzos=sum(unidades)) 

tabla_hilo%>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left")
  

pedidos_x_provincia <-final %>%     ### aca conservamos la distincion buenos aires, gran buenos aires
  group_by(Provincia) %>% 
  summarise(pedidos=n()) %>% 
  arrange(-pedidos) %>% 
  rename(provincia=Provincia)




pedidos_x_provincia %>%
  kbl() %>%
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("condensed")) 

provincias<-st_read("geos/geo_provincias/pxpciadatosok.shp") %>%
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


# Mapa principal con número de pedidos
mapa_arg <- ggplot(data = pedidos_x_provincia) +
  geom_sf(aes(fill = pedidos, geometry = geometry)) + 
  geom_sf_text(aes(label = pedidos, geometry = geometry), size = 3, color = "black", check_overlap = TRUE) +
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

# Mapa ampliado de CABA con número de pedidos
mapa_caba <- ggplot(data = caba) +
  geom_sf(aes(fill = pedidos, geometry = geometry)) + 
  geom_sf_text(aes(label = pedidos, geometry = geometry), size = 3, color = "black", check_overlap = TRUE) +
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

#carga de datos
deptos<-st_read("geos/geo_deptos/pxdptodatosok.shp")

deptos$departamen <- tolower(stri_trans_general(deptos$departamen, "Latin-ASCII")) ##todo a minuscalua y sin acentos

deptos_BA<-deptos %>%        #en esta tabla guardo los partidos de BA
  filter(provincia=="Buenos Aires") %>% 
  arrange(departamen)

comunas_CABA<-deptos %>% #en esta guardo las comunas de caba
  filter(provincia=="Ciudad Autónoma de Buenos Aires")

##baje estos otros hsape que pueden servir mas adelante

shapebsas<-st_read("geos/geo_bsas/Buenos_Aires_con_datos.shp")



shapebsas$link <- substr(shapebsas$link, 1, 8)

#localidades_ba <- shapebsas %>%       #las localidades no son usadas aun
#  group_by(link) %>%
#  summarize(geometry = st_union(geometry)) 

#codigosbsas<-read_xlsx("codigosbsas.xlsx") %>%  
#  rename("link"=toponimo_i) %>% 
#  mutate(link=as.character(link))

#exploracion
matrizba <- final %>%
  filter(Provincia == "buenos aires" | Provincia == "gran buenos aires") %>%
  mutate(
    Ciudad = str_trim(Ciudad), # Sacar espacios al principio y al final
    Ciudad = str_replace_all(Ciudad, regex("(?i)buenos aires"), ""), # Borrar todas las variantes de Buenos Aires (insensible a mayúsculas)
    Ciudad = str_replace_all(Ciudad, regex("(?i)(.*quilmes.*)"), "quilmes"), # Conservar solo "quilmes"
    Ciudad = str_replace_all(Ciudad, regex("(?i)gral\\. pueyrredon"), "general pueyrredon"), # Cambiar "gral. pueyrredon" a "general pueyrredon"
    Ciudad = str_replace_all(Ciudad, regex("(?i)angel etcheverry, la plata"), "la plata"), # Cambiar "angel etcheverry, la plata" a "la plata"
    Ciudad = str_replace_all(Ciudad, regex("(?i)general san martin, gran"), "general san martin"), # Cambiar "general san martin, gran" a "general san martin"
    Ciudad = str_replace_all(Ciudad, regex("(?i)ranos mejia"), "ramos mejia"), # Cambiar "ranos mejia" a "ramos mejia"
    Ciudad = str_replace_all(Ciudad, regex("(?i), provincia de|provincia de"), ""), # Borrar ", provincia de" y "provincia de" en todas sus formas
    Ciudad = str_replace_all(Ciudad, regex("(?i)zelaya\\. pilar"), "pilar"), # Cambiar "zelaya. pilar" a "pilar"
    Ciudad = str_replace_all(Ciudad, regex("(?i)moreno, cuartel v"), "moreno"), # Cambiar "moreno, cuartel v" a "moreno"
    Ciudad = str_replace_all(Ciudad, regex("(?i)lanus oeste"), "lanus"), # Cambiar "lanus oeste" a "lanus"
    Ciudad = str_replace_all(Ciudad, regex("(?i)city bell, la plata"), "la plata"), # Cambiar "city bell, la plata" a "la plata"
    Ciudad = str_replace_all(Ciudad, regex("(?i)jose clemente paz"), "jose c. paz"), # Cambiar "jose clemente paz" a "jose c. paz"
    Ciudad = str_replace_all(Ciudad, regex("(?i)san justo|ramos mejia|aldo bonzi|rafael castillo|ciudad evita|gonzalez catan|gregorio de laferrere|la tablada|lomas del mirador|isidro casanova|tapiales|20 de junio|villa eduardo madero|villa luzuriaga|virrey del pino"), "la matanza"), # Cambiar las localidades de La Matanza a "la matanza"
    Ciudad = str_replace_all(Ciudad, regex("(?i)banfield"), "lomas de zamora"), # Cambiar "banfield" a "lomas de zamora"
    Ciudad = str_replace_all(Ciudad, regex("(?i)batan"), "general pueyrredon"), # Cambiar "batan" a "general pueyrredon"
    Ciudad = str_replace_all(Ciudad, regex("(?i)bella vista"), "san miguel"), # Cambiar "bella vista" a "san miguel"
    Ciudad = str_replace_all(Ciudad, regex("(?i)beccar"), "san isidro"), # Cambiar "beccar" a "san isidro"
    Ciudad = str_replace_all(Ciudad, regex("(?i)benavidez"), "tigre"), # Cambiar "benavidez" a "tigre"
    Ciudad = str_replace_all(Ciudad, regex("(?i)bernal"), "quilmes"), # Cambiar "bernal" a "quilmes"
    Ciudad = str_replace_all(Ciudad, regex("(?i)burzaco"), "almirante brown"), # Cambiar "burzaco" a "almirante brown"
    Ciudad = str_replace_all(Ciudad, regex("(?i)carmen de patagones"), "patagones"), # Cambiar "carmen de patagones" a "patagones"
    Ciudad = str_replace_all(Ciudad, regex("(?i)dock sud"), "avellaneda"), # Cambiar "dock sud" a "avellaneda"
    Ciudad = str_replace_all(Ciudad, regex("(?i)el jaguel"), "esteban echeverria"), # Cambiar "el jaguel" a "esteban echeverria"
    Ciudad = str_replace_all(Ciudad, regex("(?i)guernica"), "presidente peron"), # Cambiar "guernica" a "presidente peron"
    Ciudad = str_replace_all(Ciudad, regex("(?i)lavallol"), "lomas de zamora"), # Cambiar "lavallol" a "lomas de zamora"
    Ciudad = str_replace_all(Ciudad, regex("(?i)mar del plata"), "general pueyrredon"), # Cambiar "mar del plata" a "general pueyrredon"
    Ciudad = str_replace_all(Ciudad, regex("(?i)martinez"), "san isidro"), # Cambiar "martinez" a "san isidro"
    Ciudad = str_replace_all(Ciudad, regex("(?i)miramar"), "general alvarado"), # Cambiar "miramar" a "general alvarado"
    Ciudad = str_replace_all(Ciudad, regex("(?i)paso del rey"), "moreno"), # Cambiar "paso del rey" a "moreno"
    Ciudad = str_replace_all(Ciudad, regex("(?i)pontevedra"), "merlo"), # Cambiar "pontevedra" a "merlo"
    Ciudad = str_replace_all(Ciudad, regex("(?i)san nicolas de los arroyo"), "san nicolas"), # Cambiar "san nicolas de los arroyo" a "san nicolas"
    Ciudad = str_replace_all(Ciudad, regex("(?i)lomas de zamora temperley"), "lomas de zamora"), # Cambiar "lomas de zamora temperley" a "lomas de zamora"
    Ciudad = str_replace_all(Ciudad, regex("(?i)tristan suarez"), "ezeiza"), # Cambiar "tristan suarez" a "ezeiza"
    Ciudad = str_replace_all(Ciudad, regex("(?i)wilde"), "avellaneda"), # Cambiar "wilde" a "avellaneda",
    Ciudad = str_replace_all(Ciudad, regex("(?i)monte grande"), "esteban echeverria"),
    Ciudad = str_remove_all(Ciudad, "-"), # Borrar guiones
    Ciudad = str_trim(Ciudad) # Sacar espacios al principio y al final nuevamente
  )


pedidos_ba <- matrizba %>% 
  group_by(Ciudad) %>% 
  summarise(pedidos = n()) %>% 
  arrange(-pedidos) %>% 
  rename("departamen" = Ciudad) %>% 
  mutate(indice = row_number()) %>%
  select(indice, departamen, pedidos) 


result <- left_join(deptos_BA,pedidos_ba, by = "departamen")

#localidades_ba<-  localidades_ba%>%
#  left_join(codigosbsas, by="link")

kbl(pedidos_ba) %>% 
  kable_styling(full_width = T) %>%
  scroll_box(width = "300px", height = "400px", fixed_thead = TRUE)


result <- result %>% 
  mutate(indice = as.character(indice))  # Convertir el índice a carácter para etiquetar

prov_mapa<-ggplot(data = result) +
  geom_sf(aes(fill = pedidos)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), trans = "log", breaks = c(0,1,2, 3, 10, 20, 50, 100, 120)) + 
  geom_sf_text(aes(label = indice), size = 3, color = "black") +  # Añadir el índice sobre cada geometría
  labs(title = "PEDIDOS POR PARTIDO",
       subtitle = "Provincia de Buenos Aires",
       fill = "Pedidos",
       caption = "") +
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

prov_mapa

COSTO_TIO<- final %>% filter(envio_medio=="Moto mensajería (CABA)") %>% 
  summarise(
    total_de_envios=n(),
    sueldo_tio = sum(envio_costo)
  )

## levanto el excel de contabilidad para crear las tablas relacionadas
SUELDOS<-read_xlsx("tablas_contables/CONTABILIDAD_CARGA.xlsx",2)
COSTURA<-read_xlsx("tablas_contables/CONTABILIDAD_CARGA.xlsx",3)
COSTOS_FIJOS<-read_xlsx("tablas_contables/CONTABILIDAD_CARGA.xlsx",4)
BORDADO_UNITARIO<-read_xlsx("tablas_contables/CONTABILIDAD_CARGA.xlsx",5) %>% 
  filter(concepto=="BORDADO") %>% 
  pull(julio)

PACKING_UNITARIO<-read_xlsx("tablas_contables/CONTABILIDAD_CARGA.xlsx",5) %>%
  filter(concepto=="PACKING") %>% 
  pull(julio)

MATRICES<-read_xlsx("tablas_contables/CONTABILIDAD_CARGA.xlsx",5) %>%
  filter(concepto=="MATRICES") %>% 
  pull(julio)

##  Se totalizan el gasto en sueldos fijos para la tanda
SUELDOS<-SUELDOS%>%
summarise(total_sueldos_fijos = sum(julio +agosto))

COSTOS_FIJOS<-COSTOS_FIJOS %>% 
  summarise(total_costo_fijo = sum(julio +agosto))

COSTURA<-final %>%    #PASO DE TENER UNA TABLA DE COSTURA CON COSTOS UNITARIOS A CRUZARLO CON TODA LA TABLA, LUEGO PISO LA TABLA COSTURA
  group_by(capybol) %>% 
  summarise(cantidad=sum(unidades)) %>% 
  left_join(COSTURA, by="capybol") %>% 
  mutate(costo_costura=julio*cantidad)

MATRICES<-final %>% 
  filter(tipo=="PERSO") %>% 
  summarise(costo_matrices=sum(unidades)*MATRICES)

costo_tela<- sum(calcu_completa$costoF)

costo_packing<-sum(final$unidades)*PACKING_UNITARIO

costo_bordado<-sum(final$unidades)*BORDADO_UNITARIO


kbl(COSTURA, caption = "Tabla de COSTURA") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "left", 
                font_size = 12) %>%
  column_spec(1:ncol(COSTURA), width = "100px") %>%
  add_header_above(c(" " = 1, "Información" = ncol(COSTURA) - 1))

COSTURA<-COSTURA %>% 
  summarise(costo_costura=sum(costo_costura))



JULIO_BALANCE <- final %>%         ##tabla en formato horizontal
  summarise(ingresos = sum(Total, na.rm = T),
            correoarg=sum(envio_costo, na.rm = T),
            motomensajeria = sum(na.omit(envio_costo[envio_medio == "Moto mensajería (CABA)"]))) %>% 
  cbind(MATRICES,COSTURA,COSTOS_FIJOS,SUELDOS) %>% 
  mutate(costo_tela=costo_tela,
         costo_bordado=costo_bordado,
         costo_packing=costo_packing,
         costos_totales=costo_packing+costo_bordado+costo_tela+total_costo_fijo+total_sueldos_fijos+costo_matrices+motomensajeria+correoarg+costo_costura,
         resultado_primario=ingresos-costos_totales)



JULIO_BALANCE<- JULIO_BALANCE %>%   ##tabla en formato vertical
  pivot_longer(
    cols = everything(),
    names_to = "cuenta",
    values_to = "julio"
  ) %>% 
  mutate(porcent_jul= round((julio/julio[1])*100,1))

kbl(JULIO_BALANCE) %>% 
  kable_material() %>% 
  row_spec(1, bold = T, color = "#4D4D4D", background = "#77DD77") %>% 
  row_spec(11, bold = T, color = "#4D4D4D", background = "#FF6961") %>%
  row_spec(12, bold = T, color = "#4D4D4D", background = "#B3D4FC")


torta_gral<-JULIO_BALANCE %>%
  filter(!cuenta %in%c("costos_totales","ingresos"))%>%
  mutate(cuenta = factor(cuenta, levels = cuenta[order(porcent_jul)]))%>%
  ggplot(aes(x ="", y = porcent_jul, fill = cuenta))+ 
  geom_bar(stat ="identity", width =0.5)+# Ajustar el ancho de la barra
  geom_text(aes(label = paste0(round(porcent_jul,1),"%")), 
            position = position_stack(vjust =0.5), 
            color ="black")+ 
  scale_fill_brewer(palette ="Set3")+ 
  labs(y ="Porcentaje del ingreso", 
       x ="", 
       fill ="Cuenta", 
       title ="Porcentaje del ingreso por cuenta")+ 
  theme_minimal()+ 
  theme(
    panel.background = element_rect(fill ="white", color ="white"),# Fondo blanco
    plot.background = element_rect(fill ="white", color ="white"),# Fondo del gráfico blanco
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(family ="Helvetica", face ="bold", size =16, hjust =0.5),# Cambiar tipografía del título del gráfico
    axis.title.y = element_text(family ="Helvetica", face ="bold", size =14),# Cambiar tipografía del título del eje Y
    legend.title = element_text(family ="Helvetica", face ="bold", size =14))# Cambiar tipografía del título de la leyenda

torta_gral

costos_totales <- JULIO_BALANCE %>%
  filter(cuenta =="costos_totales")%>%
  pull(julio)





pct_del_costo<-JULIO_BALANCE %>%
  filter(!cuenta %in%c("costos_totales","resultado_primario","ingresos"))%>%
  mutate(porcentaje =(julio/ costos_totales)*100,
         cuenta = factor(cuenta, levels = cuenta[order(porcentaje)]))%>%
  ggplot(aes(x ="", y = porcentaje, fill = cuenta))+ 
  geom_bar(stat ="identity", width =0.5)+# Ajustar el ancho de la barra
  geom_text(aes(label = paste0(round(porcentaje,1),"%")), 
            position = position_stack(vjust =0.5), 
            color ="black")+ 
  scale_fill_brewer(palette ="Set3")+ 
  labs(y ="Porcentaje del costo total", 
       x ="", 
       fill ="Cuenta", 
       title ="Porcentaje del costo total por cuenta")+ 
  theme_minimal()+ 
  theme(
    panel.background = element_rect(fill ="white", color ="white"),# Fondo blanco
    plot.background = element_rect(fill ="white", color ="white"),# Fondo del gráfico blanco
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(family ="Helvetica", face ="bold", size =16, hjust =0.5),# Cambiar tipografía del título del gráfico
    axis.title.y = element_text(family ="Helvetica", face ="bold", size =14),# Cambiar tipografía del título del eje Y
    legend.title = element_text(family ="Helvetica", face ="bold", size =14))# Cambiar tipografía del título de la leyenda

pct_del_costo

#este es el JULIO_COSTOFINO basado en numeros reales de esta tanda

JULIO_COSTOFINO<- final %>%         ##tabla en formato horizontal
  summarise(ingresos = sum(Total, na.rm = T)-COSTO_TIO$sueldo_tio-sum(final$envio_costo, na.rm = T)-MATRICES$costo_matrices) %>% 
  cbind(COSTURA,COSTOS_FIJOS,SUELDOS) %>% 
  mutate(costo_tela=costo_tela,
         costo_bordado=costo_bordado,
         costo_packing=costo_packing,
         costos_totales_BUZO=costo_packing+costo_bordado+costo_tela+total_costo_fijo+total_sueldos_fijos+costo_costura,
         resultado_primario=ingresos-costos_totales_BUZO)

JULIO_COSTOFINO<- JULIO_COSTOFINO %>%   ##tabla en formato vertical
  pivot_longer(
    cols = everything(),
    names_to = "cuenta",
    values_to = "julio"
  ) %>% 
  mutate(porcent_jul= round((julio/julio[1])*100,1))


JULIO_COSTOFINO<-JULIO_COSTOFINO %>% 
  mutate(unitario=round(julio/sum(final$unidades)))



kbl(JULIO_COSTOFINO) %>% 
  kable_material() %>% 
  row_spec(1, 
           bold = TRUE, 
           color = "#4D4D4D", 
           background = "#77DD77", 
           extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") %>% 
  row_spec(8, 
           bold = TRUE, 
           color = "#4D4D4D", 
           background = "#FF6961", 
           extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") %>% 
  row_spec(9, 
           bold = TRUE, 
           color = "#4D4D4D", 
           background = "#B3D4FC", 
           extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;") %>%
  row_spec(2:7, background = "#FFFDD0")


torta_gralPURA<-JULIO_COSTOFINO %>%
  filter(!cuenta %in%c("costos_totales_BUZO","ingresos"))%>%
  mutate(cuenta = factor(cuenta, levels = cuenta[order(porcent_jul)]))%>%
  ggplot(aes(x ="", y = porcent_jul, fill = cuenta))+ 
  geom_bar(stat ="identity", width =0.5)+# Ajustar el ancho de la barra
  geom_text(aes(label = paste0(round(porcent_jul,1),"%")), 
            position = position_stack(vjust =0.5), 
            color ="black")+ 
  scale_fill_brewer(palette ="Set3")+ 
  labs(y ="Porcentaje del ingreso", 
       x ="", 
       fill ="Cuenta", 
       title ="Porcentaje del ingreso PURO por cuenta")+ 
  theme_minimal()+ 
  theme(
    panel.background = element_rect(fill ="white", color ="white"),# Fondo blanco
    plot.background = element_rect(fill ="white", color ="white"),# Fondo del gráfico blanco
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(family ="Helvetica", face ="bold", size =16, hjust =0.5),# Cambiar tipografía del título del gráfico
    axis.title.y = element_text(family ="Helvetica", face ="bold", size =14),# Cambiar tipografía del título del eje Y
    legend.title = element_text(family ="Helvetica", face ="bold", size =14))# Cambiar tipografía del título de la leyenda

torta_gralPURA

costos_totalesPUROS <- JULIO_COSTOFINO %>%
  filter(cuenta =="costos_totales_BUZO")%>%
  pull(julio)


pct_del_costoPURO<-JULIO_COSTOFINO %>%
  filter(!cuenta %in%c("costos_totales_BUZO","resultado_primario","ingresos"))%>%
  mutate(porcentaje =(julio/ costos_totalesPUROS)*100,
         cuenta = factor(cuenta, levels = cuenta[order(porcentaje)]))%>%
  ggplot(aes(x ="", y = porcentaje, fill = cuenta))+ 
  geom_bar(stat ="identity", width =0.5)+# Ajustar el ancho de la barra
  geom_text(aes(label = paste0(round(porcentaje,1),"%")), 
            position = position_stack(vjust =0.5), 
            color ="black")+ 
  scale_fill_brewer(palette ="Set3")+ 
  labs(y ="Porcentaje del costo total PURO", 
       x ="", 
       fill ="Cuenta", 
       title ="Porcentaje del costo total por cuenta")+ 
  theme_minimal()+ 
  theme(
    panel.background = element_rect(fill ="white", color ="white"),# Fondo blanco
    plot.background = element_rect(fill ="white", color ="white"),# Fondo del gráfico blanco
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(family ="Helvetica", face ="bold", size =16, hjust =0.5),# Cambiar tipografía del título del gráfico
    axis.title.y = element_text(family ="Helvetica", face ="bold", size =14),# Cambiar tipografía del título del eje Y
    legend.title = element_text(family ="Helvetica", face ="bold", size =14))# Cambiar tipografía del título de la leyenda

pct_del_costoPURO

##CHUNK UTILIZADO PARA ACTUALIZAR R SCRIPT Y GUARDAR OUTPUTS

#knitr::purl(input = "INFORME_JULIO.Rmd", output = "INFORME_JULIO_CODE.R",documentation = 0) segmento que va en R para hacer la conversion, nunca en rmd

#ggsave("outputs/julio/torta_gral.png",torta_gral)
#ggsave("outputs/julio/pct_del_costo.png",pct_del_costo)
#ggsave("outputs/julio/ranking_uni.png",ranking_uni_plot)
#ggsave("outputs/julio/provincia.png",prov_mapa)
#ggsave("outputs/julio/argentina.png",mapa_combinado)


#png("outputs/julio/treemap_uni.png", width = 1000, height = 800, res = 300)
#treemap(ranking_uni,
      #      index="universidad",
       #     vSize="cantidad",
        #    type="index",
         #   palette = "Paired",
          #  title= "Grafico Treemap de universidades")
#dev.off()

#write.xlsx(JULIO_BALANCE,"outputs/julio/BALANCE_JULIO.xlsx")
#write.xlsx(calcu_completa,"outputs/julio/calcu_completa.xlsx")
#write.xlsx(nueva_tabla,"outputs/julio/lista_produ.xlsx")
#write.xlsx(produ_HP,"outputs/julio/produ_hp.xlsx")

