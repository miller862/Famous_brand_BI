## Este script se corre luego de todo el script general
 
#parametros fijos:
c_tela<-11500
c_rib<-11360
lleva_tela<-0.7
lleva_rib<-0.2
c_packing<-400
c_bordado<-6000
c_costura_maximo<-1700

#correr esto para 1000 unidades
q_hipotetica<-1000
kg_tela<-q_hipotetica*lleva_tela

#correr esto para 1200 unidades
q_hipotetica<-1200
kg_tela<-q_hipotetica*lleva_tela

#luego calcular el costo final en tela para los valores de procuccion deseados
c_tela_h<-c_tela*(kg_tela)+c_rib*(kg_tela*lleva_rib)
c_tela_h



JULIO_COSTOFINO<- final %>%         ##tabla en formato horizontal
  summarise(ingresos = sum(Total, na.rm = T)-COSTO_TIO$sueldo_tio-sum(final$envio_costo, na.rm = T)-MATRICES$costo_matrices) %>% 
  cbind(COSTURA,COSTOS_FIJOS,SUELDOS) %>% 
  mutate(costo_tela=costo_tela,
         costo_bordado=costo_bordado,
         costo_packing=costo_packing,
         costos_totales_BUZO=costo_packing+costo_bordado+costo_tela+total_costo_fijo+total_sueldos_fijos+costo_costura,
         resultado_primario=ingresos-costos_totales_BUZO)


#este nuevo julio costofino es con valores hipoteticos
JULIO_COSTOFINO<-JULIO_COSTOFINO %>% 
  mutate(costo_packing=q_hipotetica*c_packing,
         costo_bordado=q_hipotetica*c_bordado,
         costo_tela=c_tela_h,
         costo_costura=q_hipotetica*c_costura_maximo,#tome el costo de costura mas caro
         costos_totales_BUZO= costo_bordado+costo_packing+costo_tela+costo_costura+total_costo_fijo+total_sueldos_fijos,
         ingresos=49000000, #este parametro se va cambiando a mano
         resultado_primario=ingresos-costos_totales_BUZO
  )

JULIO_COSTOFINO<- JULIO_COSTOFINO %>%   ##tabla en formato vertical
  pivot_longer(
    cols = everything(),
    names_to = "cuenta",
    values_to = "julio"
  ) %>% 
  mutate(porcent_jul= round((julio/julio[1])*100,1))

JULIO_COSTOFINO<-JULIO_COSTOFINO %>% 
  mutate(unitario=round(julio/q_hipotetica))

JULIO_COSTOFINO 

#tu margen de esta tanda fue 42.6 puro

# 50000 el buzo con 1200 buzos, ingreso 60000000, margen 46.7  OK
# 48508 el buzo con 1200 buzos, ingreso 58210000, margen 45.1  OK
# 47083 el buzo con 1200 buzos, ingreso 56500000, margen 43.4  OK

# 50000 el buzo con 1000 buzos, ingreso 50000000, margen 43.2  OK
# 48500 el buzo con 1000 buzos, ingreso 48500000, margen 41.4  BAJO
# 47000 el buzo con 1000 buzos, ingreso 47000000, margen 39.6  MAL



