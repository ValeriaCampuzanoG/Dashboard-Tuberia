options(scipen = 999)
#options(repos = c(CRAN = "https://cran.r-project.org/"))


# CAPACIDAD #

#install.packages("ggiraph")

# Librerias 
library(tidyverse)
library(sf)
library(readxl)
library(ggrepel)
library(DT)
library(plotly)
library(scales)
library(leaflet)
library(reactable)
library(paletteer)
library(fishualize)
library(htmltools)
library(bslib)
library(fresh)
library(sysfonts)
library(showtext)
library(ggplot2)
library(ggiraph)




theme_hgz <- create_theme(
  bs4dash_color(
    fuchsia = "#8B3058FF",
    purple = "#541F3FFF",
    maroon = "#D39C83FF",
    red = "#AD466CFF",
    green = "#C1766FFF"
  ),
  bs4dash_status(
    primary = "#8B3058FF",
    secondary = "#A65461FF",
    success = "#FBE6C5FF",
    info = "#7BBCB0FF",
    warning = "#D39C83FF"
  )
)


# Importar datos
bd_tub2024 <- read_excel("www/bd/bd_tuberia_2024.xlsx")
bd_tub2024 <- bd_tub2024[-c(1), ]

# Lista de entidades
lista_entidades <- unique(bd_tub2024$entidad)


#Obtener porcentajes

bd_tub2024 <- bd_tub2024 %>%
  mutate( 
    mutate(across(-c(1:3), as.numeric))
    ) %>%
  mutate(
  pct_ci_con_det = round(con_detenido / carpetas_investigacion * 100, 2),
  pct_ci_sin_det = round(sin_detenido / carpetas_investigacion  * 100,2 ),
  pct_ci_mixta =  round(mixto / carpetas_investigacion * 100, 2),
  ci_gestionadas = round( carpetas_investigacion + rezago_ano_anterior, 2) ,
  pct_ci_iniciadas = round( carpetas_investigacion / ci_gestionadas * 100 ,2),
  pct_ci_rezago = round( rezago_ano_anterior/ci_gestionadas * 100 ,2), 
  causas_gestionadas = round( causas_ingresadas + causas_rezago_anterior, 2) ,
  pct_causas_iniciadas =round( causas_ingresadas /  causas_gestionadas * 100, 2) ,
  pct_causas_rezago = round( causas_rezago_anterior / causas_gestionadas * 100, 2) ,
  llamadas_emergencia = round( llamadas_911 + llamadas_089 , 2) , 
  pct_911 = round( llamadas_911 / llamadas_emergencia * 100 , 2) ,
  pct_089 = round( llamadas_089 / llamadas_emergencia * 100 , 2) , 
  # pct_acuerdos_sm = round(,2), 
  # pct_susp_sm = round(,2) ,
  pct_acuerdos_sj = round( acuerdo_reparatorio_cumplido / justicia_alternativa * 100 , 2), 
  pct_susp_sj = round( suspension_condicional_cumplida / justicia_alternativa * 100 , 2)
)



# Gráfica de determinaciones 


gen_barra_determinaciones <- function(ent_sel) {
  


determinaciones <- bd_tub2024 %>%
  select( cve, entidad, facultad_de_asbtenrse_a_investigar, criterio_oportunidad, no_ejercicio_acción_penal, archivo_temporal,
          incompetencia, acumulacion, otras_determinaciones) %>%
  filter( entidad == ent_sel) %>%
  pivot_longer(
    cols = c("facultad_de_asbtenrse_a_investigar", "criterio_oportunidad", "no_ejercicio_acción_penal", "archivo_temporal",
             "incompetencia", "acumulacion", "otras_determinaciones"),  
    names_to = "determinaciones",
    values_to = "total"
  ) %>%
  arrange(total) %>%
  ggplot(aes(x = entidad, y = total, fill = determinaciones)) +
  
  geom_col() +
  coord_flip() + 
  labs(
    title = "Distribución de determinaciones",
    x = "",
    y = "",
    fill = "Determinaciones"
  ) +
  theme_minimal()

determinaciones
}

gen_barra_determinaciones("Tlaxcala")