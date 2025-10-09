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


# Paleta de colores

color_scale <- rev(as.character(
  paletteer::paletteer_c("grDevices::BrwnYl", 30)
))


# Importar datos
bd_tub2024 <- read_excel("www/bd/bd_tuberia_2024.xlsx")
bd_tub2024 <- bd_tub2024[-c(1), ]


bd_tuberia_wide <- read_excel("www/bd/bd_tuberia_wide.xlsx", 
                              sheet = "bd_wide")

bd_tuberia <- bd_tuberia_wide %>%
  mutate(across(4:40, as.numeric)) %>%
  pivot_longer(
    cols = 4:40, 
    names_to = "variable",
    values_to = "total"
    ) %>%
  left_join(diccionario_tuberia, by = "variable") %>%
  mutate( cod_indicador = paste0( num_variable, " - ", nom_variable))




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

bd_gra_det <- bd_tub2024 %>%
  select( cve, entidad, facultad_de_asbtenrse_a_investigar, criterio_oportunidad, no_ejercicio_acción_penal, archivo_temporal,
          incompetencia, acumulacion, otras_determinaciones) %>%
  pivot_longer(
    cols = c("facultad_de_asbtenrse_a_investigar", "criterio_oportunidad", "no_ejercicio_acción_penal", "archivo_temporal",
             "incompetencia", "acumulacion", "otras_determinaciones"),  
    names_to = "determinaciones",
    values_to = "total"
  ) %>%
  filter(!is.na(total))  %>%
  group_by(entidad ) %>%
  mutate(
    pct = round((total / sum(total, na.rm = TRUE)) * 100, 2))  %>%
  mutate(
    nom_determinaciones = case_when(
      determinaciones == "acumulacion" ~ "Acumulación",
      determinaciones == "archivo_temporal" ~ "Archivo temporal",
      determinaciones == "criterio_oportunidad" ~ "Criterio de oportunidad",
      determinaciones == "facultad_de_asbtenrse_a_investigar" ~ "Facultad de abstenerse a investigar",
      determinaciones == "incompetencia" ~ "Incompetencia",
      determinaciones == "no_ejercicio_acción_penal" ~ "No ejercicio de la acción penal",
      determinaciones == "otras_determinaciones" ~ "Otras",
      TRUE ~ NA_character_ # para los casos que no cumplan ninguna condición
    )
  ) %>%
  arrange(total) 



gen_barra_determinaciones <- function(ent_sel) {
  


g <- bd_gra_det %>%
  # select( cve, entidad, facultad_de_asbtenrse_a_investigar, criterio_oportunidad, no_ejercicio_acción_penal, archivo_temporal,
  #         incompetencia, acumulacion, otras_determinaciones) %>%
  # pivot_longer(
  #   cols = c("facultad_de_asbtenrse_a_investigar", "criterio_oportunidad", "no_ejercicio_acción_penal", "archivo_temporal",
  #            "incompetencia", "acumulacion", "otras_determinaciones"),  
  #   names_to = "determinaciones",
  #   values_to = "total"
  # ) %>% 
  # mutate(
  #   pct = (total / sum(total, na.rm = TRUE)) * 100
  #   ) %>%
  # arrange(total) %>%
  # filter(!is.na(pct)) %>%
  filter( entidad == ent_sel, 
          !(pct == 0)) %>%
  ggplot(
    aes(x = reorder(nom_determinaciones, total), 
        y = total, 
        fill = total,
        tooltip = paste0("<span style='font-size:20px;'><b>", nom_determinaciones, ": ", "</b></span><br>",
                         "<span style='font-size:20px;'>", prettyNum(round(total, 2), big.mark = ","),
                         "<span style='font-size:16px;'>","   (",round(pct, 2), "%)","</span>"),
        data_id = entidad
    )) +
  ggiraph::geom_col_interactive() +
  ggiraph::geom_text_interactive(aes(label = prettyNum(round(total,2), big.mark = ",")), hjust = 0, colour = "#535353", size = 12) +
  coord_flip() +
  labs(
    #title = "none",
    x = "",
    y = "",
    fill = "Determinaciones"
  ) +
  scale_fill_gradientn(colors = color_scale) +
  #scale_fill_gradient(low = "#D39C83", high = "#813753") +
  scale_x_discrete(labels = label_wrap(20)) +
  scale_y_continuous(expand = expansion(c(0, 0.1)), labels = comma_format()) +
  theme_bw() +
  theme(
    axis.title = element_text(family = "Montserrat", size = 9),
    plot.subtitle = element_text(family = "Montserrat", 
                                 size = 25, 
                                 colour = "#636363",
                                 margin = margin(b = 30, unit = "pt")),
    plot.title = element_text(family = "Montserrat",
                              face = "bold",
                              size = 35,
                              hjust = 0,
                              margin = margin(b = 2, unit = "pt")),
    axis.text = element_text(size = 15), 
    #plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none" #,
    #axis.ticks.length = unit(0.2, "cm")
    #plot.margin = margin(60, 20, 20, 20, "pt")
  ) 



ggiraph::girafe(ggobj = g, 
                width_svg = 16, 
                height_svg =12,
                pointsize = 12,
                options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                               opts_hover(css = ''),
                               opts_hover_inv(css = "opacity:0.1;"),
                               opts_selection(type = "none"),
                               opts_toolbar(saveaspng = FALSE)))


}

gen_barra_determinaciones("Veracruz")


# Gráfica de sentencias 

bd_gra_sent <- bd_tub2024 %>%
  select( cve, entidad, procedimiento_abreviado_condenatoria, sentencia_jo_condenatoria, 
          sentencia_jo_absolutoria, sentencia_jo_mixta) %>%
  pivot_longer(
    cols = c("procedimiento_abreviado_condenatoria", "sentencia_jo_condenatoria", "sentencia_jo_absolutoria", 
             "sentencia_jo_mixta"),  
    names_to = "sentencias",
    values_to = "total"
  ) %>%
  filter(!is.na(total))  %>%
  group_by(entidad ) %>%
  mutate(
    pct = round((total / sum(total, na.rm = TRUE)) * 100, 2))  %>%
  mutate(
    nom_sentencia = case_when(
      sentencias == "procedimiento_abreviado_condenatoria" ~ "Procedimiento abreviado - Condenatoria",
      sentencias == "sentencia_jo_condenatoria" ~ "Juicio oral - Condenatoria",
      sentencias == "sentencia_jo_absolutoria" ~ "Juicio oral - Absolutoria",
      sentencias == "sentencia_jo_mixta" ~ "Juicio oral - Mixta",
      TRUE ~ NA_character_ # para los casos que no cumplan ninguna condición
    )
  ) %>%
  arrange(total) 



gen_barra_sentencias <- function(ent_sel) {
  
  
  
  g <- bd_gra_sent %>%
    # select( cve, entidad, facultad_de_asbtenrse_a_investigar, criterio_oportunidad, no_ejercicio_acción_penal, archivo_temporal,
    #         incompetencia, acumulacion, otras_determinaciones) %>%
    # pivot_longer(
    #   cols = c("facultad_de_asbtenrse_a_investigar", "criterio_oportunidad", "no_ejercicio_acción_penal", "archivo_temporal",
    #            "incompetencia", "acumulacion", "otras_determinaciones"),  
    #   names_to = "determinaciones",
    #   values_to = "total"
    # ) %>% 
    # mutate(
    #   pct = (total / sum(total, na.rm = TRUE)) * 100
    #   ) %>%
    # arrange(total) %>%
    # filter(!is.na(pct)) %>%
    filter( entidad == ent_sel , 
            !(pct == 0)) %>%
    ggplot(
      aes(x = reorder(nom_sentencia, total), 
          y = total, 
          fill = total,
          tooltip = paste0("<span style='font-size:20px;'><b>", nom_sentencia, ": ", "</b></span><br>",
                           "<span style='font-size:20px;'>", prettyNum(round(total, 2), big.mark = ","),
                           "<span style='font-size:16px;'>","   (",round(pct, 2), "%)","</span>"),
          data_id = entidad
      )) +
    ggiraph::geom_col_interactive() +
    ggiraph::geom_text_interactive(aes(label = prettyNum(round(total,2), big.mark = ",")), hjust = -.05, colour = "#535353", size = 12) +
    coord_flip() +
    labs(
      #title = "none",
      x = "",
      y = "",
      fill = "Sentencias"
    ) +
    scale_fill_gradientn(colors = color_scale) +
    #scale_fill_gradient(low = "#D39C83", high = "#813753") +
    scale_x_discrete(labels = label_wrap(20)) +
    scale_y_continuous(expand = expansion(c(0, 0.1)), labels = comma_format()) +
    theme_bw() +
    theme(
      axis.title = element_text(family = "Montserrat", size = 9),
      plot.subtitle = element_text(family = "Montserrat", 
                                   size = 25, 
                                   colour = "#636363",
                                   margin = margin(b = 30, unit = "pt")),
      plot.title = element_text(family = "Montserrat",
                                face = "bold",
                                size = 35,
                                hjust = 0,
                                margin = margin(b = 2, unit = "pt")),
      axis.text = element_text(size = 15), 
      #plot.title.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.position = "none" #,
      #axis.ticks.length = unit(0.2, "cm")
      #plot.margin = margin(60, 20, 20, 20, "pt")
    ) 
  
  
  
  ggiraph::girafe(ggobj = g, 
                  width_svg = 16, 
                  height_svg =12,
                  pointsize = 12,
                  options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                 opts_hover(css = ''),
                                 opts_hover_inv(css = "opacity:0.1;"),
                                 opts_selection(type = "none"),
                                 opts_toolbar(saveaspng = FALSE)))
  
  
}

gen_barra_sentencias("Veracruz")





create_mcv_header <- function() {
  tagList(
    # Barra superior con redes sociales
    # div(
    #   class = "mcv-top-bar",
    #   style = "
    #     background-color: #6551D0;
    #     color: white;
    #     padding: 10px 0;
    #     font-size: 12px;
    #     position: relative;
    #     font-family: 'Ubuntu', sans-serif;
    #   ",
    #   div(
    #     class = "container-fluid",
    #     style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
    #     div(
    #       style = "display: flex; justify-content: space-between; align-items: center;",
    #       
    #       # Redes sociales izquierda
    #       div(
    #         style = "display: flex; align-items: center; gap: 15px;",
    #         tags$a(
    #           href = "https://twitter.com/mexicocomovamos", 
    #           style = "color: white; font-size: 18px; text-decoration: none;",
    #           HTML('<i class="fab fa-twitter"></i>')
    #         ),
    #         tags$a(
    #           href = "https://facebook.com/mexicocomovamos", 
    #           style = "color: white; font-size: 18px; text-decoration: none;",
    #           HTML('<i class="fab fa-facebook"></i>')
    #         ),
    #         tags$a(
    #           href = "https://youtube.com/mexicocomovamos", 
    #           style = "color: white; font-size: 18px; text-decoration: none;",
    #           HTML('<i class="fab fa-youtube"></i>')
    #         ),
    #         tags$a(
    #           href = "https://instagram.com/mexicocomovamos", 
    #           style = "color: white; font-size: 18px; text-decoration: none;",
    #           HTML('<i class="fab fa-instagram"></i>')
    #         )
    #       ),
    #       
    #       # Mensaje central
    #       div(
    #         style = "color: white; font-weight: 500; font-family: 'Ubuntu', sans-serif;",
    #         "México, ¿cómo vamos? 🇲🇽"
    #       ),
    #       
    #       # Espacio derecha
    #       div(
    #         style = "width: 150px;"
    #       )
    #     )
    #   )
    # ),
    
    # Barra principal con navegación
    div(
      class = "mcv-main-header",
      style = "
        background-color: white;
        color: #333333;
        padding: 20px 0;
        font-family: 'Montserrat', sans-serif;
        border-bottom: 1px solid #e9ecef;
      ",
      div(
        class = "container-fluid",
        style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          
          # Navegación izquierda
          div(
            style = "display: flex; align-items: center; gap: 30px; flex: 1;",
            
            tags$a(
              #href = "https://mexicocomovamos.mx/equipo/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Montserrat', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid  #813753'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "Hallazgos",
            ),
            
            tags$a(
              #href = "https://mexicocomovamos.mx/indice-de-progreso-social/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Montserrat', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid  #813753'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "Índice de",
              br(),
              "Capacidad"
            )
          ),
          
          # Logo central
          div(
            style = "flex: 0 0 auto; margin: 0 20px;",
            tags$a(
              href = "https://www.mexicoevalua.org/hallazgos-2023/",
              tags$img(
                src = "https://www.mexicoevalua.org/wp-content/uploads/2023/06/mxe-logotipoprincipal-05.png",
                alt = "México Evalúa",
                style = "height: 60px; width: auto;"
              )
            )
          ),
          
          # Navegación derecha
          div(
            style = "display: flex; align-items: center; gap: 20px; flex: 1; justify-content: flex-end;",
            
            tags$a(
              #href = "https://mexicocomovamos.mx/fichas-por-estado/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Montserrat', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid  #813753'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "Indicador de",
              br(),
              "Impunidad"
            ),
            
            tags$a(
              #href = "https://mexicocomovamos.mx/categoria/publicaciones/",
              style = "
                color:  #813753;
                text-decoration: none;
                font-size: 12px;
                font-weight: 700;
                padding: 8px 0;
                border-bottom: 2px solid  #813753;
                font-family: 'Montserrat', sans-serif;
              ", 
              "Tubería",
              br(),
              "Procesal"
            )
          )
        )
      )
    )
  )
}



# Gráfica de barras Tubería Procesal -----



opciones_tuberia <- unique(bd_tuberia$cod_indicador)

#ind_sel== "índice de impunidad"

gen_barras_tub <- function(ind_sel, ano_sel_imp){
  
  datos_sel <- bd_tuberia %>%
    filter(cod_indicador %in% opciones_tuberia) %>%
    filter(cod_indicador == ind_sel, 
           ano == ano_sel_imp,
           !entidad == "Nacional") %>% 
    filter(!is.na(total)) 
  
  g <- ggplot(datos_sel, 
              aes(x = reorder(entidad, -total), 
                  y = total, 
                  fill = total,
                  tooltip = paste0("<span style='font-size:20px;'><b>", entidad, ", ", ano, "</b></span><br>",
                                   "<span style='font-size:16px;'>", prettyNum(round(total, 2), big.mark = ","), "</span>"),
                  data_id = entidad)) +
    ggiraph::geom_col_interactive() +
    ggiraph::geom_text_interactive(aes(label = prettyNum(round(total,2), big.mark = ",")), hjust = -0.5, colour = "#535353", size = 3) +
    coord_flip() +
    scale_fill_gradientn(colors = color_scale) +
    #scale_fill_gradient(low = "#D39C83", high = "#813753") +
    scale_y_continuous(expand = expansion(c(0, 0.1)), labels = comma_format()) +
    labs(x = "Entidad", 
         y = "",
         title = paste0(datos_sel$nom_variable), 
         subtitle = paste0(datos_sel$ano),
         caption = "@mexeval | Elaboración propia con base en el CNIJE, CNPJE, CNSE."
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(family = "Montserrat", size = 9),
      plot.subtitle = element_text(family = "Montserrat", 
                                   size = 15, 
                                   colour = "#636363",
                                   margin = margin(b = 10, unit = "pt")),
      plot.title = element_text(family = "Montserrat",
                                face = "bold",
                                size = 20,
                                hjust = 0,
                                margin = margin(b = 10, unit = "pt")),
      plot.caption = element_text(family = "Montserrat"), 
      #axis.text = element_text(family = "Montserrat"), 
      #plot.title.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.position = "none" #,
      #axis.ticks.length = unit(0.2, "cm")
      #plot.margin = margin(60, 20, 20, 20, "pt")
    ) 
  
  
  ggiraph::girafe(ggobj = g, 
                  width_svg = 10, 
                  height_svg =6,
                  pointsize = 12,
                  options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                 opts_hover(css = ''),
                                 opts_hover_inv(css = "opacity:0.1;"),
                                 opts_selection(type = "none"),
                                 opts_toolbar(saveaspng = T)))
  
}

gen_barras_tub("14 - Archivo temporal", "2022")


# Gráfica de lineas
gen_lineas_tub <- function(ind_sel, entidades_resaltadas){
  
  datos_sel <- bd_tuberia %>%
    #filter(nom_indicador %in% opciones_impunidad) %>%
    filter(cod_indicador == ind_sel, 
           !entidad == "Nacional") 
  
  
  datos_sel <- datos_sel %>%
    mutate(
      is_selected = entidad %in% entidades_resaltadas,
      line_color  = ifelse(is_selected, "#C1766F", "grey80"),
      point_color = ifelse(is_selected, "#541F3F", "grey75"),
      line_alpha  = ifelse(is_selected, 1, 0.1)
    )
  
  
  datos_labels <- datos_sel %>%
    filter(is_selected) %>%
    group_by(entidad) %>%
    slice_max(ano, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  g <- datos_sel %>% 
    ggplot(aes(x = ano, 
               y = total, 
               group = entidad)) +
    geom_line_interactive(
      aes(color = I(line_color), 
          alpha = line_alpha,
          tooltip = paste0(
            "<span style='font-size:24px;'><b>", entidad, ", " ,ano,  "</b></span><br><br>",
            "<span style='font-size:18px;'>", prettyNum(round(total, 2), big.mark = ",")
          ), 
          data_id = entidad),
      size = 0.3,    
      show.legend = FALSE
    )  +
    geom_point_interactive(aes(color = I(point_color),
                               alpha = line_alpha,
                               tooltip = if(ind_sel == "Índice de impunidad"){
                                 paste0(
                                   "<span style='font-size:24px;'><b>", entidad, " " ,ano,  "</b></span><br><br>",
                                   "<span style='font-size:18px;'>", prettyNum(round(total, 2), big.mark = ","), " %") 
                               } else {
                                 paste0(
                                   "<span style='font-size:24px;'><b>", entidad, " " ,ano,  "</b></span><br><br>",
                                   "<span style='font-size:18px;'>", prettyNum(round(total, 2), big.mark = ",")) 
                               },
                               data_id = entidad),
                           size = 2, show.legend = FALSE) +
    scale_alpha_identity() +
    geom_text_interactive(data = datos_labels, 
                          aes(x = ano, y = total, label = entidad,
                              tooltip = entidad, 
                              data_id = entidad),
                          hjust = -0.1, vjust = 0.5, size = 3, color = "#541F3F") +
    labs(
      title = paste0(datos_sel$nom_variable), 
      x = "Año",
      y = "", 
      caption = "@mexeval | Elaboración propia con base en el CNIJE, CNPJE, CNSE."
    ) +
    scale_y_continuous(
      labels = comma_format()
    ) + 
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.15))) +
    theme_bw() +
    theme(
      axis.title = element_text(family = "Montserrat", size = 9),
      plot.subtitle = element_text(family = "Montserrat", 
                                   size = 15, 
                                   colour = "#636363",
                                   margin = margin(b = 10, unit = "pt")),
      plot.title = element_text(family = "Montserrat",
                                face = "bold",
                                size = 20,
                                hjust = 0,
                                margin = margin(b = 20, unit = "pt")),
      plot.caption = element_text(family = "Montserrat"), 
      #axis.text = element_text(family = "Montserrat"), 
      #plot.title.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.position = "none" #,
      #axis.ticks.length = unit(0.2, "cm")
      #plot.margin = margin(60, 20, 20, 20, "pt")
    ) 
    
  
  
  ggiraph::girafe(ggobj = g, 
                  width_svg = 10, 
                  height_svg =6,
                  pointsize = 12,
                  options = list(opts_tooltip(css = "background-color:#d9d9d9;color:black;padding:5px;border-radius:3px;opacity:0.9"),
                                 opts_hover(css = ''),
                                 opts_hover_inv(css = "opacity:0.1;"),
                                 opts_selection(type = "none"),
                                 opts_toolbar(saveaspng = T)))
  
  
  
}

gen_lineas_tub("04 - Carpetas de investigación iniciadas", "Oaxaca")


