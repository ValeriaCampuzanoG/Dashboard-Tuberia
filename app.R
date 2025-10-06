# DASHBOARD CAPACIDAD


#Librerías 

#install.packages("renv")

library(shiny)
library(bs4Dash)
library(plotly)
library(shinycssloaders)
library(reactable)
library(leaflet)
library(fresh)

#source(file = "paquetes-setup.R")
#source(file = "importacion_textos.R")
#source(file = "codigo_preparacion.R")


# UI
ui <- fluidPage(
  
   tags$head(
    includeCSS("estilos.css")
  #   tags$style(HTML())
   ),
  
  div(class = "main-container",
      # Controles al inicio
      div(class = "controls-section",
          fluidRow(
            column(3,
                   div(class = "control-group",
                       tags$label("Seleccione una entidad:", `for` = "sel_entidad"),
                       selectInput("sel_entidad", "",
                                   choices = lista_entidades,
                                   selected = "Nacional") #aquí poner Nacional
                   )),
            column(3,
                   div(class = "control-group",
                       tags$label("Año:", `for` = "ano"),
                       selectInput("ano", "",
                                   choices = ("2024"),
                                   selected = "2024") #aquí poner solo 2024 y que no hay forma de cambiarlo
                   )))),
      
      # Header con información general
      # div(class = "header-section",
      #     div(class = "header-title", "Información general"),
      #     div(class = "metrics-row",
      #         div(class = "metric-card blue",
      #             div(class = "metric-icon", "🏛️"),
      #             div(class = "metric-value", textOutput("total_tlf")),
      #             div(class = "metric-label", "TLF")
      #         ),
      #         div(class = "metric-card green",
      #             div(class = "metric-icon", "⚖️"),
      #             div(class = "metric-value", textOutput("total_jueces")),
      #             div(class = "metric-label", "Jueces")
      #         ),
      #         div(class = "metric-card orange",
      #             div(class = "metric-icon", "🔨"),
      #             div(class = "metric-value", textOutput("total_juezas")),
      #             div(class = "metric-label", "Juezas")
      #         )
      #     )
      # ),
      div(class = "breakdown-cards",
          #Tablita de C.I. iniciadas
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Carpetas de investigación iniciadas"),
              div(class = "flow-main-value", textOutput("ciInicidas")), #Total de C.I. iniciadas
              div(id = "tramite_breakdown",
                  div(class = "breakdown-item",
                      span("Con persona imputada"),
                      span(
                        span(class = "breakdown-value", textOutput("CIimputado", inline = TRUE)), #total con personas imputadas
                        " ",
                        span(class = "breakdown-percentage", textOutput("CIimputadopct", inline = TRUE)) #pct personas imputadas
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Sin persona imputada"),
                      span(
                        span(class = "breakdown-value", textOutput("CISinimputado", inline = TRUE)), #total sin persona imputada
                        " ",
                        span(class = "breakdown-percentage", textOutput("CISinimputadopct", inline = TRUE)) #pct sin persona imputada
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Mixta"),
                      span(
                        span(class = "breakdown-value", textOutput("CImixto", inline = TRUE)), #total ci mixta
                        " ",
                        span(class = "breakdown-percentage", textOutput("CImixtopct", inline = TRUE)) #pct mixta
                      )
                  )
                  
              )
          ),
          
          
          #Tablita de C.I. gestionadas
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Carpetas de investigación gestionadas"),
              div(class = "flow-main-value", textOutput("ciGestionadas")),
              div(id = "tramite_breakdown",
                  div(class = "breakdown-item",
                      span("Iniciadas"),
                      span(
                        span(class = "breakdown-value", textOutput("ciNuevas", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("ciNuevaspct", inline = TRUE))
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Pendientes del año anterior"),
                      span(
                        span(class = "breakdown-value", textOutput("ciRezago", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("ciRezagopct", inline = TRUE))
                      )
                  )
              )
          ),
          
          
          
          #Tablita de causas penales gestionadas 
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Causas penales gestionadas"),
              div(class = "flow-main-value", textOutput("causasGestionadas")),
              div(id = "tramite_breakdown",
                  div(class = "breakdown-item",
                      span("Iniciadas"),
                      span(
                        span(class = "breakdown-value", textOutput("causasNuevas", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("causasRezago", inline = TRUE))
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Pendientes del año anterior"),
                      span(
                        span(class = "breakdown-value", textOutput("causasNuevaspct", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("causasRezagopct", inline = TRUE))
                      )
                  )
              )
          )
      ),
      
      br(),
      
      # Flujo principal
      
      div(class = "flow-container",
          #Llamadas
          div(class = "flow-card purple",
              div(class = "flow-title", "Llamadas de emergencia"),
              div(class = "flow-main-value", textOutput("llamadas")),
              div(class = "flow-detail-item",
                  span("911: "),
                  span(class = "flow-detail-value", textOutput("llamadas911", inline = TRUE))
              ),
              div(class = "flow-detail-item",
                  span("089: "),
                  span(class = "flow-detail-value", textOutput("llamadas089", inline = TRUE))
              )
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          #C.I. gestionadas
          div(class = "flow-card green",
              div(class = "flow-title", "C.I. gestionadas"),
              div(class = "flow-main-value", textOutput("resueltos"))
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          # Canalizadas a MASC
          div(class = "flow-card purple",
              div(class = "flow-title", "Canalizadas a MASC"),
              div(class = "flow-main-value", textOutput("asuntos_ingresados")),
              div(class = "flow-detail-item",
                  span(" Concluidas por acuerdos reparatorios: "),
                  span(class = "flow-detail-value", textOutput("fisicos_display", inline = TRUE))
              )
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          
          # Causas penales
          div(class = "flow-card blue",
              div(class = "flow-title", "Causas penales gestionadas"),
              div(class = "flow-main-value", textOutput("concluidos")) #,
              #div(class = "flow-detail-item", textOutput("concluidos_pct"))
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          # Justicia Alternativa
          div(class = "flow-card purple",
              div(class = "flow-title", "Justicia Alternativa"),
              div(class = "flow-main-value", textOutput("asuntos_ingresados")),
              div(class = "flow-detail-item",
                  span("Acuerdos reparatorios: "),
                  span(class = "flow-detail-value", textOutput("fisicos_display", inline = TRUE))
              ),
              div(class = "flow-detail-item",
                  span("Suspensión condicional: "),
                  span(class = "flow-detail-value", textOutput("electronicos_display", inline = TRUE))
              )   
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          # Sentencias en Juicio Oral
          div(class = "flow-card blue",
              div(class = "flow-title", "Sentencias en Juicio Oral"),
              div(class = "flow-main-value", textOutput("concluidos")),
              div(class = "flow-detail-item", textOutput("concluidos_pct"))
          )
      ),
      
      # Cards de gráficas
      # Gráficas de determinaciones
      div(class = "breakdown-cards",
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Determinaciones"),
              div(style = "height: 100px; text-align: center; line-height: 100px; color: #999;",
                  "Gráfico placeholder")
          ),
          
      # Gráfica de sentencias    
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Sentencias en juicio oral"),
              div(style = "height: 100px; text-align: center; line-height: 100px; color: #999;",
                  "Gráfico placeholder")
          ),
          
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Pendientes al final del año"),
              div(style = "height: 100px; text-align: center; line-height: 100px; color: #999;",
                  "Gráfico placeholder")
          ) 
      )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  updateSelectInput(session, "sel_entidad", 
                    choices = unique(bd_tub2024$entidad), 
                    selected = "Nacional")
  
  
  datos <- reactive({   
    #req(input$filtrar) # espera a que el usuario presione el botón
    
    bd_tub2024 %>% filter(entidad == input$sel_entidad)
  
  })
  
  
  
  # Outputs para tablas de carpetas de investigación abiertas 
  #Totales
  output$ciInicidas <- renderText({ format(datos()$carpetas_investigacion, big.mark = ",") })
  output$CIimputado <- renderText({ format(datos()$con_detenido, big.mark = ",") })
  output$CISinimputado <- renderText({ format(datos()$sin_detenido, big.mark = ",") })
  output$CImixto <- renderText({ format(datos()$mixto, big.mark = ",") })
  #Porcentajes
  output$CIimputadopct <- renderText({ paste0(format(datos()$pct_ci_con_det, big.mark = ","), "%") })
  output$CISinimputadopct <- renderText({ paste0(format(datos()$pct_ci_sin_det, big.mark = ","), "%") })
  output$CImixtopct <- renderText({ paste0(format(datos()$pct_ci_mixta, big.mark = ","), "%") })
  
  
  # Outputs para tabla de carpetas de investigación gestionadas 
  #Totales
  output$ciGestionadas <- renderText({ format(datos()$ci_gestionadas, big.mark = ",") })
  output$ciNuevas <- renderText({ format(datos()$carpetas_investigacion, big.mark = ",") })
  output$ciRezago <- renderText({ format(datos()$rezago_ano_anterior, big.mark = ",") })
  #Porcentaje
  output$ciNuevaspct <- renderText({ paste0(format(datos()$pct_ci_iniciadas, big.mark = ","), "%") })
  output$ciRezagopct <- renderText({ paste0(format(datos()$pct_ci_rezago, big.mark = ","), "%") })
  
  # Outputs para tabla de causas gestionadas 
  #Totales
  output$causasGestionadas <- renderText({ format(datos()$causas_gestionadas, big.mark = ",") })
  output$causasNuevas <- renderText({ format(datos()$causas_ingresadas, big.mark = ",") })
  output$causasRezago <- renderText({ format(datos()$causas_rezago_anterior, big.mark = ",") })
  #Porcentaje
  output$causasNuevaspct <- renderText({ paste0(format(datos()$pct_causas_iniciadas, big.mark = ","), "%") })
  output$causasRezagopct <- renderText({ paste0(format(datos()$pct_causas_rezago, big.mark = ","), "%") })
  
  # Flujo principal
  # Llamadas de emergencia 
  output$llamadas <- renderText({ format(datos()$llamadas_emergencia, big.mark = ",") })
  output$llamadas911 <- renderText({ format(datos()$llamadas_911, big.mark = ",") })
  output$llamadas089 <- renderText({ format(datos()$llamadas_089, big.mark = ",") })
  
  # Carpetas 
  output$llamadas <- renderText({ format(datos()$llamadas_emergencia, big.mark = ",") })
  
  #Canalizadas a MASC
  # Canalizadas
  
  # Concluidas
  
  # Outputs para el flujo principal
  output$asuntos_ingresados <- renderText({ format(datos()$asuntos_ingresados, big.mark = ",") })
  
  output$fisicos_display <- renderText({ 
    paste0(format(datos()$fisicos, big.mark = ","), " (", 
           round(datos()$fisicos / datos()$asuntos_ingresados * 100, 2), "%)")
  })
  
  output$electronicos_display <- renderText({ 
    paste0(format(datos()$electronicos, big.mark = ","), " (", 
           round(datos()$electronicos / datos()$asuntos_ingresados * 100, 2), "%)")
  })
  
  output$resueltos <- renderText({ format(datos()$resueltos, big.mark = ",") })
  
  output$egresos_display <- renderText({ 
    paste0(format(datos()$egresos, big.mark = ","), " (", 
           round(datos()$egresos / datos()$resueltos * 100, 2), "%)")
  })
  
  output$salida_display <- renderText({ 
    paste0(datos()$salida, " (", 
           round(datos()$salida / datos()$resueltos * 100, 2), "%)")
  })
  

}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)