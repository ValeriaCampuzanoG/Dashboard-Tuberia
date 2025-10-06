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
    tags$style(HTML("
      .main-container {
        font-family: Arial, sans-serif;
        background-color: #f5f5f5;
        padding: 20px;
      }
      
      .controls-section {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .control-group label {
        background-color: #635583;
        color: white;
        padding: 5px 10px;
        border-radius: 4px;
        font-size: 12px;
        font-weight: bold;
        margin-bottom: 5px;
        display: inline-block;
      }
      
      .header-section {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .header-title {
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 15px;
        color: #333;
      }
      
      .metrics-row {
        display: flex;
        gap: 10px;
        margin-bottom: 20px;
        flex-wrap: wrap;
      }
      
      .metric-card {
        background-color: white;
        border-radius: 8px;
        padding: 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        min-width: 100px;
        text-align: center;
        border-top: 4px solid;
        flex: 1;
        max-width: 150px;
      }
      
      .metric-card.blue { border-top-color: #4a90e2; }
      .metric-card.green { border-top-color: #7ed321; }
      .metric-card.orange { border-top-color: #f5a623; }
      
      .metric-icon {
        font-size: 20px;
        margin-bottom: 5px;
      }
      
      .metric-value {
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 3px;
        color: #333;
      }
      
      .metric-label {
        font-size: 11px;
        color: #666;
      }
      
      .flow-container {
        display: flex;
        align-items: center;
        gap: 20px;
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        overflow-x: auto;
      }
      
      .flow-card {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 15px;
        text-align: center;
        min-width: 150px;
        border: 2px solid transparent;
      }
      
      .flow-card.purple { 
        background-color: #e8e4f3; 
        border-color: #9b59b6;
      }
      
      .flow-card.green { 
        background-color: #e8f5e8; 
        border-color: #27ae60;
      }
      
      .flow-card.blue { 
        background-color: #e3f2fd; 
        border-color: #2196f3;
      }
      
      .flow-card.orange { 
        background-color: #fff3e0; 
        border-color: #ff9800;
      }
      
      .flow-title {
        font-size: 12px;
        font-weight: bold;
        color: #333;
        margin-bottom: 8px;
      }
      
      .flow-main-value {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 8px;
        color: #333;
      }
      
      .flow-detail-item {
        font-size: 11px;
        margin-bottom: 2px;
        color: #555;
      }
      
      .flow-detail-value {
        font-weight: bold;
      }
      
      .flow-operator {
        font-size: 36px;
        font-weight: bold;
        color: #333;
        align-self: center;
      }
      
      .controls-section {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .breakdown-cards {
        display: flex;
        gap: 15px;
        margin-top: 20px;
        flex-wrap: wrap;
      }
      
      .breakdown-card {
        background-color: white;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        flex: 1;
        min-width: 200px;
      }
      
      .breakdown-title {
        font-size: 14px;
        font-weight: bold;
        margin-bottom: 15px;
        color: #333;
        text-align: center;
        padding-bottom: 10px;
        border-bottom: 2px solid #eee;
      }
      
      .breakdown-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 8px 0;
        border-bottom: 1px solid #f0f0f0;
      }
      
      .breakdown-item:last-child {
        border-bottom: none;
      }
      
      .breakdown-value {
        font-weight: bold;
      }
      
      .breakdown-percentage {
        font-size: 12px;
        color: #666;
      }
    "))
  ),
  
  div(class = "main-container",
      # Controles al inicio
      div(class = "controls-section",
          fluidRow(
            column(3,
                   div(class = "control-group",
                       tags$label("Seleccione una entidad:", `for` = "periodo"),
                       selectInput("entidadSel", "",
                                   choices = list("2025_ABR" = "2025_ABR", "2025_MAR" = "2025_MAR"),
                                   selected = "2025_ABR") #aquí poner Nacional
                   )
            ),
            column(3,
                   div(class = "control-group",
                       tags$label("Año:", `for` = "circuito"),
                       selectInput("circuito", "",
                                   choices = list("All" = "All", "Circuito 1" = "C1", "Circuito 2" = "C2"),
                                   selected = "All") #aquí poner solo 2024 y que no hay forma de cambiarlo
                   )
            ) #,
            # column(3,
            #        div(class = "control-group",
            #            tags$label("TLF:", `for` = "tlf"),
            #            selectInput("tlf", "",
            #                        choices = list("All" = "All"),
            #                        selected = "All")
            #        )
            # )
          )
      ),
      
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
              div(class = "breakdown-title", "C.I. iniciadas"),
              div(class = "flow-main-value", textOutput("resueltos")),
              div(id = "tramite_breakdown",
                  div(class = "breakdown-item",
                      span("Con persona imputada"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_ingresos", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_ingresos_pct", inline = TRUE))
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Sin persona imputada"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_reingresos_comp", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_reingresos_comp_pct", inline = TRUE))
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Mixta"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_reingresos_comp", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_reingresos_comp_pct", inline = TRUE))
                      )
                  )
                  
              )
          ),
          
          
          #Tablita de C.I. gestionadas
          div(class = "breakdown-card",
              div(class = "breakdown-title", "C.I. gestionadas"),
              div(class = "flow-main-value", textOutput("resueltos")),
              div(id = "tramite_breakdown",
                  div(class = "breakdown-item",
                      span("Iniciadas"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_ingresos", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_ingresos_pct", inline = TRUE))
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Pendientes del año anterior"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_reingresos_comp", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_reingresos_comp_pct", inline = TRUE))
                      )
                  )
              )
          ),
          
          
          
          #Tablita de causas penales gestionadas 
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Causas penales gestionadas"),
              div(class = "flow-main-value", textOutput("resueltos")),
              div(id = "tramite_breakdown",
                  div(class = "breakdown-item",
                      span("Iniciadas"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_ingresos", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_ingresos_pct", inline = TRUE))
                      )
                  ),
                  div(class = "breakdown-item",
                      span("Pendientes del año anterior"),
                      span(
                        span(class = "breakdown-value", textOutput("tramite_reingresos_comp", inline = TRUE)),
                        " ",
                        span(class = "breakdown-percentage", textOutput("tramite_reingresos_comp_pct", inline = TRUE))
                      )
                  )
              )
          )
      ),
      
      br(),
      # Flujo principal
      div(class = "flow-container",
          div(class = "flow-card purple",
              div(class = "flow-title", "Llamadas de emergencia"),
              div(class = "flow-main-value", textOutput("asuntos_ingresados")),
              div(class = "flow-detail-item",
                  span("911: "),
                  span(class = "flow-detail-value", textOutput("fisicos_display", inline = TRUE))
              ),
              div(class = "flow-detail-item",
                  span("089: "),
                  span(class = "flow-detail-value", textOutput("electronicos_display", inline = TRUE))
              )
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          div(class = "flow-card green",
              div(class = "flow-title", "C.I. gestionadas"),
              div(class = "flow-main-value", textOutput("resueltos"))
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          div(class = "flow-card purple",
              div(class = "flow-title", "Justicia Alternativa (S.M)"),
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
          
          div(class = "flow-card blue",
              div(class = "flow-title", "Causas penales gestionadas"),
              div(class = "flow-main-value", textOutput("concluidos")),
              div(class = "flow-detail-item", textOutput("concluidos_pct"))
          ),
          
          div(class = "flow-operator", "\u279C"),
          
          div(class = "flow-card purple",
              div(class = "flow-title", "Justicia Alternativa (S.J)"),
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
          
          div(class = "flow-card blue",
              div(class = "flow-title", "Sentencias"),
              div(class = "flow-main-value", textOutput("concluidos")),
              div(class = "flow-detail-item", textOutput("concluidos_pct"))
          )
      ),
      
      # Cards de desglose
      div(class = "breakdown-cards",
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Determinaciones"),
              div(style = "height: 100px; text-align: center; line-height: 100px; color: #999;",
                  "Gráfico placeholder")
          ),
          
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Sentencias en juicio oral"),
              div(style = "height: 100px; text-align: center; line-height: 100px; color: #999;",
                  "Gráfico placeholder")
          ),
          
          div(class = "breakdown-card",
              div(class = "breakdown-title", "Pendientes al final del año"),
              div(style = "height: 100px; text-align: center; line-height: 100px; color: #999;",
                  "Gráfico placeholder")
          ) #,
          
          # div(class = "breakdown-card",
          #     div(class = "breakdown-title", "Asuntos en trámite acumulados"),
          #     div(id = "tramite_breakdown",
          #         div(class = "breakdown-item",
          #             span("Ingresos en trámite"),
          #             span(
          #                 span(class = "breakdown-value", textOutput("tramite_ingresos", inline = TRUE)),
          #                 " ",
          #                 span(class = "breakdown-percentage", textOutput("tramite_ingresos_pct", inline = TRUE))
          #             )
          #         ),
          #         div(class = "breakdown-item",
          #             span("Reingresos por competencia"),
          #             span(
          #                 span(class = "breakdown-value", textOutput("tramite_reingresos_comp", inline = TRUE)),
          #                 " ",
          #                 span(class = "breakdown-percentage", textOutput("tramite_reingresos_comp_pct", inline = TRUE))
          #             )
          #         ),
          #         div(class = "breakdown-item",
          #             span("Reingresos por sentencia de entrada"),
          #             span(
          #                 span(class = "breakdown-value", textOutput("tramite_reingresos_sent", inline = TRUE)),
          #                 " ",
          #                 span(class = "breakdown-percentage", textOutput("tramite_reingresos_sent_pct", inline = TRUE))
          #             )
          #         ),
          #         div(class = "breakdown-item",
          #             span("Entrada"),
          #             span(
          #                 span(class = "breakdown-value", textOutput("tramite_entrada", inline = TRUE)),
          #                 " ",
          #                 span(class = "breakdown-percentage", textOutput("tramite_entrada_pct", inline = TRUE))
          #             )
          #         )
          #     )
          # )
      )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  
  # Outputs para métricas principales
  output$total_tlf <- renderText({ datos()$tlf })
  output$total_jueces <- renderText({ datos()$jueces })
  output$total_juezas <- renderText({ datos()$juezas })
  
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
  
  output$concluidos <- renderText({ format(datos()$concluidos, big.mark = ",") })
  output$concluidos_pct <- renderText({ paste0(round(datos()$concluidos / datos()$asuntos_ingresados * 100, 1), "%") })
  
  output$tramite <- renderText({ format(datos()$tramite, big.mark = ",") })
  output$tramite_pct <- renderText({ paste0(round(datos()$tramite / datos()$asuntos_ingresados * 100, 2), "%") })
  
  # Outputs para desglose de trámite
  output$tramite_ingresos <- renderText({ format(datos()$tramite_ingresos, big.mark = ",") })
  output$tramite_ingresos_pct <- renderText({ paste0(round(datos()$tramite_ingresos / datos()$tramite * 100, 2), " %") })
  
  output$tramite_reingresos_comp <- renderText({ format(datos()$tramite_reingresos_comp, big.mark = ",") })
  output$tramite_reingresos_comp_pct <- renderText({ paste0(round(datos()$tramite_reingresos_comp / datos()$tramite * 100, 2), " %") })
  
  output$tramite_reingresos_sent <- renderText({ format(datos()$tramite_reingresos_sent, big.mark = ",") })
  output$tramite_reingresos_sent_pct <- renderText({ paste0(round(datos()$tramite_reingresos_sent / datos()$tramite * 100, 2), " %") })
  
  output$tramite_entrada <- renderText({ format(datos()$tramite_entrada, big.mark = ",") })
  output$tramite_entrada_pct <- renderText({ paste0(round(datos()$tramite_entrada / datos()$tramite * 100, 2), " %") })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)