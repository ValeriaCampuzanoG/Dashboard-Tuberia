# DASHBOARD CAPACIDAD


#Librerías 

#install.packages("renv")

library(shiny)
library(bs4Dash)
library(plotly)
library(shinycssloaders)
library(reactable)
library(fresh)

#source(file = "paquetes-setup.R")
#source(file = "importacion_textos.R")
#source(file = "codigo_preparacion.R")


# UI
ui <- fluidPage(
  
  
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/png", href = "https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg"),
    
    # Meta tags para compartir en redes sociales
    tags$meta(property = "og:title", content = "México ¿Cómo vamos? - Análisis del IPS"),
    tags$meta(property = "og:description", content = "Análisis interactivo del Índice de Progreso Social de México"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "México ¿Cómo vamos? - Análisis del IPS"),
    tags$meta(name = "twitter:description", content = "Análisis interactivo del Índice de Progreso Social de México"),
    
    # Title
    tags$title("México ¿Cómo vamos? - Análisis del IPS"),
    
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Ubuntu:wght@300;400;500;700&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    tags$style(
      HTML("
        body {
          margin: 0;
          padding: 0;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Cantarell, 'Montserrat', sans-serif;
          background-color: #f7f7f7;
        }
        
        .mcv-top-bar a:hover {
          transform: scale(1.1);
          transition: transform 0.2s ease;
        }
        
        .mcv-main-header a:hover {
          text-decoration: none !important;
        }
        
        /* Responsive */
        @media (max-width: 1024px) {
          .mcv-main-header > div > div {
            flex-direction: column;
            gap: 20px;
          }
          
          .mcv-main-header > div > div > div {
            flex-wrap: wrap;
            justify-content: center;
            gap: 15px;
          }
        }
        
        @media (max-width: 768px) {
          .mcv-top-bar > div > div {
            flex-direction: column;
            gap: 10px;
          }
          
          .mcv-main-header a {
            font-size: 10px;
          }
        }
        
        /* Estilos para el contenedor principal */
        .main-content {
          background-color: #f7f7f7;
          min-height: 100vh;
          padding: 10px 0;
        }
        
        /* Estilos para wellPanel */
        .well {
          background-color: white !important;
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border: 1px solid #e9ecef;
          margin-bottom: 20px;
        }
        
        /* Estilos para la sección de gráficas */
        .graph-container {
          background-color: white;
          border-radius: 12px;
          padding: 25px;
          margin: 20px 0;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          border: 1px solid #e9ecef;
        }
        
        /* Estilos para el hero header */
        .hero-header {
          position: relative;
          height: 180px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin: 0 0 25px 0;
          background: linear-gradient(135deg,  #A65461 0%, #D39C83 100%); 
          border-radius: 12px;
          overflow: hidden;
        }
        
        .hero-header::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(0, 0, 0, 0.2);
          z-index: 1;
        }
        
        .hero-title {
          position: relative;
          z-index: 2;
          color: white;
          text-align: center;
          font-size: 2.8rem;
          font-weight: 700;
          text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
          margin: 0;
          padding: 0 20px;
          white-space: pre-line;
          line-height: 1.2;
          font-family: 'Montserrat', sans-serif;
        }
        
        @media (max-width: 768px) {
          .hero-header {
            height: 140px;
          }
          
          .hero-title {
            font-size: 1.8rem;
          }
        }
        
        /* Estilos para tabs */
        .nav-tabs {
          border-bottom: 3px solid #6551D0;
        }
        
        .nav-tabs > li > a {
          color: #666;
          font-weight: 500;
          font-family: 'Montserrat', sans-serif;
        }
        
        .nav-tabs > li.active > a {
          color: #6551D0;
          font-weight: 700;
          border-bottom: 3px solid #6551D0;
        }
        
        /* Estilos para botones */
        .btn-primary {
          background-color: #6551D0;
          border-color: #6551D0;
          font-family: 'Montserrat', sans-serif;
          font-weight: 500;
        }
        
        .btn-primary:hover {
          background-color: #5441C0;
          border-color: #5441C0;
        }
        
        .btn-success {
          background-color: #00b783;
          border-color: #00b783;
          font-family: 'Montserrat', sans-serif;
          font-weight: 500;
        }
        
        .btn-success:hover {
          background-color: #009670;
          border-color: #009670;
        }
        
        /* Estilos para inputs */
        .form-control {
          border-radius: 8px;
          border: 2px solid #e9ecef;
          font-family: 'Montserrat', sans-serif;
        }
        
        .form-control:focus {
          border-color: #6551D0;
          box-shadow: 0 0 0 0.2rem rgba(101, 81, 208, 0.25);
        }
        
        /* Estilos para labels */
        .control-label {
          font-weight: 600;
          color: #333;
          font-family: 'Montserrat', sans-serif;
          margin-bottom: 8px;
        }
      ")
    )
  ),
  
  
  div(
    style = "width: 100%; margin: 0; padding: 0;",
    create_mcv_header()
  ),
  
  
  div(
    class = "hero-header",
    h1("Tubería procesal", 
       class = "hero-title")
  ),
  
  
  navset_tab( 
  
   tags$head(
    includeCSS("estilos.css")
    
   ),
   
   
   nav_panel("Tablero", 
             div(class = "main-container",
                 #Controles al inicio
                 div(class = "controls-section",
                     fluidRow(
                       column(3,
                              div(class = "control-group inline-control",
                                  tags$label("Seleccione una entidad:", `for` = "sel_entidad"),
                                  selectInput("sel_entidad", NULL,
                                              choices = lista_entidades,
                                              selected = "Nacional")
                              )),
                       column(3,
                              div(class = "control-group inline-control",
                                  tags$label("Año:", `for` = "ano"),
                                  selectInput("ano", NULL,
                                              choices = c("2024"),
                                              selected = "2024")
                              ))
                     )
                 ),
                 
                 div(class = "breakdown-cards",
                     #Tablita de C.I. iniciadas
                     div(class = "breakdown-card",
                         div(class = "breakdown-title", "Carpetas de investigación iniciadas"),
                         div(class = "breakdown-main-value", textOutput("ciInicidas")), #Total de C.I. iniciadas
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
                         div(class = "breakdown-main-value", textOutput("ciGestionadas")),
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
                         div(class = "breakdown-main-value", textOutput("causasGestionadas")),
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
                     div(class = "flow-card uno",
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
                     div(class = "flow-card dos",
                         div(class = "flow-title", "C.I. gestionadas"),
                         div(class = "flow-main-value", textOutput("totalci"))
                     ),
                     
                     div(class = "flow-operator", "\u279C"),
                     
                     # Canalizadas a MASC
                     div(class = "flow-card tres",
                         div(class = "flow-title", "Canalizadas a MASC"),
                         div(class = "flow-main-value", textOutput("canalizadasMASC")),
                         div(class = "flow-detail-item",
                             span(" Concluidas por acuerdos reparatorios: "),
                             span(class = "flow-detail-value", textOutput("concluidasMASC", inline = TRUE))
                         )
                     ),
                     
                     div(class = "flow-operator", "\u279C"),
                     
                     
                     # Causas penales
                     div(class = "flow-card cuatro",
                         div(class = "flow-title", "Causas penales gestionadas"),
                         div(class = "flow-main-value", textOutput("totalCausas")) #,
                         #div(class = "flow-detail-item", textOutput("concluidos_pct"))
                     ),
                     
                     div(class = "flow-operator", "\u279C"),
                     
                     # Justicia Alternativa
                     div(class = "flow-card cinco",
                         div(class = "flow-title", "Justicia Alternativa"),
                         div(class = "flow-main-value", textOutput("totalJA")),
                         div(class = "flow-detail-item",
                             span("Acuerdos reparatorios: "),
                             span(class = "flow-detail-value", textOutput("totalAcuerdos", inline = TRUE))
                         ),
                         div(class = "flow-detail-item",
                             span("Suspensión condicional: "),
                             span(class = "flow-detail-value", textOutput("totalSusp", inline = TRUE))
                         )   
                     ),
                     
                     div(class = "flow-operator", "\u279C"),
                     
                     # Sentencias en Juicio Oral
                     div(class = "flow-card seis",
                         div(class = "flow-title", "Sentencias en Juicio Oral"),
                         div(class = "flow-main-value", textOutput("Sentencias")),
                         div(class = "flow-detail-item", textOutput("concluidos_pct"))
                     )
                 ),
                 
                 # Cards de gráficas
                 # Gráficas de determinaciones
                 div(class = "breakdown-cards",
                     div(class = "breakdown-card",
                         div(class = "breakdown-title", "Determinaciones"),
                         girafeOutput("grafica_barras_determinaciones", width = "100%", height = "300px")
                     ),
                     
                     # Gráfica de sentencias    
                     div(class = "breakdown-card",
                         div(class = "breakdown-title", "Sentencias"),
                         girafeOutput("grafica_barras_sentencias", width = "100%", height = "300px")
                     ),
                     # Tab de pendientes     
                     div(class = "breakdown-card",
                         div(class = "breakdown-title", "Pendientes al final del año"),
                         div(style = "height: 20px; text-align: center;  color: #999;font-size: 20px;",
                             "Fiscalía:"), 
                         br(),
                         div(#class = "flow-main-value", 
                           style = "height: 25px; text-align: center; color: #882217; font-size: 60px; font-weight: bold;",
                           textOutput("ciPendientes")), 
                         br(),
                         br(),
                         br(),
                         div(style = "height: 20px; text-align: center; color: #999;font-size: 20px; ",
                             "Tribunales:"), 
                         br(),
                         div(#class = "flow-main-value", 
                           style = "height: 25px; text-align: center; color: #882217; font-size: 60px; font-weight: bold;",
                           textOutput("causasPendientes"))
                     ) 
                 )
             )
             
             
             ), 
   nav_panel("Gráfico de barras", 
             
             div(class = "controls-section",
                 fluidRow(
                   column(6,
                          div(class = "control-group inline-control",
                              tags$label("Seleccione una variable:", `for` = "ind_sel"),
                              selectInput("ind_sel", NULL,
                                          choices = opciones_tuberia,
                                          selected = "01 - Llamadas 911")
                          )),
                   column(6,
                          div(class = "control-group inline-control",
                              tags$label("Año:", `for` = "ano_sel"),
                              selectInput("ano_sel", NULL,
                                          choices = c("2024", "2023", "2022", "2021", "2020", "2019"),
                                          selected = "2023")
                          ))
                 )
             ),
             div(
               style = "width: 100%; height: min(55vh, 500px);",
               girafeOutput("grafica_barras_tub", width = "100%", height = "100%")
             )
             
             ), 
   nav_panel("Series de tiempo", 
            
             div(class = "controls-section",
                 fluidRow(
                   column(6,
                          div(class = "control-group inline-control",
                              tags$label("Seleccione una variable:", `for` = "ind_sel_2"),
                              selectInput("ind_sel_2", NULL,
                                          choices = opciones_tuberia,
                                          selected = "01 - Llamadas 911")
                          )),
                   column(
                     width = 6, 
                     div(class = "control-group inline-control",
                         tags$label("Selecciona la(s) entidad(es):", `for` = "entidades_lineas_tuberia"),
                     shinyWidgets::pickerInput(
                       inputId = "entidades_lineas_tuberia",
                       #label = "Selecciona la(s) entidad(es):",
                       choices = lista_entidades,
                       multiple = TRUE,
                       selected = sort(x = lista_entidades),
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Deseleccionar todas",
                                      `select-all-text` = "Seleccionar todas",
                                      `none-selected-text` = "Ninguna unidad seleccionada")
                     )
                     )
                   )
                 )
             ),
             div(
               style = "width: 100%; height: min(55vh, 500px);",
               girafeOutput("grafica_lineas_tub", width = "100%", height = "100%")
             )
             
   ) , 
   nav_panel("Tablas",
             titlePanel(""),
             div(class = "controls-section",
                 fluidRow(
                   column(6,
                          div(class = "control-group inline-control",
                              tags$label("Genera tablas con la variable y/o entidad de tu elección:", `for` = "ind_sel")
                          ))
                 )
             ),

             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_state", "Selecciona una(s) entidad(ed):",
                             choices = lista_entidades, multiple = TRUE, selected = "Nacional"),
                 selectInput("selected_variable", "Selecciona una(s) variable(s):",
                             choices = opciones_tuberia, multiple = TRUE, selected = "01 - Llamadas 911"),
                 sliderInput("selected_years", "Selecciona el periodo:",
                             min = min(as.numeric(bd_tuberia$ano)), max = max(as.numeric(bd_tuberia$ano)),
                             value = c(min(bd_tuberia$ano), max(bd_tuberia$ano)), step = 1),
                 downloadButton("download_xlsx", "Descarga los datos en formato .xlsx"),
                 downloadButton("download_png", "Descarga la tabla en formato PNG")
               ),

               mainPanel(
                 h3(textOutput("table_title")),
                 reactableOutput("custom_table")
               )
             ))
  
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
  
  
  # TABLAS 
  
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
  
  # FLUJO PRINCIPAL 
  
  # Llamadas de emergencia 
  output$llamadas <- renderText({ format(datos()$llamadas_emergencia, big.mark = ",") })
  output$llamadas911 <- renderText({ format(datos()$llamadas_911, big.mark = ",") })
  output$llamadas089 <- renderText({ format(datos()$llamadas_089, big.mark = ",") })
  
  # Carpetas de investigacion
  output$totalci <- renderText({ format(datos()$ci_gestionadas, big.mark = ",") })
  
  #Canalizadas a MASC
  # Canalizadas
  output$canalizadasMASC <- renderText({ format(datos()$total_canalizadas_masc, big.mark = ",") })
  # Concluidas
  
  # Causas ingreasadas
  output$totalCausas <- renderText({ format(datos()$causas_gestionadas, big.mark = ",") })
  
  # Justicia Alternativa
  output$totalJA <- renderText({ format(datos()$justicia_alternativa, big.mark = ",") })
  output$totalAcuerdos <- renderText({ 
    paste0(format(datos()$acuerdo_reparatorio_cumplido, big.mark = ","),
           " (",
           format(datos()$pct_acuerdos_sj, big.mark = ","), 
           "%)") 
    })
  
  output$totalSusp <- renderText({ 
    paste0(format(datos()$suspension_condicional_cumplida, big.mark = ","),
           " (",
           format(datos()$pct_susp_sj, big.mark = ","), 
           "%)")
    })
  
  # Sentencias
  output$Sentencias <- renderText({ format(datos()$sentencia_jo_tt, big.mark = ",") })
  
  
  # Pendientes 
  output$ciPendientes <- renderText({ format(datos()$rezago_ano_corriente, big.mark = ",") })
  output$causasPendientes <- renderText({ format(datos()$causas_rezago_corriente, big.mark = ",") })
  
  # Gráfica de determinaciones 
  
  output$grafica_barras_determinaciones <- renderGirafe({
    gen_barra_determinaciones(ent_sel = input$sel_entidad)
  })
  
  
  # Gráfica de sentencias 
  
  output$grafica_barras_sentencias <- renderGirafe({
    gen_barra_sentencias(ent_sel = input$sel_entidad)
  })
  
  
  # Gráfico de barras 
  output$grafica_barras_tub <- renderGirafe({
    gen_barras_tub(
                   ind_sel = input$ind_sel, 
                   ano_sel_imp = input$ano_sel)
  })
  
  
  # Gráfico de líneas 
  output$grafica_lineas_tub <- renderGirafe({
    gen_lineas_tub(
      ind_sel = input$ind_sel_2, 
      entidades_resaltadas = input$entidades_lineas_tuberia)
  })
  
  
  # Generador de tablas 
  data_filtered <- reactive({
    bd_tuberia %>%
      mutate(ano = as.numeric(ano)) %>%
      filter(
        entidad %in% input$selected_state,
        cod_indicador %in% input$selected_variable,
        ano >= input$selected_years[1],
        ano <= input$selected_years[2]
      )
  })
  
  # Reactive for table construction
  custom_table_data <- reactive({
    df <- data_filtered()
    
    # Case 1: One state, one variable
    if(length(input$selected_state) == 1 && length(input$selected_variable) == 1){
      df %>%
        select(ano, total) %>%
        rename(Año = ano, Total = total)
      
      # Case 2: One state, multiple variables
    } else if(length(input$selected_state) == 1 && length(input$selected_variable) > 1){
      df %>%
        select(entidad, cod_indicador, ano, total) %>%
        tidyr::pivot_wider(names_from = cod_indicador, values_from = total) %>%
        rename(Año = ano)
      
      # Case 3: Multiple states, one variable
    } else if(length(input$selected_state) > 1 && length(input$selected_variable) == 1){
      df %>%
        select(entidad, cod_indicador, ano, total) %>%
        tidyr::pivot_wider(names_from = state, values_from = total) %>%
        rename(Año = ano)
      
    } else {
      # fallback: just show year + total
      df %>%
        group_by(ano) %>%
        summarise(Total = sum(total)) %>%
        rename(Año = ano)
    }
  })
  
  # Table title
  output$table_title <- renderText({
    df <- data_filtered()
    years <- range(bd_tuberia$ano)
    
    if(length(input$selected_state) == 1 && length(input$selected_variable) == 1){
      paste0("Total de ", input$selected_variable, " en ", input$selected_state, ", ", 
             years[1], "-", years[2])
    } else if(length(input$selected_state) == 1 && length(input$selected_variable) > 1){
      paste0("Indicadores procesales de ", input$selected_state, ", ", 
             years[1], "-", years[2])
    } else if(length(input$selected_state) > 1 && length(input$selected_variable) == 1){
      paste0("Evolución de ", input$selected_variable, ", ", years[1], "-", years[2])
    } else {
      paste0("Resumen ", years[1], "-", years[2])
    }
  })
  
  # Render reactable
  # output$custom_table <- renderReactable({
  #   reactable(custom_table_data(), searchable = TRUE, filterable = TRUE)
  # })
  
  output$custom_table <- renderReactable({
    data <- custom_table_data()  # your reactive data frame
    col_names <- names(df)
    
    # Create a list of column definitions
    column_defs <- lapply(col_names, function(col) {
      if (col == col_names[1]) {
        # First column always centered
        colDef(align = "center")
      } else {
        # All other columns formatted with prettyNum
        colDef(
          align = "right",
          format = colFormat(
            transform = function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
          )
        )
      }
    })
    names(column_defs) <- col_names
    
    reactable(
      data,
      searchable = TRUE,
      filterable = TRUE,
      columns = column_defs
    )
  })
  
  # Download XLSX
  output$download_xlsx <- downloadHandler(
    filename = function() {"custom_table.xlsx"},
    content = function(file) {
      write_xlsx(custom_table_data(), file)
    }
  )
  
  # Download PNG
  output$download_png <- downloadHandler(
    filename = function() {"custom_table.png"},
    content = function(file) {
      tmpfile <- tempfile(fileext = ".html")
      reactablefmtr::save_reactable(custom_table_data(), tmpfile)
      webshot2::webshot(tmpfile, file = file, vwidth = 1200, vheight = 800)
    }
  )
  

}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)