library(shiny)
library(shinydashboard)
library(here)
library(RPostgres)
library(readr)
library(dplyr)
library(lubridate)
library(rpart)
library(caret)
library(ggplot2)


# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  

  
  # Main Application UI (hidden initially)
  navbarPage(
    "SKIP",
    theme = shinythemes::shinytheme("flatly"),
    id = "main_tabs",  # Identificador del navbar para poder controlarlo
    
    tabPanel( "Login", # Login Panel
      div(
        id = "login",
        class = "login-container",
        div(
          class = "login-panel",
          img(src = "SKIP.JPG", 
              width = "150px", 
              style = "display: block; margin: 0 auto;"),
          h2("Inicio de sesión", class = "login-title"),
          textInput("username", "Usuario"),
          passwordInput("password", "Contraseña"),
          actionButton("login_button", "Acceder", class = "login-button"),
          div(
            class = "login-links",
            #a("¿Problemas para iniciar sesión?", onclick = "Shiny.onInputChange('show_password_reset_modal', true)"),
            class = "login-links",
            a("¿Problemas para iniciar sesión?", onclick = "Shiny.onInputChange('show_password_reset_modal', true)"),
            actionButton("show_register", "Regístrate aquí", class = "switch-to-register"),
            a(href = "#", "Reportar problema de ingreso", onclick = "Shiny.onInputChange('show_report_modal', true)")
          )
        )
      )),
    
    tabPanel("Introducción",
             div(class = "content-wrapper",
                 h2("Introducción"),
                 p("En un mercado cada vez más competitivo, como el de los combustibles y servicios asociados,
                   las empresas deben adaptarse constantemente a las necesidades y expectativas cambiantes de sus clientes.
                   Skip, como líder en este sector, ha reconocido la importancia de comprender a fondo el comportamiento de
                   sus usuarios para mantener su posición de liderazgo y mejorar la satisfacción del cliente. Los productos
                   digitales como cupones electrónicos, TCT (Tarjeta de Combustible) y TAE (Tarjeta de Abastecimiento Empresarial)
                   generan un volumen significativo de datos que, si se analizan correctamente, pueden proporcionar información
                   crucial sobre las tendencias de consumo y las preferencias de los usuarios."),
                 
                 p("Este proyecto de análisis predictivo se enmarca en la metodología CRISP-DM (Cross-Industry Standard Process
                   for Data Mining), un estándar ampliamente utilizado para estructurar proyectos de minería de datos. 
                   El análisis se centrará en identificar y predecir las tendencias de comportamiento de los clientes en relación con 
                   los productos mencionados, con el fin de clasificarlos en categorías estratégicas como nuevos, perdidos, decrecientes,
                   crecientes y reactivados. Estas categorías ayudarán a Skip a segmentar a sus clientes de manera más precisa y a diseñar
                   estrategias de marketing y retención altamente personalizadas."),
                 
                 p("El desarrollo de este análisis se llevará a cabo utilizando herramientas avanzadas de R y PostgreSQL, aprovechando
                   tecnologías colaborativas como RStudio para la ejecución eficiente del código. Se utilizarán modelos predictivos, 
                   tales como Árboles de Decisión y otros algoritmos de clasificación, implementados mediante la librería caret, junto
                   con técnicas de análisis de datos proporcionadas por dplyr y lubridate. Además, se realizarán visualizaciones con ggplot2
                   para facilitar la interpretación de los resultados."),
                 
                 p("El objetivo final es no solo predecir con precisión el comportamiento futuro de los clientes, sino también ofrecer a Skip
                   insights valiosos que le permitan optimizar la toma de decisiones en tiempo real, mejorar la retención de clientes y aumentar
                   la eficiencia en la gestión de sus recursos. Este análisis predictivo proporcionará una base sólida para que Skip pueda anticiparse
                   a las necesidades de sus clientes, adaptarse rápidamente a los cambios en sus patrones de consumo y fortalecer su posición en el
                   mercado.")
             ) ,
             
             actionButton("logout_button", "Cerrar sesión", class = "logout-button"),
    ),
    
   
    tabPanel("Descripción del Problema",
             h2("Descripción del Problema"),
             p("Skip enfrenta el desafío de gestionar eficazmente la retención de clientes y optimizar sus estrategias de marketing en un entorno 
               altamente competitivo. A pesar de contar con datos de consumo de sus productos (como cupones electrónicos, TCT, y TAE), la empresa 
               carece de un sistema robusto que le permita identificar con precisión a los clientes en riesgo de abandono, aquellos cuyo consumo 
               está decreciendo, o detectar cambios tempranos en los patrones de comportamiento. Esta falta de previsión y segmentación limita la
               capacidad de Skip para implementar estrategias de retención proactivas, adaptar su oferta y mejorar la eficiencia operativa, lo que
               puede afectar negativamente la lealtad del cliente y su posición competitiva en el mercado."),
             
             h2("Propuesta de solución"),
             
             p("El proyecto propone desarrollar un modelo predictivo basado en técnicas avanzadas de análisis de datos y machine learning para 
               identificar las tendencias de comportamiento de los clientes de Skip. A través del análisis de datos históricos de consumo, el modelo
               permitirá categorizar a los clientes en segmentos clave (nuevos, perdidos, decrecientes, crecientes, reactivados) y anticipar el riesgo
               de abandono, así como cambios en los patrones de consumo. Este modelo servirá de base para personalizar las estrategias de marketing y optimizar 
               la asignación de recursos, proporcionando insights clave para tomar decisiones estratégicas basadas en datos. La solución también permitirá 
               a Skip ajustar sus campañas y recursos en tiempo real, alineando mejor la oferta con la demanda y mejorando la satisfacción del cliente.
               Al implementar esta solución, Skip podrá fortalecer su posición competitiva, aumentar la lealtad de los clientes, reducir la tasa de churn 
               y optimizar la eficiencia operativa.") ,
             
             actionButton("logout_button", "Cerrar sesión", class = "logout-button"),
    ),
    tabPanel("Objetivos",
             h2("Objetivos del Proyecto"),
             tags$ul(
               tags$li("Analizar y predecir las tendencias de comportamiento de los clientes en relación con los productos digitales de Skip."),
               tags$li("Clasificar a los clientes en categorías estratégicas como nuevos, perdidos, decrecientes, crecientes y reactivados."),
               tags$li("Desarrollar estrategias de marketing personalizadas basadas en los insights obtenidos del análisis de datos.")
             ) ,
             
             actionButton("logout_button", "Cerrar sesión", class = "logout-button"),
    ),
    tabPanel("Metodología",
             fluidRow(
               column(12,
                      p("El desarrollo del Proyecto APT se llevó a cabo utilizando la metodología CRISP-DM, 
        con la integración de R, RStudio y PostgreSQL como tecnologías principales para el 
        análisis predictivo. A continuación, se describen las fases y los procedimientos realizados:"),
                      
                      # Entendimiento del Negocio
                      div(class = "section-title", h3("Entendimiento del Negocio")),
                      div(class = "section-content",
                          tags$ul(
                            tags$li("Se definió el objetivo general del proyecto: desarrollar un modelo predictivo 
                  para anticipar las tendencias de comportamiento de los clientes de Skip en el 
                  uso de productos como cupones electrónicos, TCT y TAE."),
                            tags$li("Identificación de los segmentos clave de clientes (nuevos, perdidos, 
                  decrecientes, crecientes, reactivados) a partir de las necesidades comerciales."),
                            tags$li("Reuniones con los stakeholders para comprender las dinámicas operativas y 
                  de marketing relevantes para el análisis.")
                          )
                      ),
                      
                      # Entendimiento de los Datos
                      div(class = "section-title", h3("Entendimiento de los Datos")),
                      div(class = "section-content",
                          tags$ul(
                            tags$li("Conexión a la base de datos PostgreSQL mediante la librería RPostgres en RStudio. 
                  Se realizaron consultas SQL para extraer los datos históricos de consumo de clientes."),
                            tags$li("Exploración de los datos con dplyr y lubridate para manejar y analizar la 
                  información temporal."),
                            tags$li("Se utilizó ggplot2 para generar gráficos preliminares, que permitieron 
                  identificar patrones en los datos y explorar distribuciones de las variables.")
                          )
                      ),
                      
                      # Preparación de los Datos
                      div(class = "section-title", h3("Preparación de los Datos")),
                      div(class = "section-content",
                          tags$ul(
                            tags$li("Limpieza y transformación de los datos utilizando las librerías dplyr y lubridate. 
                  Se normalizaron las variables y se imputaron valores faltantes."),
                            tags$li("Integración de datos provenientes de diferentes tablas y fuentes en PostgreSQL. 
                  Esto permitió unificar la información clave para el análisis."),
                            tags$li("Se utilizó readr para cargar datos adicionales y here para gestionar las rutas 
                  de los archivos de manera más eficiente dentro del proyecto.")
                          )
                      ),
                      
                      # Modelado Predictivo
                      div(class = "section-title", h3("Modelado Predictivo")),
                      div(class = "section-content",
                          tags$ul(
                            tags$li("Implementación de modelos predictivos con caret y rpart. Se entrenaron distintos 
                  modelos, incluyendo árboles de decisión y algoritmos de clasificación, para 
                  predecir el comportamiento de los clientes."),
                            tags$li("Utilización de técnicas de validación cruzada para mejorar la precisión de los 
                  modelos y evitar el sobreajuste."),
                            tags$li("Se evaluaron los modelos con métricas como la RMSE, accuracy, y R2, utilizando 
                  la librería caret.")
                          )
                      ),
                      
                      # Evaluación del Modelo
                      div(class = "section-title", h3("Evaluación del Modelo")),
                      div(class = "section-content",
                          tags$ul(
                            tags$li("Evaluación de los resultados de los modelos utilizando un conjunto de datos 
                  de prueba independiente."),
                            tags$li("Visualización de los resultados utilizando ggplot2, generando gráficos como 
                  curvas ROC, gráficos de dispersión y líneas de tendencia para facilitar la 
                  interpretación de los resultados."),
                            tags$li("Selección del modelo más robusto para su despliegue, basado en el rendimiento 
                  en predicción de comportamientos y patrones de consumo.")
                          )
                      ),
                      
                      # Despliegue y Comunicación de Resultados
                      div(class = "section-title", h3("Despliegue y Comunicación de Resultados")),
                      div(class = "section-content",
                          tags$ul(
                            tags$li("Desarrollo de un dashboard interactivo utilizando las librerías shiny y 
                  shinydashboard, donde los usuarios pueden visualizar las predicciones y los 
                  segmentos de clientes."),
                            tags$li("Creación de un sistema que conecta los resultados del análisis predictivo 
                  con la base de datos PostgreSQL, permitiendo actualizaciones en tiempo real 
                  de las predicciones."),
                            tags$li("Documentación completa del proceso y presentación de los resultados a los 
                  stakeholders, incluyendo gráficos y reportes con insights accionables.")
                          )
                      )
               )
             ) ,
             
             actionButton("logout_button", "Cerrar sesión", class = "logout-button"),
    
    ),
    tabPanel("Análisis de Datos",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("analysis_date_range", 
                                "Selecciona el rango de fechas para el análisis:", 
                                start = Sys.Date() - 30, 
                                end = Sys.Date()),
                 actionButton("run_analysis", "Ejecutar Análisis")
               ),
               mainPanel(
                 h3("Resultados del Análisis de KPI"),
                 tableOutput("kpi_results"),  # Tabla donde se mostrarán los resultados de los KPI calculados
                 
                 h3("Resultados del Análisis Predictivo"),
                 textOutput("accuracy"),
                 #tableOutput("analysis_results"),
                 
                 h3("Distribución de Categorías de Clientes"),
                 plotOutput("category_plot"),  # Gráfico de distribución de categorías de clientes
                 
                 h3("Ventas Actuales vs Promedio de los Últimos 3 Meses"),
                 plotOutput("sales_plot")  # Gráfico de ventas actuales vs promedio de los últimos 3 meses
               )
             ) ,
             
             actionButton("logout_button", "Cerrar sesión", class = "logout-button"),
    )
    ,
    tabPanel("Configuración",
             fluidRow(
               column(
                 6,
                 fileInput("file1", "Sube un archivo CSV", accept = ".csv"),
                 tableOutput("contents") # Muestra la previsualización del archivo
               )
             ),
             fluidRow(
               column(6,
                      actionButton("save_data", "Guardar en la base de datos")
               )
             ),
             
             actionButton("logout_button", "Cerrar sesión", class = "logout-button"),   
             ),
    
  )
)


######################################################################################
### Server ###
######################################################################################

server <- function(input, output, session) {
  
  # Modal para reportar problema
  observeEvent(input$show_report_modal, {
    showModal(modalDialog(
      title = "Reporte de problema",
      "Para reportar un problema de ingreso, comuníquese al siguiente número de teléfono: +56 1 2312 3123 123",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })
  
  
  # Oculta las pestañas hasta que el usuario inicie sesión
  observe({
    hideTab(inputId = "main_tabs", target = "Introducción", session = session)
    hideTab(inputId = "main_tabs", target = "Descripción del Problema", session = session)
    hideTab(inputId = "main_tabs", target = "Objetivos", session = session)
    hideTab(inputId = "main_tabs", target = "Metodología", session = session)
    hideTab(inputId = "main_tabs", target = "Análisis de Datos", session = session)
    hideTab(inputId = "main_tabs", target = "Configuración", session = session)
  })
  
  # Inicializa la conexión como NULL
  con <- NULL
  
  # Función para establecer la conexión
  connect_to_db <- function() {
    tryCatch({
      dbConnect(
        RPostgres::Postgres(),
        dbname = "Skip1",
        host = "localhost",
        port = 5432,
        user = "postgres",
        password = "admin"
      )
    }, error = function(e) {
      showNotification(paste("Error al conectar con la base de datos:", e$message), type = "error")
      NULL
    })
  }
  
  # Establece la conexión cuando se inicia la sesión
  observe({
    con <<- connect_to_db()
    if (!is.null(con)) {
      showNotification("Conexión a la base de datos establecida", type = "message")
      print("Conexión a la base de datos establecida")
    }
  })
  
  
  
  
 ########################################################################################
 #   modal cambio de contraseña
 ########################################################################################    
  observeEvent(input$show_password_reset_modal, {
    showModal(modalDialog(
      title = "Restablecer Contraseña",
      textInput("reset_username", "Usuario"),
      passwordInput("new_password", "Nueva Contraseña"),
      passwordInput("confirm_password", "Confirmar Nueva Contraseña"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("reset_password_button", "Cambiar Contraseña")
      )
    ))
  })
  
  
  observeEvent(input$reset_password_button, {
    # Validación de campos
    if (input$new_password != input$confirm_password) {
      showNotification("Las contraseñas no coinciden", type = "error")
      return()
    }
    
    # Conexión a la base de datos
    if (is.null(con)) {
      showNotification("No se puede conectar a la base de datos", type = "error")
      return()
    }
    
    # Actualizar la contraseña en la base de datos
    query <- sprintf("UPDATE login_data SET contraseña = '%s' WHERE usuario = '%s'", 
                     input$new_password, input$reset_username)
    
    tryCatch({
      dbExecute(con, query)
      showNotification("Contraseña cambiada exitosamente", type = "message")
      removeModal()
    }, error = function(e) {
      showNotification(paste("Error al cambiar la contraseña:", e$message), type = "error")
    })
  })
 
  
  
  
  ##########################################################################################
  ### LOGIN ###
  ##########################################################################################
  
  
  observeEvent(input$login_button, {
    
    # Verificar si el usuario o la contraseña están en blanco
    if (input$username == "" || input$password == "") {
      showNotification("Usuario y/o contraseña no pueden estar en blanco", type = "error")
      return()  # Detener la ejecución si los campos están vacíos
    }
    
    # Verificar que se pueda conectar a la base de datos
    if (is.null(con)) {
      showNotification("No se puede conectar a la base de datos", type = "error")
      return()
    }
    
    # Consulta para verificar el usuario y obtener el tipo de usuario
    query <- sprintf("SELECT * FROM login_data WHERE usuario = '%s' AND contraseña = '%s'",
                     input$username, input$password)
    
    result <- dbGetQuery(con, query)
    
    if (nrow(result) > 0) {
      # El usuario está registrado y la contraseña es correcta
      showNotification("Inicio de sesión exitoso", type = "message")
      
      # Ocultar login y mostrar las pestañas del contenido
      hideTab(inputId = "main_tabs", target = "Login", session = session)
      showTab(inputId = "main_tabs", target = "Introducción", session = session)
      showTab(inputId = "main_tabs", target = "Descripción del Problema", session = session)
      showTab(inputId = "main_tabs", target = "Objetivos", session = session)
      showTab(inputId = "main_tabs", target = "Metodología", session = session)
      showTab(inputId = "main_tabs", target = "Análisis de Datos", session = session)

      
      # Habilitar la pestaña de configuración si el usuario es super_administrador
      tipo_usuario <- result$tipo_usuario[1]
     
      if (tipo_usuario == "super_administrador") {
        showTab(inputId = "main_tabs", target = "Configuración", session = session)
      }
    } else {
      # El usuario no está registrado o la contraseña es incorrecta
      showNotification("Usuario o contraseña incorrectos", type = "error")
    }
    
    
   
  })
  
 
#############################################################################################  
### REGISTRO ###  
#############################################################################################  
  observeEvent(input$show_register, {
    showModal(modalDialog(
      title = NULL, 
      easyClose = TRUE,
      div(
        class = "register-container",
        div(
          class = "register-panel",
          img(src = "SKIP.JPG", 
              width = "150px", 
              style = "display: block; margin: 0 auto;"),
          h2("Formulario de Registro", class = "register-title"),
          textInput("reg_username", "Usuario"),
          textInput("reg_nombre", "Nombre"),
          textInput("reg_email", "Email"),
          textInput("reg_telefono", "Teléfono"),
          passwordInput("reg_password", "Contraseña"),
          passwordInput("reg_confirm_password", "Confirmar Contraseña"),
          actionButton("register_button", "Enviar", class = "register-button"),
          
        )
      ),
      footer = NULL
    ))
  })
  
  observeEvent(input$register_button, {
    print("Botón de registro presionado")
    if (input$reg_password != input$reg_confirm_password) {
      showNotification("Las contraseñas no coinciden", type = "error")
      return()
    }
    
    if (is.null(con)) {
      showNotification("No se puede conectar a la base de datos", type = "error")
      return()
    }
    
    query <- sprintf("INSERT INTO login_data (email, usuario, nombre, telefono, contraseña, tipo_usuario) 
                    VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
                     input$reg_email,
                     input$reg_username,
                     input$reg_nombre,
                     input$reg_telefono,
                     input$reg_password,
                     "usuario")
    
    tryCatch({
      dbExecute(con, query)
      showNotification("Registro exitoso", type = "message")
      
      # Limpiar los campos después del registro
      updateTextInput(session, "reg_username", value = "")
      updateTextInput(session, "reg_nombre", value = "")
      updateTextInput(session, "reg_email", value = "")
      updateTextInput(session, "reg_password", value = "")
      updateTextInput(session, "reg_confirm_password", value = "")
      
      removeModal()
      
      
      
    }, error = function(e) {
      showNotification(paste("Error al registrar:", e$message), type = "error")
      
      
    })
  })
  #################################################################################
  ### CONFIGURACION ###
  #################################################################################
  
  # Leer el archivo CSV y mostrar el contenido
  data <- reactive({
    req(input$file1)
    file <- input$file1$datapath
    read_csv(file)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  # Guardar los datos en la tabla 'clientes'
  observeEvent(input$save_data, {
    req(data())
    
    df <- data() # Los datos cargados en el archivo CSV
    
    # Consulta SQL para insertar los datos
    query_insert <- "INSERT INTO clientes (nombre, apellido, rut, fecha_compra, producto, monto) VALUES ($1, $2, $3, $4, $5, $6)"
    
    tryCatch({
      for (i in 1:nrow(df)) {
        # Intentar insertar el registro
        tryCatch({
          dbExecute(con, query_insert, params = list(df$Nombre[i], df$Apellido[i], df$RUT[i], df$Fecha_Compra[i], df$Producto[i], df$Monto[i]))
        }, error = function(e) {
          # Ignorar errores de llave duplicada
          if (!grepl("duplicate key value violates unique constraint", e$message)) {
            stop(e) # Si no es un error de duplicado, lanza el error
          }
        })
      }
      showNotification("Datos guardados exitosamente", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al guardar los datos:", e$message), type = "error")
    })
  })
  
 ###############################################################################################
 ### ANALISIS DE DATOS ###
 ###############################################################################################
  
  # Función para calcular los KPI en función del rango de fechas
  calcular_kpis <- function(data, inicio_periodo, fin_periodo) {
    # KPI 1: Tasa de Retención de Clientes
    clientes_inicio_periodo <- data %>% filter(fecha_compra <= inicio_periodo) %>% distinct(rut) %>% nrow()
    clientes_fin_periodo <- data %>% filter(fecha_compra <= fin_periodo) %>% distinct(rut) %>% nrow()
    nuevos_clientes <- data %>% filter(fecha_compra > inicio_periodo & fecha_compra <= fin_periodo) %>% distinct(rut) %>% nrow()
    tasa_retencion <- ((clientes_inicio_periodo - clientes_fin_periodo + nuevos_clientes) / clientes_inicio_periodo) * 100
    
    # KPI 2: Valor de Vida del Cliente (CLV)
    clv <- data %>% group_by(rut) %>%
      summarise(
        valor_promedio_compra = mean(monto, na.rm = TRUE),
        frecuencia_compra = n(),
        duracion_cliente = as.numeric(difftime(fin_periodo, min(fecha_compra), units = "days")) / 365
      ) %>%
      summarise(clv = mean(valor_promedio_compra * frecuencia_compra * duracion_cliente, na.rm = TRUE)) %>%
      pull(clv)
    
    # KPI 3: Tasa de Conversión de Campañas de Marketing
    conversion <- data %>%
      summarise(
        clientes_contactados = n(),
        clientes_conversion = sum(monto > 0)
      ) %>%
      summarise(tasa_conversion = (clientes_conversion / clientes_contactados) * 100) %>%
      pull(tasa_conversion)
    
    # KPI 4: Índice de Satisfacción del Cliente (Ejemplo estático; ajustar si hay datos de encuestas)
    indice_satisfaccion <- 85.0  # Ejemplo estático
    
    # Retornar todos los KPI en una lista
    list(
      tasa_retencion = tasa_retencion,
      clv = clv,
      tasa_conversion = conversion,
      indice_satisfaccion = indice_satisfaccion
    )
  }
  
  # Observador para ejecutar el análisis de datos al hacer clic en el botón
  observeEvent(input$run_analysis, {
    req(input$analysis_date_range)
    
    # Fechas seleccionadas para el análisis
    inicio_periodo <- as.Date(input$analysis_date_range[1])
    fin_periodo <- as.Date(input$analysis_date_range[2])
    
    # Consultar los datos de clientes desde la base de datos
    query <- "SELECT nombre, apellido, rut, fecha_compra, monto FROM clientes ORDER BY fecha_compra"
    client_data <- dbGetQuery(con, query)
    client_data$fecha_compra <- as.Date(client_data$fecha_compra)
    
    # Calcular KPI con la función `calcular_kpis`
    kpi_results <- calcular_kpis(client_data, inicio_periodo, fin_periodo)
    
    # Mostrar resultados de KPI en la interfaz
    output$kpi_results <- renderTable({
      data.frame(
        KPI = c(
          "Tasa de Retención de Clientes",
          "Valor de Vida del Cliente",
          "Tasa de Conversión de Campañas de Marketing",
          "Índice de Satisfacción del Cliente"
        ),
        Resultado = c(
          paste0(round(kpi_results$tasa_retencion, 2), "%"),
          round(kpi_results$clv, 2),
          paste0(round(kpi_results$tasa_conversion, 2), "%"),
          paste0(round(kpi_results$indice_satisfaccion, 2), "%")
        )
      )
    })
    
    # Análisis de datos original
    analysis_period <- fin_periodo
    
    results <- client_data %>%
      group_by(rut) %>%
      arrange(fecha_compra) %>%
      summarise(
        first_purchase = min(fecha_compra, na.rm = TRUE),
        last_purchase = max(fecha_compra, na.rm = TRUE),
        total_sales = n(),
        last_3_months_avg = mean(monto[fecha_compra >= (analysis_period - months(3))], na.rm = TRUE),
        current_sale = sum(monto[fecha_compra == analysis_period], na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Crear una nueva columna para el estado basado en las reglas
    results$status <- case_when(
      results$first_purchase == analysis_period & results$total_sales == 1 ~ "Cliente Nuevo",
      results$total_sales == 0 & year(results$last_purchase) < year(analysis_period) ~ "Cliente Perdido",
      results$last_3_months_avg * 0.5 < results$current_sale ~ "Cliente Decrecen",
      results$current_sale > results$last_3_months_avg * 1.5 ~ "Cliente Crecen",
      results$current_sale > 0 & results$total_sales > 1 & year(results$last_purchase) == (year(analysis_period) - 1) &
        month(results$last_purchase) == month(analysis_period) & results$total_sales == 1 ~ "Clientes Reactivados",
      TRUE ~ "Sin Clasificar"
    )
    
    # Convertir el estado a factor
    results$status <- as.factor(results$status)
    
    # Preparar los datos para el modelo
    model_data <- results %>%
      select(total_sales, last_3_months_avg, current_sale, status) %>%
      na.omit()
    
    # Verificar si hay suficientes categorías para el modelo
    if (length(unique(model_data$status)) < 2) {
      showNotification("No hay suficientes categorías para realizar el análisis.", type = "warning")
      output$analysis_results <- renderTable(NULL)
      output$accuracy <- renderText("")
      output$category_plot <- renderPlot(NULL)
      output$sales_plot <- renderPlot(NULL)
      return()
    }
    
    # Dividir los datos en conjunto de entrenamiento y prueba
    set.seed(123)
    train_index <- createDataPartition(model_data$status, p = 0.8, list = FALSE)
    train_data <- model_data[train_index, ]
    test_data <- model_data[-train_index, ]
    
    # Entrenar un modelo de árbol de decisión
    model <- rpart(status ~ ., data = train_data, method = "class")
    
    # Realizar predicciones en el conjunto de prueba
    predictions <- predict(model, test_data, type = "class")
    
    # Crear un resumen de los resultados
    confusion_matrix <- confusionMatrix(predictions, test_data$status)
    
    # Mostrar resultados en la tabla
    output$analysis_results <- renderTable({
      confusion_matrix$table
    })
    
    # Mostrar el accuracy del modelo
    output$accuracy <- renderText({
      paste("Accuracy del modelo:", round(confusion_matrix$overall['Accuracy'], 4) * 100, "%")
    })
    
    # Graficar la distribución de las categorías de clientes
    output$category_plot <- renderPlot({
      ggplot(results, aes(x = status)) +
        geom_bar(fill = "skyblue") +
        labs(title = "Distribución de Clientes por Categoría",
             x = "Categoría de Cliente",
             y = "Cantidad de Clientes") +
        theme_minimal()
    })
    
    # Graficar las ventas actuales vs promedio de los últimos 3 meses
    output$sales_plot <- renderPlot({
      ggplot(results, aes(x = last_3_months_avg, y = current_sale, color = status)) +
        geom_point(size = 3) +
        labs(title = "Ventas Actuales vs Promedio de Últimos 3 Meses",
             x = "Promedio de Ventas (Últimos 3 Meses)",
             y = "Ventas Actuales") +
        theme_minimal()
    })
  })
  
  
  observeEvent(input$logout_button, {
    # Ocultar las pestañas del contenido y volver a mostrar la pestaña de inicio de sesión
    showTab(inputId = "main_tabs", target = "Login", session = session)
    hideTab(inputId = "main_tabs", target = "Introducción", session = session)
    hideTab(inputId = "main_tabs", target = "Descripción del Problema", session = session)
    hideTab(inputId = "main_tabs", target = "Objetivos", session = session)
    hideTab(inputId = "main_tabs", target = "Metodología", session = session)
    hideTab(inputId = "main_tabs", target = "Análisis de Datos", session = session)
    hideTab(inputId = "main_tabs", target = "Configuración", session = session)
    
    # Opcional: puedes limpiar variables de sesión o notificar al usuario
    showNotification("Sesión cerrada exitosamente", type = "message")
  })
  
  
  
  
}



# Run the app
shinyApp(ui = ui, server = server)
