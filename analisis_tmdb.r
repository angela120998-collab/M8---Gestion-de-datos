# =====================================================================
# PROYECTO: Procesamiento e Imputación de Datos Cinematográficos (TMDB)
# AUTORA: Angela Betalleluz
# =====================================================================

# =====================================================================
# 1. CONEXIÓN A AWS RDS Y EXTRACCIÓN DE DATOS
# =====================================================================
library(DBI)
library(RMySQL)
library(dplyr)

# Se establece la conexión con la base de datos MySQL alojada en Amazon RDS.
con <- dbConnect(MySQL(), 
                 user = 'admin', 
                 password = 'tu_password_seguro', 
                 dbname = 'movies_db', 
                 host = 'database-1.cluster-xxx.region.rds.amazonaws.com')

# Se ejecuta la consulta SQL para obtener solo las columnas requeridas para el análisis.
query <- "SELECT id, title, budget, revenue, runtime, vote_average, vote_count FROM tmdb_movies;"
movies_df <- dbGetQuery(con, query)

# Desconectar de la base de datos al finalizar la consulta.
dbDisconnect(con)

# =====================================================================
# 2. PRE-PROCESAMIENTO E IMPUTACIÓN DE DATOS (MICE)
# =====================================================================
library(mice)

# Transformar valores 0 a NA para que el algoritmo los reconozca como perdidos.
movies_df <- movies_df %>%
  mutate(budget = ifelse(budget == 0, NA, budget),
         revenue = ifelse(revenue == 0, NA, revenue),
         runtime = ifelse(runtime == 0, NA, runtime))

# Imputación usando Predictive Mean Matching (pmm).
set.seed(123) 
imputed_data <- mice(movies_df[, c("budget", "revenue", "runtime", "vote_average", "vote_count")], 
                     m = 1, method = "pmm", maxit = 5, printFlag = FALSE)

# Se recupera el dataframe consolidado y sin datos faltantes
movies_clean <- complete(imputed_data, 1)
movies_clean$title <- movies_df$title

# =====================================================================
# 3. DETECCIÓN DE VALORES ATÍPICOS MULTIVARIADOS (MAHALANOBIS)
# =====================================================================
num_vars <- movies_clean[, c("budget", "revenue", "vote_average")]

# Cálculo del centroide y la matriz de covarianza
center <- colMeans(num_vars)
cov_matrix <- cov(num_vars)

# Cálculo de la Distancia de Mahalanobis
movies_clean$mahalanobis <- mahalanobis(num_vars, center, cov_matrix)

# Identificación de outliers (p=0.001, 3 grados de libertad)
threshold <- qchisq(p = 0.999, df = 3)
movies_clean$is_outlier <- movies_clean$mahalanobis > threshold

# =====================================================================
# 4. DESARROLLO DE LA APLICACIÓN INTERACTIVA (SHINY)
# =====================================================================
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dashboard Interactivo: Rentabilidad y Atípicos en TMDB"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("budget_filter", "Filtrar por Presupuesto Mínimo (USD):",
                  min = 0, max = max(movies_clean$budget, na.rm=TRUE), 
                  value = 1000000, step = 5000000, pre = "$", sep = ",")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      h4("Listado de Valores Atípicos Multivariados (Anomalías del Mercado)"),
      dataTableOutput("outlierTable")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    movies_clean %>% filter(budget >= input$budget_filter)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = budget, y = revenue, color = is_outlier)) +
      geom_point(alpha = 0.6, size = 3) +
      scale_color_manual(values = c("#1f77b4", "#d62728"), 
                         labels = c("Comportamiento Normal", "Atípico Multivariado"),
                         name = "Diagnóstico") +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Análisis Bivariado: Presupuesto vs. Recaudación", 
           x = "Presupuesto (USD)", y = "Recaudación (USD)")
  })
  
  output$outlierTable <- renderDataTable({
    filtered_data() %>% 
      filter(is_outlier == TRUE) %>% 
      select(title, budget, revenue, vote_average, mahalanobis) %>%
      arrange(desc(mahalanobis)) 
  })
}

# Ejecutar aplicación:
# shinyApp(ui = ui, server = server)