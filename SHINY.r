# =====================================================================
# EXTRA: GENERAR GRÁFICOS ESTÁTICOS PARA EL INFORME ESCRITO
# =====================================================================
library(ggplot2)

# 1. Gráfico de Boxplot para Presupuesto
ggplot(movies_clean, aes(y = budget / 1e6)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribución de Presupuesto", y = "Presupuesto (Millones USD)")

# 2. Gráfico de Boxplot para Ingresos (Recaudación)
ggplot(movies_clean, aes(y = revenue / 1e6)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Distribución de Ingresos", y = "Ingresos (Millones USD)")

# 3. Histograma de Calificaciones
ggplot(movies_clean, aes(x = vote_average)) +
  geom_histogram(fill = "green", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribución de Calificaciones", x = "Calificación Promedio", y = "Frecuencia")