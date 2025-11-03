# ================================================
# PARTE 3: ANÁLISIS Y VISUALIZACIÓN
# ================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)
library(rpart)
library(kknn)
library(nnet)

# Leer CSV limpio
eq_clean <- read_csv("/data/earthquakes_clean.csv", quote='"', na = c("", "NA"))

# Convertir columnas de texto a minúsculas
eq_clean <- eq_clean %>% mutate(across(where(is.character), ~ str_to_lower(.)))

# Renombrar columnas según tu CSV limpio
colnames(eq_clean) <- c("time", "latitude", "longitude", "depth", "mag",
                        "magType", "nst", "gap", "dmin", "place", "type", "status")

# =======================
# Visualizaciones
# =======================

# Histograma de magnitudes
png("/data/figures/mag_histogram.png", width = 800, height = 600)
ggplot(eq_clean, aes(x = mag)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Magnitudes", x = "Magnitud", y = "Frecuencia")
dev.off()

# Histograma de profundidades
png("/data/figures/depth_histogram.png", width = 800, height = 600)
ggplot(eq_clean, aes(x = depth)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Distribución de Profundidades", x = "Profundidad (km)", y = "Frecuencia")
dev.off()

# Boxplot de magnitud por tipo de magnitud
png("/data/figures/mag_boxplot_by_type.png", width = 800, height = 600)
ggplot(eq_clean, aes(x = magType, y = mag)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Magnitud por Tipo de Magnitud", x = "Tipo de Magnitud", y = "Magnitud")
dev.off()

# Conteo por tipo de evento
png("/data/figures/type_count.png", width = 800, height = 600)
ggplot(eq_clean, aes(x = type)) +
  geom_bar(fill = "purple") +
  labs(title = "Conteo por Tipo de Evento", x = "Tipo de Evento", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Conteo por estado
png("/data/figures/status_count.png", width = 800, height = 600)
ggplot(eq_clean, aes(x = status)) +
  geom_bar(fill = "pink") +
  labs(title = "Conteo por Estado", x = "Estado", y = "Frecuencia")
dev.off()

# Mapa de epicentros
png("/data/figures/epicenter_map.png", width = 1000, height = 600)
ggplot(eq_clean, aes(x = longitude, y = latitude, color = mag)) +
  geom_point(alpha = 0.6) +
  labs(title = "Mapa de Epicentros", x = "Longitud", y = "Latitud") +
  scale_color_gradient(low = "yellow", high = "red")
dev.off()

# ================================================
# PARTE 4: MODELAMIENTO
# ================================================

# Preparar datos para modelamiento
df_model <- eq_clean %>%
  select(depth, mag, magType, nst, gap, dmin, status, type) %>%
  filter(complete.cases(.))

# Variables categóricas a factor
df_model$magType <- as.factor(df_model$magType)
df_model$status <- as.factor(df_model$status)
df_model$type <- as.factor(df_model$type)

# División entrenamiento / test (70%-30%)
set.seed(123)
train_index <- createDataPartition(df_model$type, p = 0.7, list = FALSE)
train_data <- df_model[train_index, ]
test_data <- df_model[-train_index, ]

# ========================
# Entrenamiento de 5 modelos
# ========================

# 1. Árbol de decisión
model_tree <- rpart(type ~ ., data = train_data, method = "class")

# 2. Random Forest
model_rf <- randomForest(type ~ ., data = train_data, ntree = 100)

# 3. kNN
model_knn <- train(type ~ ., data = train_data, method = "kknn")

# 4. SVM
model_svm <- svm(type ~ ., data = train_data, probability = TRUE)

# 5. Red neuronal
model_nn <- nnet(type ~ ., data = train_data, size = 5, maxit = 200, trace = FALSE)

# ========================
# Evaluación de modelos
# ========================

accuracy <- function(pred, actual) {
  mean(pred == actual)
}

# Predicciones
pred_tree <- predict(model_tree, test_data, type = "class")
pred_rf   <- predict(model_rf, test_data)
pred_knn  <- predict(model_knn, test_data)
pred_svm  <- predict(model_svm, test_data)
pred_nn   <- predict(model_nn, test_data, type = "class")

# Crear tabla de resultados
results <- data.frame(
  model = c("Decision Tree", "Random Forest", "kNN", "SVM", "Neural Net"),
  accuracy = c(
    accuracy(pred_tree, test_data$type),
    accuracy(pred_rf, test_data$type),
    accuracy(pred_knn, test_data$type),
    accuracy(pred_svm, test_data$type),
    accuracy(pred_nn, test_data$type)
  )
)

# Guardar resultados
write_csv(results, "/data/my-data/data/model_results.csv")
print(results)
