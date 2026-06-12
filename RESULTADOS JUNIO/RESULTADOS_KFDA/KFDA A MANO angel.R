rm(list=ls())

# Cargar la libreria que hace el Kernell #
library(lfda)

# Entrenador del modelo

kfda_entrenar <- function(K_train, y_train, r = NULL, reg = 1e-6) {
  classes <- levels(as.factor(y_train))
  n_train <- nrow(K_train)
  c       <- length(classes)
  
  # Si no se especifica 'r', por teoría de Fisher el máximo espacio discriminante es c - 1
  if (is.null(r)) {
    r <- c - 1
  }
  
  # 1. Centrado de la matriz de Kernel de Entrenamiento
  one_n <- matrix(1/n_train, n_train, n_train)
  K_c   <- K_train - one_n %*% K_train - K_train %*% one_n + (one_n %*% K_train %*% one_n)
  
  S_B <- matrix(0, n_train, n_train)
  S_W <- matrix(0, n_train, n_train)
  
  for (cls in classes) {
    idx  <- which(y_train == cls)
    nc   <- length(idx)
    K_cl <- K_c[, idx, drop = FALSE]
    
    # Matriz entre clases
    mu_c <- rowMeans(K_cl)
    S_B  <- S_B + nc * (mu_c %o% mu_c)
    
    # Matriz dentro de clases
    H_c  <- diag(nc) - matrix(1/nc, nc, nc)
    S_W  <- S_W + K_cl %*% H_c %*% t(K_cl)
  }
  
  # 2. Regularización y Descomposición de Cholesky
  S_W_reg <- S_W + reg * diag(n_train)
  L       <- chol(S_W_reg)
  
  S_B_trans <- t(solve(L)) %*% S_B %*% solve(L)
  eig       <- eigen(S_B_trans, symmetric = TRUE)
  
  # 3. Coeficientes de la transformación (Dim: n_train x r)
  T_matrix <- solve(L) %*% eig$vectors[, 1:r, drop = FALSE]
  
  # 4. Proyectar y TRANSPONER inmediatamente para formato estándar
  # Operación original da (r x n). Al transponer da (n x r)
  Z_train <- t(t(T_matrix) %*% K_c)
  
  # Forzar a que siga siendo una matriz bidimensional aunque r = 1 (Caso c = 2)
  Z_train <- matrix(Z_train, nrow = n_train, ncol = r)
  colnames(Z_train) <- paste0("KFDA_Dim", 1:r)
  
  # 5. Calcular los centroides sobre la matriz ya orientada (filas = sujetos)
  centroides <- list()
  for (cls in classes) {
    # Como ahora las filas son sujetos, indexamos las filas que pertenecen a la clase
    centroides[[cls]] <- colMeans(Z_train[y_train == cls, , drop = FALSE])
  }
  
  # Devolver objeto del modelo con todo lo necesario para predecir
  # RECOMENDACIÓN DE DOCUMENTACIÓN DE RETORNO:
  return(list(
    Tr          = T_matrix,  # Matriz de coeficientes de transformación
    Z_train     = Z_train,    # Datos de entrenamiento proyectados (n_train x r)
    y_train     = y_train,    # Etiquetas reales de entrenamiento (Ground Truth)
    classes     = classes,    # Niveles/Etiquetas de las clases del dataset
    centroides  = centroides, # Centroides de clase en el espacio proyectado
    K_train     = K_train     # Matriz de kernel original de entrenamiento
  ))
}

kfda_predecir <- function(modelo, K_test) {
  # Asignar los objetos presentes en el modelo
  T_matrix <- modelo$Tr # Matriz de pesos
  K_train  <- modelo$K_train # Matriz Kernel
  n_train  <- nrow(K_train) # Entrenamiento
  n_test   <- nrow(K_test) # Prueba
  
  # 1. Centrado estricto de la matriz de Test usando las medias de Entrenamiento
  # (Fórmula matemática para mapear correctamente datos fuera de la muestra)
  one_n_train <- matrix(1/n_train, n_train, n_train)
  one_n_test  <- matrix(1/n_train, n_test, n_train)
  
  K_test_c <- K_test - one_n_test %*% K_train - K_test %*% one_n_train + (one_n_test %*% K_train %*% one_n_train)
  
  # 2. Proyectar los datos de prueba al espacio discriminante (Dim: r x n_test)
  Z_test <- t(T_matrix) %*% t(K_test_c)
  
  # 3. Clasificación por cercanía al centroide (Criterio de distancia mínima)
  predicciones <- character(n_test)
  # Para cada uno de los puntos
  for (i in 1:n_test) {
    punto_test <- Z_test[, i]
    distancias <- numeric(length(modelo$classes))
    names(distancias) <- modelo$classes
    
    for (cls in modelo$classes) {
      centroide_c <- modelo$centroides[[cls]]
      # Distancia Euclidiana en el espacio ya optimizado por Fisher
      distancias[cls] <- sum((punto_test - centroide_c)^2)
    }
    
    # Asignar la clase con la distancia mínima
    predicciones[i] <- names(which.min(distancias))
  }

  return(list(preds =factor(predicciones, levels = modelo$classes),
              proyecciones = t(Z_test)))
}

### Uso sobre iris ###


library(kernlab) # Solo para calcular matrices de kernel de forma estándar

# 1. Simular una partición 70/30
set.seed(42)
idx_train <- sample(1:nrow(iris), 0.70 * nrow(iris))

train_data <- iris[idx_train, -5]
train_y    <- iris[idx_train, 5]
test_data  <- iris[-idx_train, -5]
test_y     <- iris[-idx_train, 5]

# 2. Calcular matrices de Kernel usando una escala sigma = 0.5
rbf <- rbfdot(sigma = 1)
K_train <- kernelMatrix(rbf, as.matrix(train_data))
K_test  = kernelMatrix(rbf, as.matrix(test_data), as.matrix(train_data)) # n_test x n_train

# 3. Entrenar Framework KFDA Manual
modelo_kfda <- kfda_entrenar(K_train, train_y)
plot(modelo_kfda$Z_train,pch = 16,
     col = train_y)

# 4. Predecir sobre el 30% oculto
predicciones_train <- kfda_predecir(modelo_kfda, K_train)
sum(predicciones_train$preds!=train_y)
predicciones_test <- kfda_predecir(modelo_kfda, K_test)
sum(predicciones_test$preds!=test_y)
plot(predicciones_test$proyecciones,pch = 16,
     col = test_y)



















#### Replicar sobre otra base más real ####

ruta = "C:\\Users\\Angeal\\Desktop\\GNLPDA\\RESULTADOS JUNIO\\DATASETS\\3.2.Cebolla10D.csv"
ds = read.csv(ruta)

# 1. Simular una partición 70/30
set.seed(42)
idx_train <- sample(1:nrow(ds), 0.70 * nrow(ds))
train_data <- ds[idx_train, -ncol(ds)]
train_y    <- ds[idx_train, ncol(ds)]
test_data  <- ds[-idx_train, -ncol(ds)]
test_y     <- ds[-idx_train, ncol(ds)]

# 2. Calcular matrices de Kernel usando una escala sigma = 0.5
# Calcula un rango estimado de sigma (bajo, medio, alto) basados en los datos
estimacion <- sigest(as.matrix(train_data))
sigma_inicial <- estimacion[2] # Tomar el valor medio

rbf <- rbfdot(sigma = sigma_inicial)
K_train <- kernelMatrix(rbf, as.matrix(train_data))
K_test  = kernelMatrix(rbf, as.matrix(test_data), as.matrix(train_data)) # n_test x n_train

# 3. Entrenar Framework KFDA Manual
modelo_kfda <- kfda_entrenar(K_train, train_y)
plot(modelo_kfda$Z_train,pch = 16,
     col = factor(train_y))

# 4. Predecir sobre el 30% oculto
predicciones_train <- kfda_predecir(modelo_kfda, K_train)
sum(predicciones_train$preds!=train_y)
predicciones_test <- kfda_predecir(modelo_kfda, K_test)
round(100*(1-sum(predicciones_test$preds!=test_y)/length(test_y)),2)
if(length(unique(train_y))<=2){
  histogram(test_data[,1],
            col = factor(test_y))
}else{
  plot(predicciones_test$proyecciones,pch = 16,
       col = factor(test_y))
}


library(kernlab)

# 1. Entrenar el referente oficial (KPCA) con tus mismos datos
kpca_oficial <- kpca(as.matrix(train_data), kernel = "rbfdot", 
                     kpar = list(sigma = sigma_inicial), features = 2)

# 2. Proyectar los datos de entrenamiento y prueba
Z_train_referente <- rotated(kpca_oficial)
Z_test_referente  <- predict(kpca_oficial, as.matrix(test_data))

# 3. Graficar el referente para comparar la estructura de los puntos
plot(Z_train_referente[,1:2],
     col = factor(train_y), pch = 16, 
     main = "Referente Oficial: Kernel PCA")



