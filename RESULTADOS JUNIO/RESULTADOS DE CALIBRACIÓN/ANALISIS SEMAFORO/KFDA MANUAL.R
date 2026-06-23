library(kernlab)

# =========================================================================
# ENTRENADOR KFDA OPTIMIZADO
# =========================================================================
kfda_entrenar <- function(K_train, y_train, r = NULL, reg = 1e-6) {
  classes <- levels(as.factor(y_train))
  n_train <- nrow(K_train)
  c       <- length(classes)
  
  if (is.null(r)) { r <- c - 1 }
  
  # 1. Centrado de la matriz de Kernel de Entrenamiento
  one_n <- matrix(1/n_train, n_train, n_train)
  K_c   <- K_train - one_n %*% K_train - K_train %*% one_n + (one_n %*% K_train %*% one_n)
  
  S_B <- matrix(0, n_train, n_train)
  S_W <- matrix(0, n_train, n_train)
  
  # 2. Calcular las matrices de dispersión
  for (cls in classes) {
    idx  <- which(y_train == cls)
    nc   <- length(idx)
    K_cl <- K_c[, idx, drop = FALSE]
    
    mu_c <- rowMeans(K_cl)
    S_B  <- S_B + nc * (mu_c %o% mu_c)
    
    H_c  <- diag(nc) - matrix(1/nc, nc, nc)
    S_W  <- S_W + K_cl %*% H_c %*% t(K_cl)
  }
  
  # 3. Regularización y Descomposición de Cholesky
  S_W_reg <- S_W + reg * diag(n_train)
  L       <- chol(S_W_reg)
  
  S_B_trans <- t(solve(L)) %*% S_B %*% solve(L)
  eig       <- eigen(S_B_trans, symmetric = TRUE)
  
  # 4. Coeficientes de la transformación
  T_matrix <- solve(L) %*% eig$vectors[, 1:r, drop = FALSE]
  
  # 5. Proyectar y forzar orientación estándar (n_train x r)
  Z_train <- t(t(T_matrix) %*% K_c)
  Z_train <- matrix(Z_train, nrow = n_train, ncol = r)
  colnames(Z_train) <- paste0("KFDA_Dim", 1:r)
  
  # 6. Calcular los centroides sobre la matriz orientada
  centroides <- list()
  for (cls in classes) {
    centroides[[cls]] <- colMeans(Z_train[y_train == cls, , drop = FALSE])
  }
  
  return(list(
    Tr          = T_matrix,  
    Z_train     = Z_train,    
    y_train     = y_train,    
    classes     = classes,    
    centroides  = centroides, 
    K_train     = K_train,
    r           = r
  ))
}

# =========================================================================
# PREDICTOR Y PROYECTOR KFDA OPTIMIZADO
# =========================================================================
kfda_predecir <- function(modelo, K_test) {
  T_matrix <- modelo$Tr 
  K_train  <- modelo$K_train 
  n_train  <- nrow(K_train) 
  n_test   <- nrow(K_test)
  r        <- modelo$r
  
  # 1. Centrado estricto de la matriz de Test
  one_n_train <- matrix(1/n_train, n_train, n_train)
  one_n_test  <- matrix(1/n_train, n_test, n_train)
  
  K_test_c <- K_test - one_n_test %*% K_train - K_test %*% one_n_train + (one_n_test %*% K_train %*% one_n_train)
  
  # 2. Proyectar los datos de prueba al espacio discriminante
  Z_test <- t(T_matrix) %*% t(K_test_c)
  # PREVENCIÓN DE BUG: Forzar bidimensionalidad (r x n_test)
  Z_test <- matrix(Z_test, nrow = r, ncol = n_test)
  
  # 3. Clasificación por cercanía al centroide
  predicciones <- character(n_test)
  for (i in 1:n_test) {
    punto_test <- Z_test[, i]
    distancias <- numeric(length(modelo$classes))
    names(distancias) <- modelo$classes
    
    for (cls in modelo$classes) {
      centroide_c <- modelo$centroides[[cls]]
      distancias[cls] <- sum((punto_test - centroide_c)^2)
    }
    predicciones[i] <- names(which.min(distancias))
  }
  
  return(list(
    preds = factor(predicciones, levels = modelo$classes),
    proyecciones = t(Z_test) # Devuelve orientación (n_test x r)
  ))
}