
#### EXPANSION POLINOMIAL DE JULIA ####
library(JuliaCall)
julia_setup()
julia_command('include("POLEXP REV1.jl")')
#### Wrapper del polexp en Julia a R ####
polexpj <- function(df_nombre_en_R, grado = 2) {
  # 1. Pasamos el objeto al entorno de Julia con un nombre fijo
  julia_assign("df_temp_in", df_nombre_en_R)
  # 2. Ejecutamos la expansión dentro de Julia (el ';' evita que Julia intente imprimir el resultado en la consola de R)
  julia_command(sprintf("df_temp_out = polexp(df_temp_in, grado = %d);", grado))
  # 3. Traemos el resultado de vuelta de forma explícita
  res <- julia_eval("df_temp_out")
  # 4. Limpieza opcional en Julia para liberar RAM
  julia_command("df_temp_in = nothing; df_temp_out = nothing; GC.gc();")
  names(res) = normalizar_nombres(names(res))
  return(res)
}


#### FUNCION GENERADORA DE DATOS CEBOLLA NDIMENSIONALES ####

cebollaND <- function(n_samples = 300, n_dims = 2, n_classes = 3, noise = 0.1) {
  points_per_class <- floor(n_samples / n_classes)
  dataset <- data.frame()
  
  for (i in 1:n_classes) {
    # Definir radios para la "capa" actual
    # Clase 1: r entre 0 y 1.5
    # Clase 2: r entre 2 y 3.5
    # Clase 3: r entre 4 y 5.5
    inner_radius <- (i - 1) * 2.0
    outer_radius <- inner_radius + 1.5
    
    # 1. Generar puntos en una hiperesfera unitaria (distribución normal)
    raw_points <- matrix(rnorm(points_per_class * n_dims), ncol = n_dims)
    
    # 2. Normalizar para que tengan norma 1 (estén en la superficie)
    norms <- sqrt(rowSums(raw_points^2))
    unit_points <- raw_points / norms
    
    # 3. Escalar aleatoriamente entre inner_radius y outer_radius
    r <- inner_radius + runif(points_per_class) * (outer_radius - inner_radius)
    scaled_points <- unit_points * r
    
    # 4. Agregar el ruido Gaussiano
    final_points <- scaled_points + matrix(rnorm(points_per_class * n_dims, sd = noise), ncol = n_dims)
    
    # Estructurar en DataFrame
    df_class <- as.data.frame(final_points)
    colnames(df_class) <- paste0("V", 1:n_dims)
    df_class$class <- as.factor(paste0("G_", i))
    
    dataset <- rbind(dataset, df_class)
  }
  
  return(dataset)
}


#### Normalizador de nombres en R ####

normalizar_nombres <- function(vector_nombres) {
  sapply(vector_nombres, function(nom) {
    # 1. Separar por el asterisco
    partes <- unlist(strsplit(nom, "\\*"))
    # 2. Ordenar alfabéticamente
    partes_ordenadas <- sort(partes)
    # 3. Volver a pegar
    paste(partes_ordenadas, collapse = "*")
  }, USE.NAMES = FALSE)
}

#### LDA PURO A MANO ####
LDAM <- function(X, Y){
  
  # Preparación de datos
  X <- as.matrix(X)
  nc <- ncol(X)          # Número de variables
  clas <- unique(Y)      # Clases
  nr <- length(clas)     # Número de clases
  
  # Cálculo de Sw
  medias <- matrix(0, nr, nc)
  Sw <- matrix(0, nc, nc)
  
  for(i in 1:nr){
    mini1 <- X[Y == clas[i], , drop = FALSE]
    # Media de la clase
    medias[i, ] <- colMeans(mini1)
    # Matriz de dispersión de la clase
    Sn <- (t(mini1) - medias[i, ]) %*%
      t(t(mini1) - medias[i, ])
    # Acumulación
    Sw <- Sw + Sn
  }
  # Dispersión total
  m <- colMeans(X)
  St <- (t(X) - m) %*% t(t(X) - m)
  # Dispersión entre clases
  Sb <- St - Sw
  # Diagnóstico inicial
  rangoSw <- qr(Sw)$rank
  condSw <- kappa(Sw)
  estado <- "OK"
  # Revisar si hay singularidades #
  if(rangoSw < nc){
    estado <- "Sw singular"
  } else if(condSw > 1e10){
    estado <- "Sw mal condicionada"
  }
  # Intentar resolver Sw^-1 Sb
  S <- tryCatch(
    solve(Sw) %*% Sb,
    error = function(e) NULL)
  # Si no pudo invertirse
  if(is.null(S)){
    diagnostico <- data.frame(
      estado = "Sw singular",
      rangoSw = rangoSw,
      dimension = nc,
      condicionSw = condSw,
      maxImaginario = NA
    )
    return(list(
      estado = "Sw singular",
      diagnostico = diagnostico,
      Sw = Sw,
      Sb = Sb
    ))
  }
  
  # Eigenvalores y eigenvectores
  A <- eigen(S) # Revisar si hay imaginarios
  maxIm <- max(abs(Im(A$values)))
  if(maxIm > 1e-10){
    if(estado == "OK"){
      estado <- "Eigenvalores complejos"
    } else {
      estado <- paste0(estado," + Eigenvalores complejos")
    }
  }
  # Varianza explicada
  VE <- round(100 *Re(A$values)/sum(Re(A$values)),4)
  # Selección de discriminantes
  
  #### Orddenar explicitamente ####
  orden <- order(Re(A$values), decreasing = TRUE)
  SV <- Re(A$vectors[, orden[1:(nr - 1)], drop = FALSE])
  
  # Proyecciones
  Z <- X %*% SV
  colnames(Z) <- paste0("ND",1:ncol(SV))
  
  # Métrica J de Fisher
  TSb <- sum(diag(t(SV) %*% Sb %*% SV))
  TSw <- sum(diag(t(SV) %*% Sw %*% SV))
  Jtraza <- TSb / TSw
  metricas <- data.frame(
    dw = TSw,
    db = TSb,
    Jmax = Jtraza)
  # Diagnóstico final
  diagnostico <- data.frame(
    estado = estado,
    rangoSw = rangoSw,
    dimension = nc,
    condicionSw = signif(condSw, 5),
    maxImaginario = signif(maxIm, 5)
  )
  # Salida
  resu <- list(
    estado = estado,
    diagnostico = diagnostico,
    varianza = VE,
    coeficientes = SV,
    proyecciones = Z,
    metricas = metricas,
    eigen = A$values,
    Sw = Sw,
    Sb = Sb)
  return(resu)
}
