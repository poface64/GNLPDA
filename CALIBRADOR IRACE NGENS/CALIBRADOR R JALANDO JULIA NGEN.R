# Limpiar todo el entorno #
rm(list=ls())

# ==========================================
#### CALIBRADOR DEFINITIVO EN UBUNTU ####
# ==========================================
library(irace)

#### Definir la función objetivo para IRACE ####

#### Definir la función objetivo del Irace  ####
target_runner_tesis <- function(experiment, scenario) {
  # 1. Normalizar y ESCAPAR las rutas para que los espacios no rompan el comando
  path_script <- shQuote(normalizePath("bridge_tesis.jl", mustWork = TRUE))
  path_inst   <- shQuote(normalizePath(experiment$instance, mustWork = TRUE))
  # 2.- Armar la instrucción  para lanzar las corridas a la linea de comandos 
  args <- c("--project=@Angel", 
            path_script,  # Ahora va entre comillas
            # Aquí podria poner el calibrador del ngen
            "--p_mut", experiment$configuration[["p_mut"]],
            "--pop", experiment$configuration[["popSize"]],
            "--p_cruza", experiment$configuration[["p_cruza"]],
            "--lambda", experiment$configuration[["lambda"]],
            "--n_gen", experiment$configuration[["n_gen"]],
            "--inst", path_inst) # Ya tiene shQuote
  
  res <- system2("julia", 
                 args = args, 
                 stdout = TRUE, 
                 stderr = TRUE, 
                 env = "LD_LIBRARY_PATH=") 
  # Verificación de ejecución (Debug)
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
    cat("\n--- ERROR DETECTADO EN JULIA ---\n")
    cat(res, sep = "\n")
    return(list(cost = 1e10))
  }
  # Procesar el costo
  res_clean <- trimws(res)
  res_clean <- res_clean[res_clean != ""]
  costo <- as.numeric(tail(res_clean, 1))
  # Si la cosa falla, devuelve un valor gigante
  if (is.na(costo)) return(list(cost = 1234e10))
  # Devolver el resultado 
  return(list(cost = costo))
}


#### Parametros y rangos a calibrar ####
parameters <- readParameters(text = '
  p_mut    "" r (0.05, 0.2)
  popSize  "" i (20, 100)
  p_cruza  "" r (0.4, 0.95)
  lambda   "" r (0.01, 0.2)
  n_gen    "" i (20, 100) ')

#### Definir la ruta exacta donde esta funcionando ####
# getwd()
setwd("/home/angel/Desktop/GNLPDA REV1/CALIBRADOR IRACE NGENS") # Compu escritorio
#setwd("/home/angeal/Desktop/GNLPDA REV1/CALIBRADOR IRACE NGENS")

### Definir de donde va a obtener las rutas de los datasets ####
archivos_csv = list.files("DATASETS", pattern = "\\.csv$", full.names = TRUE)
if(identical(archivos_csv, character(0))){
  # Buscar en la carpeta superior
  archivos_csv = list.files("../DATASETS", pattern = "\\.csv$", full.names = TRUE)
};archivos_csv

# Verificación rápida para tu tranquilidad
if (length(archivos_csv) == 0) {
  stop("¡Ojo! No se encontraron archivos CSV en la carpeta ../DATASETS")
} else {
  cat("Se encontraron", length(archivos_csv), "instancias.\n")
}

#### Armar el escenario para correr las pruebas ####

scenario <- list(
  targetRunner = target_runner_tesis, # La función que ya corregimos
  instances = archivos_csv,
  parameters = parameters,
  maxExperiments = 3000,               # Ajusta según el tiempo (500 es buen número para tesis)
  parallel = 11,                      # Usaremos 10 hilos para dejar uno libre al sistema
  logFile = "calibracion_final_GAN.Rdata"
)

#### Verificar que funciona el scenario ####

checkIraceScenario(scenario)


#### Iniciar la calibración de parametros ####

irace_results <- irace(scenario = scenario)

#### Explorar los parametros ####

# Ver los mejores parámetros encontrados
best_config <- iraceResults
print(best_config)

# Cargar librería de apoyo si no la tienes
# install.packages("iraceplot") 
library(iraceplot)

# Gráfico de cajas para ver cómo evolucionaron los parámetros
parallel_coord(iraceResults)
iraceResults$allConfigurations[iraceResults$iterationElites,]


# Extraer la mejor configuración (la número 1 de los élites)
print(best_config)

# Guardar los resultados
write.csv(best_config,"ResultadosTODALANOCHE2xd.csv",row.names = F)




