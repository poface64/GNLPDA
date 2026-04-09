# ==========================================
# SOLUCIÓN DEFINITIVA DE RUTA
# ==========================================

rm(list=ls())

### DEFINIR LA RUTA DE TRABAJO ###

library(irace)

#setwd("/home/jesus/Desktop/GPLDA/CALIBRACION CORRECCION/")

#### Definir la función objetivo del Irace  ####

target_runner_tesis <- function(experiment, scenario) {
  # 1. Normalizar y ESCAPAR las rutas para que los espacios no rompan el comando
  path_script <- shQuote(normalizePath("bridge_tesis.jl", mustWork = TRUE))
  path_inst   <- shQuote(normalizePath(experiment$instance, mustWork = TRUE))
  
  args <- c("--project=@Angel", 
            path_script,  # Ahora va entre comillas
            "--p_mut", experiment$configuration[["p_mut"]],
            "--pop", experiment$configuration[["popSize"]],
            "--p_cruza", experiment$configuration[["p_cruza"]],
            "--lambda", experiment$configuration[["lambda"]],
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
  
  if (is.na(costo)) return(list(cost = 1e10))
  return(list(cost = costo))
}

#### Definir los parametros ####
# Definición de parámetros (ajustados a tu GA)
parameters <- readParameters(text = '
  p_mut    "" r (0.001, 0.2)
  popSize  "" i (20, 100)
  p_cruza  "" r (0.4, 0.95)
  lambda   "" r (0.1, 0.2)')


#### Armar el scenario ####
archivos_csv = list.files("DATASETS",pattern = "\\.csv$",full.names = T)[10:11]
scenario <- list(
  targetRunner = target_runner_tesis, # La función que ya corregimos
  instances = archivos_csv,
  parameters = parameters,
  maxExperiments = 2000,               # Ajusta según el tiempo (500 es buen número para tesis)
  parallel = 11,                      # Usaremos 10 hilos para dejar uno libre al sistema
  logFile = "calibracion_final_GA3.Rdata"
)



# Iniciar la búsqueda de los parámetros óptimos
#irace_results <- irace(scenario = scenario)

# Ver los mejores parámetros encontrados

best_config <- irace_results
print(best_config)

write.csv(best_config,"ResultadosTODALANOCHE2xd.csv",row.names = F)




#### Debugger ####
archivos_csv


# Ejecuta la función manualmente
resultado <- target_runner_tesis(test_exp, list())
print(resultado)

