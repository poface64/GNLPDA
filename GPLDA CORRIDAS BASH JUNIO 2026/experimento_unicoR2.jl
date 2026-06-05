using DataFrames, Statistics, Combinatorics, Distributions, MultivariateStats
using MLJ, CSV, CategoricalArrays, Base.Threads

# 1. Cargar modelo y funciones
LDA_Model_Type = @load LDA pkg=MultivariateStats verbosity=0
include("FUNCIONES BASE 12.jl") 

# 2. Capturar argumentos del sistema (Ahora requiere 2 rutas)
if length(ARGS) < 2
    error("Error: Se requiere la ruta del dataset y la ruta del archivo de particiones.")
end

ruta_dataset = ARGS[1]
ruta_particiones = ARGS[2]
nombre_actual = splitext(basename(ruta_dataset))[1]

# 3. Configuración
n_corridas_por_instancia = 33 # Se procesan las corridas del 1 al 30
lda_instancia = LDA_Model_Type()
const MI_LAMBDA = 0.01
config_e1 = (n_gen=94, tam_pop=64, p_cruza=0.9, p_mut=0.05)
config_e2 = (n_gen=94, tam_pop=64, p_cruza=0.9, p_mut=0.05)

println(">>> Procesando dataset: $nombre_actual con $(nthreads()) hilos.")

# 4. Carga centralizada en memoria una sola vez
df_actual = DataFrame(CSV.File(ruta_dataset))
df_particiones = DataFrame(CSV.File(ruta_particiones))

# Validar alineación de dimensiones
if nrow(df_actual) != nrow(df_particiones)
    error("Error: El número de filas del dataset y del archivo de particiones no coincide.")
end

# 5. Ejecución del procesamiento pesado pasándole los índices de las particiones
res_30 = validacion_estadistica_paralela(
    n_corridas_por_instancia, df_actual, df_particiones, 
    config_e1, config_e2, MI_LAMBDA, lda_instancia
)

# 6. Guardar resultados
guardar_resultados_tesis(res_30, nombre_actual)

println(">>> Finalizado: $nombre_actual")