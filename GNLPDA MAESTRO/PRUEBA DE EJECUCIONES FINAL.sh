
julia --project=@Angel -t 11

#### Librerias necesarias para la expansión ####
using DataFrames
using Statistics
using Combinatorics
using Distributions
using MultivariateStats
using MLJ
using CategoricalArrays
using CSV
using Base.Threads
LDA_Model_Type = @load LDA pkg=MultivariateStats verbosity=0
const MI_LAMBDA = 0.0012

#### Cargar el codigo con las funciones necesarias ####


ruta = "/home/jesus/Desktop/GPLDA/FUNCIONES BASE 10.jl"
include(ruta)

#### Ruta donde se encuentran los datos ####
# Cargar el codigo con las funciones necesarias #
rutad = "/home/jesus/Desktop/GPLDA/DATASETS/";
archivos = filter(f -> endswith(lowercase(f), ".csv"), readdir(rutad));
rutascsv = joinpath.(rutad, archivos);

# Cargar todos los datasets en un diccionario
# La clave será el nombre del archivo sin extensión
# Cargar todos los datasets en un vector
datasets = [DataFrame(CSV.File(r)) for r in rutascsv];
# Guardar también los nombres de los datasets
nombres = [splitext(basename(r))[1] for r in rutascsv];
nombres

######## Bucle perron ###########

# ==============================================================================
# BUCLE MAESTRO PARA PROCESAR TODOS LOS DATASETS
# ==============================================================================

# Definimos el número de corridas estocásticas por dataset
n_corridas_por_instancia = 30
lda_instancia = LDA_Model_Type()
config_e1 = (n_gen=50, tam_pop=30, p_cruza=0.85, p_mut=0.075)
config_e2 = (n_gen=50, tam_pop=30, p_cruza=0.85, p_mut=0.075)


println(">>> INICIANDO EXPERIMENTACIÓN MASIVA (11 HILOS ACTIVOS)")

# Contenedor para la tabla maestra
tabla_maestra_resultados = []

println(">>> INICIANDO PROCESAMIENTO MASIVO...")

for i in 1:length(datasets)
    nombre_actual = nombres[i]
    println("\n>>> Dataset $i/$(length(datasets)): $nombre_actual")
    
    # --- PROCESO ---
    df_actual = datasets[i]
    ncol = size(df_actual, 2)
    X1_actual = select(df_actual, Not(ncol))
    y1_actual = categorical(String.(string.(df_actual[!, ncol])))
    
    X_exp = polexp(X1_actual, grado=2)
    y_final = coerce(y1_actual, Multiclass)
    
    # --- 30 CORRIDAS PARALELAS ---
    res_30 = validacion_estadistica_paralela(n_corridas_por_instancia, X_exp, X1_actual, y_final, config_e1, config_e2, MI_LAMBDA, lda_instancia);
    
    # --- GUARDAR DETALLE INDIVIDUAL (Seguridad) ---
    guardar_resultados_tesis(res_30, nombre_actual);
    
    # --- AÑADIR A TABLA MAESTRA ---
    push!(tabla_maestra_resultados, preparar_fila_maestra(nombre_actual, res_30));
    # 1. Liberar memoria de los objetos grandes del dataset actual
    df_actual = nothing
    X_exp = nothing
    res_30 = nothing
    
    # 2. Forzar al Garbage Collector de Julia a limpiar TODO
    # GC.gc() busca objetos sin referencia. 
    # Al llamarlo dos veces, nos aseguramos de limpiar referencias circulares.
    GC.gc()
    GC.gc()
    # Reportar el final de la corrida
    println(">>> Memoria liberada tras procesar $nombre_actual")
    println("-"^30)
end

# --- EXPORTAR TABLA MAESTRA FINAL ---
df_final_maestro = DataFrame(tabla_maestra_resultados)
CSV.write("TABLA_MAESTRA_RESULTADOS_TESIS.csv", df_final_maestro)

println("\n" * "╔" * "═"^40 * "╗")
println("║   PROCESO COMPLETADO EXITOSAMENTE      ║")
println("║ Archivo: TABLA_MAESTRA_RESULTADOS.csv  ║")
println("╚" * "═"^40 * "╝")

