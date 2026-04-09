using DataFrames, Statistics, Combinatorics, Distributions, MultivariateStats
using MLJ, CSV, CategoricalArrays, Base.Threads

# 1. Cargar modelo y funciones
LDA_Model_Type = @load LDA pkg=MultivariateStats verbosity=0
include("FUNCIONES BASE 11.jl") # Asegurarse que esté en la misma carpeta

# 2. Capturar argumento del sistema
if isempty(ARGS)
    error("Error: Se requiere la ruta del dataset.")
end

ruta_dataset = ARGS[1]
nombre_actual = splitext(basename(ruta_dataset))[1]

# 3. Configuración
n_corridas_por_instancia = 30
lda_instancia = LDA_Model_Type()
const MI_LAMBDA = 0.1
config_e1 = (n_gen=50, tam_pop=30, p_cruza=0.75, p_mut=0.1)
config_e2 = (n_gen=50, tam_pop=30, p_cruza=0.75, p_mut=0.1)

println(">>> Procesando dataset: $nombre_actual con $(nthreads()) hilos.")

# 4. Ejecución
df_actual = DataFrame(CSV.File(ruta_dataset))
ncol = size(df_actual, 2)
X1_actual = select(df_actual, Not(ncol))
y1_actual = categorical(String.(string.(df_actual[!, ncol])))

X_exp = polexp(X1_actual, grado=2)
y_final = coerce(y1_actual, Multiclass)

# Procesamiento pesado
res_30 = validacion_estadistica_paralela(n_corridas_por_instancia, X_exp, X1_actual, y_final, config_e1, config_e2, MI_LAMBDA, lda_instancia)

# 5. Guardar resultados
guardar_resultados_tesis(res_30, nombre_actual)

println(">>> Finalizado: $nombre_actual")