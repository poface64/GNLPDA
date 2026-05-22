
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
#ruta_dataset = "DATASETS/1.4.wines.csv"
#ruta_dataset = "DATASETS/4.2.Ionosphere.csv" 
nombre_actual = splitext(basename(ruta_dataset))[1]

# 3. Configuración
n_corridas_por_instancia = 30;
lda_instancia = LDA_Model_Type();
const MI_LAMBDA = 0.01 #Correccion de parametros calibrados con irace
config_e1 = (n_gen=94, tam_pop=64, p_cruza=0.90, p_mut=0.05)
config_e2 = (n_gen=94, tam_pop=64, p_cruza=0.90, p_mut=0.05)

# 3.1 Fase de calentamiento para Julia #

# --- INICIO DEL WARM-UP ---
println("Precompilar una version Dummy para aprovechar el JIT")
let
    # Datos mínimos para forzar compilación
    X_d = DataFrame(rand(10, 2), :auto)
    y_d = categorical(["A", "A", "B", "B", "C", "C", "A", "B", "C", "A"])
    X_exp_d = polexp(X_d, grado=2)
    # Ejecución rápida de 1 sola corrida con 2 generaciones
    conf_w = (n_gen=2, tam_pop=10, p_cruza=0.9, p_mut=0.05)
    try
        # Llamada a la función principal con carga mínima
        validacion_estadistica_paralela(1, X_exp_d, X_d, y_d, conf_w, conf_w, MI_LAMBDA, lda_instancia)
        println("JIT Ready.")
    catch e
        println("Calentamiento omitido.")
    end
end

# --- FIN DEL WARM-UP ---
println(">>> Procesando dataset: $nombre_actual con $(nthreads()) hilos.")

# 4. Ejecución
df_actual = DataFrame(CSV.File(ruta_dataset));
ncol = size(df_actual, 2);
X1_actual = select(df_actual, Not(ncol));
# Aplicar aquí el escalado
# Selecciona todas las columnas numéricas y aplícales la operación vectorizada
#X1_actual = DataFrames.transform(X1_actual, names(X1_actual) .=> (x -> (x .- mean(x)) ./ std(x)) .=> names(X1_actual));
# Seguir con la logica
y1_actual = categorical(String.(string.(df_actual[!, ncol])));

X_exp = polexp(X1_actual, grado=2);
y_final = coerce(y1_actual, Multiclass);

# Procesamiento pesado
res_30 = validacion_estadistica_paralela(n_corridas_por_instancia, X_exp, X1_actual, y_final, config_e1, config_e2, MI_LAMBDA, lda_instancia)

# 5. Guardar resultados
guardar_resultados_tesis(res_30, nombre_actual)

println(">>> Finalizado: $nombre_actual")