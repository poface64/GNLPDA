# bridge_tesis.jl

using ArgParse
using Base.Threads
using CSV
using CategoricalArrays
using Combinatorics
using DataFrames
using Distributions
using IJulia
using MLJ
using MLJMultivariateStatsInterface
using MultivariateStats
using RCall





# 1. Cargar tus funciones y el modelo
include("FUNCIONES BASE 11.jl")
LDA_Model_Type = @load LDA pkg=MultivariateStats verbosity=0
const lda_global = LDA_Model_Type()

# Funcion principal 

function main()
    s = ArgParseSettings()
    @add_arg_table! s begin
        "--p_mut"    arg_type = Float64; required = true
        "--pop"      arg_type = Int;     required = true
        "--p_cruza"  arg_type = Float64; required = true
        "--lambda"   arg_type = Float64; required = true
        "--inst"     arg_type = String;  required = true
        "--n_gen"    arg_type = Int;     required = true  # <--- NUEVO PARÁMETRO
    end
  
    args = parse_args(ARGS, s)

    try
        # 1. Carga y limpieza estricta
        df_actual = DataFrame(CSV.File(args["inst"]))
        dropmissing!(df_actual) # Adiós a los NAs
        
        ncol = size(df_actual, 2)
        X1_actual = select(df_actual, Not(ncol))
        
        # Seleccionar solo numéricas para evitar errores en polexp
        X1_actual = select(X1_actual, findall(col -> eltype(col) <: Number, eachcol(X1_actual)))
        
        y1_actual = categorical(String.(string.(df_actual[!, ncol])))
        y_final = coerce(y1_actual, Multiclass)
        
        # 2. Expansión (polexp ya tiene un filtro de varianza, pero asegúrate)
        X_exp = polexp(X1_actual, grado=2)

        # 3. Configuración
        conf = (n_gen=args["n_gen"], tam_pop=args["pop"], p_cruza=args["p_cruza"], p_mut=args["p_mut"])

        # --- REGLA DE ORO PARA IRACE ---
        # Redirigimos temporalmente el stdout para que los "println" de tu GA 
        # no ensucien lo que R va a leer.
        costo_final = 1.0
        
        # Ejecutamos el GA (usando redirect_stdout para silenciarlo)
        redirect_stdout(devnull) do
            res = ejecutar_gpn_lda(X_exp, X1_actual, y_final, conf, conf, args["lambda"], lda_global)
            costo_final = 1.0 - res["e2"].fitness
        end
        
        # 4. Único valor que verá R
        println(costo_final)

    catch e
        # Si falla, devolvemos un costo penalizado pero no 1.0 exacto 
        # para que irace vea que hay una diferencia.
        println(1.5) 
    end
end

main()