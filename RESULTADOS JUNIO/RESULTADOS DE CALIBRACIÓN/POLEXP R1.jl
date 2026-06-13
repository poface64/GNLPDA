#### Función para hacer la expansión de grado 2 con nombres normalizados ####
using Combinatorics, DataFrames, Statistics

function polexp(X::DataFrame; grado=2, solo_numeros=false)
    n_filas, n_cols = size(X)
    nombres_orig = names(X)
    
    # 1. Generar combinaciones de índices para los términos polinomiales
    indices = []
    for d in 1:grado
        append!(indices, collect(with_replacement_combinations(1:n_cols, d)))
    end
    
    # 2. Calcular los valores de las nuevas columnas
    matriz_X = Matrix(X)
    columnas_expandidas = []
    nuevos_nombres = String[]
    
    for idx_group in indices
        # Multiplicar los valores de las columnas indicadas en el grupo
        col_data = ones(n_filas)
        for idx in idx_group
            col_data .*= matriz_X[:, idx]
        end
        
        # 3. Limpieza de varianza (Crucial para estabilidad)
        if var(col_data) > 1e-10
            push!(columnas_expandidas, col_data)
            
            if !solo_numeros
                # Lógica de nombres: "Var1^2*Var2"
                counts = Dict{String, Int}()
                for i in idx_group
                    name = nombres_orig[i]
                    counts[name] = get(counts, name, 0) + 1
                end
                
                # --- CORRECCIÓN: NORMALIZACIÓN ALFABÉTICA ---
                # Ordenamos los nombres de las variables antes de unir las piezas
                nombres_ordenados = sort(collect(keys(counts)))
                
                nombre_formateado = join(
                    [counts[k] > 1 ? "$k^$(counts[k])" : "$k" for k in nombres_ordenados], 
                    "*"
                )
                # --------------------------------------------
                
                push!(nuevos_nombres, nombre_formateado)
            end
        end
    end
    
    # 4. Retornar como DataFrame
    # Usamos hcat para mayor eficiencia al crear el DataFrame desde columnas
    df_res = DataFrame(columnas_expandidas, :auto)
    if !solo_numeros
        rename!(df_res, nuevos_nombres)
    end
    return df_res
end
# df = DataFrame(A = 1:5,B = 11:15)
# df2 = polexp(df)

