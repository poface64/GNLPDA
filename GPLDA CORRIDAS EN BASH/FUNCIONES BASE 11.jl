
#### Función para hacer la expansión de grado 2 ####

function polexp(X::DataFrame; grado=2, solo_numeros=false)
    n_filas, n_cols = size(X)
    nombres_orig = names(X)
    
    # 1. Generar combinaciones de índices para los términos polinomiales
    # Esto incluye términos de grado 1 hasta 'grado'
    indices = []
    for d in 1:grado
        # combinations_with_replacement genera todas las interacciones posibles
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
                counts = Dict()
                for i in idx_group
                    name = nombres_orig[i]
                    counts[name] = get(counts, name, 0) + 1
                end
                
                nombre_formateado = join([v > 1 ? "$k^$v" : "$k" for (k, v) in counts], "*")
                push!(nuevos_nombres, nombre_formateado)
            end
        end
    end
    
    # 4. Retornar como DataFrame
    df_res = DataFrame(columnas_expandidas, :auto)
    if !solo_numeros
        rename!(df_res, nuevos_nombres)
    end
    return df_res
end


#### Generador de datos tipo cebolla ####

using DataFrames, Distributions, LinearAlgebra, Combinatorics

function cebollaND(n_dims; num_classes=3, points_per_class=100, noise=0.1)
    total_points = num_classes * points_per_class
    X_total = Matrix{Float64}(undef, total_points, n_dims)
    # Cambiamos a Vector de Strings para las etiquetas reales
    y_total = Vector{String}(undef, total_points)
    
    current_row = 1
    for class_idx in 0:(num_classes - 1)
        inner_radius = class_idx * 2.0
        outer_radius = inner_radius + 1.5
        
        # Etiqueta de texto descriptiva
        label = "G_$(class_idx + 1)"
        
        for i in 1:points_per_class
            # Generar dirección aleatoria (Normal Estándar)
            punto = rand(Normal(0, 1), n_dims)
            norma = norm(punto)
            punto_unitario = punto ./ (norma + 1e-10)
            
            # Escalar al radio de la "capa de cebolla"
            r = inner_radius + rand() * (outer_radius - inner_radius)
            punto_escalado = punto_unitario .* r
            
            # Agregar ruido y guardar en la matriz pre-asignada
            X_total[current_row, :] = punto_escalado .+ ((rand(n_dims) .- 0.5) .* noise)
            y_total[current_row] = label
            
            current_row += 1
        end
    end
    
    df_x = DataFrame(X_total, [Symbol("dim$i") for i in 1:n_dims])
    return (X = df_x, y = y_total)
end

#### Selector de las variables de la etapa 1 ####

"""
    factory_selector(X_exp::DataFrame, nombres_originales::Vector{String})

Crea el mapa de dependencias y retorna una función optimizada para filtrar columnas
basándose en un vector booleano (genotipo).
"""
function factory_selector(X_exp, nombres_orig)
    n_exp = names(X_exp)
    
    # Creamos una matriz donde cada columna indica QUÉ variables originales componen el término
    # Ejemplo: para el término "Var1*Var2", la columna tendrá true en Var1 y Var2, y false en las demás.
    mapa_componentes = BitMatrix(occursin.(Regex("\\b" * o * "\\b"), e) 
                                 for o in nombres_orig, e in n_exp)
    
    return function(vec_bool)
        # Convertimos el genotipo a BitVector
        bits = BitVector(vec_bool)
        
        # LÓGICA DE ANGEL: Un término se selecciona SI Y SOLO SI 
        # todas sus variables componentes están activas en 'bits'.
        # Matemáticamente: (componentes ⊆ seleccionadas)
        indices_validos = [all(mapa_componentes[getbits, j] for getbits in findall(mapa_componentes[:, j])) 
                           for j in 1:length(n_exp)]
        
        # Pero hay una forma más eficiente en Julia usando álgebra de bits:
        # Un término 'j' es válido si no tiene ningún bit activo que NO esté en 'bits'
        mask_invalida = .!bits
        validos = [!any(mapa_componentes[:, j] .& mask_invalida) for j in 1:length(n_exp)]
        
        return MLJ.selectcols(X_exp, findall(validos))
    end
end


#### Fitness para la primer etapa MEJORADO CON MEMORIA ####

# 1. Carga el modelo LDA EN EL ESPACIO GLOBAL (fuera de cualquier función)
# Esto asegura que el "mundo" de Julia ya lo conozca desde el inicio
using MLJ
LDA_Model_Type = @load LDA pkg=MultivariateStats verbosity=0

"""
    fitness1(seleccionar, y, lambda, modelo_base, usar_f1=false)
"""

### Función de evaluación optimizada ####

function factory_fitness1(seleccionar, y, lambda, modelo_base, metrica_objetivo)
    # Almacén de memoria y contadores internos
    memoria = Dict{BitVector, Float64}()
    hits = 0
    total_llamadas = 0

    return function(individuo; reporte=false)
        # Si se solicita el reporte de desempeño de la memoria
        if reporte 
            return (hits = hits, total = total_llamadas)
        end

        total_llamadas += 1
        ind = BitVector(individuo)

        # Regla de Angel: Mínimo 2 variables
        k = sum(ind)
        n_max = length(ind)
        if k < 2; return 0.0; end 

        # --- MECANISMO DE MEMORIA ---
        if haskey(memoria, ind)
            hits += 1
            return memoria[ind] # Retorna valor almacenado sin evaluar [cite: 199]
        end

        # --- EVALUACIÓN REAL (COSTOSA) ---
        X_sub = seleccionar(ind)
        mach = machine(modelo_base, X_sub, y)
        
        try
            res = evaluate!(mach,
                resampling = CV(nfolds=5, shuffle=true, rng=0503),
                measures = [metrica_objetivo],
                operation = predict_mode,
                verbosity = 0
            )
            # Aquí puedo cambiar la metrica
            val_fitness = res.measurement[1] - lambda*(k/n_max)
            
            # Guardar en memoria para futuras consultas [cite: 198]
            memoria[copy(ind)] = val_fitness
            
            return val_fitness
        catch e
            return 0.0
        end
    end
end

#### generador de la población inicial ####

"""
    generar_poblacion(n_sujetos, n_vars_orig)

Crea una matriz de (n_sujetos x n_vars_orig) con al menos 2 bits activos por fila.
"""
function generar_poblacion(n_sujetos, n_vars_orig)
    poblacion = BitMatrix(undef, n_sujetos, n_vars_orig)
    
    for i in 1:n_sujetos
        # Generar un vector aleatorio
        ind = rand(Bool, n_vars_orig)
        
        # Si no cumple la regla de Angel (al menos 2), forzamos 2 bits al azar
        while sum(ind) < 2
            indices = rand(1:n_vars_orig, 2)
            ind[indices] .= true
        end
        poblacion[i, :] = ind
    end
    return poblacion
end

#### 4.- Operador cruza de un punto ####

# Cruza de un punto (One-point Crossover)
function cruzar(p1::BitVector, p2::BitVector)
    punto = rand(1:length(p1)-1)
    return vcat(p1[1:punto], p2[punto+1:end]), vcat(p2[1:punto], p1[punto+1:end])
end

#### 5.- Mutación tipo bitflip y correccion ####
# Mutación Bitflip Vectorizada (Máscara XOR)
function mutar!(ind::BitVector, p_mut::Float64)
    ind .= ind .⊻ (rand(length(ind)) .< p_mut)
    if sum(ind) < 2  # Regla de Angel
        ind[rand(1:length(ind), 2)] .= true
    end
end

#### Selección por Torneo Binario ####
function torneo(poblacion, scores)
    i, j = rand(1:length(scores), 2)
    return scores[i] > scores[j] ? poblacion[i, :] : poblacion[j, :]
end

#### 6.- Logica completa del ciclo ####

function ejecutar_experimento(evaluador, n_vars, conf)
    # Inicialización
    poblacion = generar_poblacion(conf.tam_pop, n_vars)
    p_mut = conf.p_mut
    historia = Float64[]
    
    println("--- Iniciando GA (p_mut = $p_mut) ---")
    
    time_total = @elapsed for gen in 1:conf.n_gen
        # A. Evaluación
        scores = [evaluador(poblacion[i, :]) for i in 1:conf.tam_pop]
        
        # B. Elitismo (Top 2)
        idxs = sortperm(scores, rev=true)
        mejor_sujeto_gen = poblacion[idxs[1], :] # El mejor de ESTA generación
        push!(historia, scores[idxs[1]])
        
        # C. Nueva Generación
        nueva_pop = BitMatrix(undef, conf.tam_pop, n_vars)
        nueva_pop[1, :] = mejor_sujeto_gen # Elite 1
        nueva_pop[2, :] = poblacion[idxs[2], :]  # Elite 2
        
        for i in 3:2:conf.tam_pop
            p1, p2 = torneo(poblacion, scores), torneo(poblacion, scores)
            h1, h2 = rand() < conf.p_cruza ? cruzar(p1, p2) : (copy(p1), copy(p2))
            
            mutar!(h1, p_mut)
            mutar!(h2, p_mut)
            
            nueva_pop[i, :] = h1
            if i + 1 <= conf.tam_pop; nueva_pop[i+1, :] = h2; end
        end
        poblacion = nueva_pop
        
        # Reporte detallado solicitado
        if gen % 10 == 0
            n_activas = sum(mejor_sujeto_gen)
            println("Gen $gen | Mejor Fitness: $(round(historia[end], digits=4)) | Vars: $n_activas")
        end
    end
    
    return poblacion[1, :], historia, time_total
end



### Preprocesado para el factory etapa 2 ###

# Versión con Memoria para Etapa 2
function factory_evaluador_etapa2(X_etapa2, y, lda_instancia, MI_LAMBDA, metrica)
    # Almacén de memoria y contadores
    memoria = Dict{BitVector, Float64}()
    hits = 0
    total_llamadas = 0

    return function(individuo_bits; reporte=false)
        # Si el GA pide el reporte al final, devolvemos las estadísticas
        if reporte 
            return (hits = hits, total = total_llamadas)
        end

        total_llamadas += 1
        ind = BitVector(individuo_bits)
        
        # Regla de Angel
        k = sum(ind)
        n_max = length(ind)
        if k < 2; return 0.0; end

        # --- CONSULTA DE MEMORIA ---
        if haskey(memoria, ind)
            hits += 1
            return memoria[ind]
        end

        # --- EVALUACIÓN REAL ---
        X_sub = X_etapa2[:, ind]
        mach = machine(lda_instancia, X_sub, y)
        
        try
            res = evaluate!(mach,
                resampling = CV(nfolds=5, shuffle=true, rng=0503),
                measures = [metrica],
                operation = predict_mode,
                verbosity = 0
            )
            val_fitness = res.measurement[1] - MI_LAMBDA*(k/n_max)
            memoria[copy(ind)] = val_fitness
            return val_fitness
        catch
            return 0.0
        end
    end
end


#### Encapsulado del proceso en 2 partes como una sola ####


function ejecutar_gpn_lda(X_pool, X_orig, y_labels, config_e1, config_e2, lambda, modelo)
    # --- RESULTADOS A REPORTAR ---
    resultados = Dict()
    
    # ==========================================================================
    # ETAPA 1: MACRO-SELECCIÓN (Variables Originales)
    # ==========================================================================
    seleccionar_e1 = factory_selector(X_pool, names(X_orig))
    evaluador_e1   = factory_fitness1(seleccionar_e1, y_labels, lambda, modelo, accuracy)
    
    # Ejecución
    mejor_ind_e1, curva1, t_e1 = ejecutar_experimento(evaluador_e1, size(X_orig, 2), config_e1)
    
    # Métricas E1
    stats1 = evaluador_e1(BitVector([]); reporte=true)
    
    resultados["e1"] = (
        mejor_ind = mejor_ind_e1,
        n_vars    = sum(mejor_ind_e1),
        nombres   = names(X_orig)[mejor_ind_e1],
        tiempo    = t_e1,
        fitness   = curva1[end],
        memoria   = stats1
    )

    # ==========================================================================
    # ETAPA 2: MICRO-SELECCIÓN (Términos Polinomiales)
    # ==========================================================================
    # Espacio restringido según la visión de Angel
    X_sub_exp = seleccionar_e1(mejor_ind_e1)
    n_vars_e2 = size(X_sub_exp, 2)
    
    evaluador_e2 = factory_evaluador_etapa2(X_sub_exp, y_labels, modelo, lambda, accuracy)
    
    # Ejecución
    mejor_ind_e2, curva2, t_e2 = ejecutar_experimento(evaluador_e2, n_vars_e2, config_e2)
    
    # Métricas E2
    stats2 = evaluador_e2(BitVector([]); reporte=true)
    
    resultados["e2"] = (
        mejor_ind = mejor_ind_e2,
        n_vars    = sum(mejor_ind_e2),
        nombres   = names(X_sub_exp)[mejor_ind_e2],
        df_final  = X_sub_exp[:, mejor_ind_e2], # El dataset final optimizado
        tiempo    = t_e2,
        fitness   = curva2[end],
        memoria   = stats2
    )
    
    return resultados
end



#### Función para paralelizar las 30 corridas ####

using Base.Threads
using Statistics

function validacion_estadistica_paralela(n_corridas, X_pool, X_orig, y_labels, config_e1, config_e2, lambda, modelo)
    
    # 1. Pre-asignar contenedores para los resultados (Thread-safe)
    lista_resultados = Vector{Any}(undef, n_corridas)
    
    println(">>> Iniciando $n_corridas corridas en paralelo usando $(Threads.nthreads()) hilos...")
    
    # 2. Bucle Paralelizado
    # Ajuste en el bucle @threads para incluir Recall, Precision y Nombres
    @threads for i in 1:n_corridas
        res_gpn = ejecutar_gpn_lda(X_pool, X_orig, y_labels, config_e1, config_e2, lambda, modelo)
        
        X_final = res_gpn["e2"].df_final
        mach_final = machine(modelo, X_final, y_labels)
        
        eval_ext = evaluate!(mach_final,
            resampling = CV(nfolds=10, shuffle=true, rng=i),
            measures = [accuracy, multiclass_f1score, multiclass_recall, multiclass_precision],
            operation = predict_mode,
            verbosity = 0
        )
        
        lista_resultados[i] = (
            acc_externo  = eval_ext.measurement[1],
            f1_externo   = eval_ext.measurement[2],
            rec_externo  = eval_ext.measurement[3],
            prec_externo = eval_ext.measurement[4],
            n_vars_e1    = res_gpn["e1"].n_vars,
            n_vars_e2    = res_gpn["e2"].n_vars,
            # --- NUEVO: Guardar nombres como Strings ---
            nombres_e1   = join(res_gpn["e1"].nombres, ", "),
            nombres_e2   = join(res_gpn["e2"].nombres, ", "),
            # --------------------------------------------
            tiempo_total = res_gpn["e1"].tiempo + res_gpn["e2"].tiempo,
            ahorro_mem   = (res_gpn["e1"].memoria.hits + res_gpn["e2"].memoria.hits) / 
                           (res_gpn["e1"].memoria.total + res_gpn["e2"].memoria.total)
        )
        println("Thread $(threadid()): Corrida $i finalizada.")
    end    
    return lista_resultados
end




#### Pos procesado par aresultados ####

function posprocesar_resultados(lista_resultados, nombres_instancia)
    n = length(lista_resultados)
    
    # 1. Extracción de métricas
    accs  = [r.acc_externo for r in lista_resultados]
    f1s   = [r.f1_externo for r in lista_resultados]
    recs  = [r.rec_externo for r in lista_resultados]
    precs = [r.prec_externo for r in lista_resultados]
    
    tiempos = [r.tiempo_total for r in lista_resultados]
    vars_e1 = [r.n_vars_e1 for r in lista_resultados]
    vars_e2 = [r.n_vars_e2 for r in lista_resultados]
    ahorros = [r.ahorro_mem for r in lista_resultados]

    # 2. Historial de variables para análisis de frecuencia
    historial_vars_e1 = [r.nombres_e1 for r in lista_resultados]
    historial_vars_e2 = [r.nombres_e2 for r in lista_resultados]

    # 3. Función auxiliar para formato Media ± Desviación Estándar
    fmt(v) = "$(round(mean(v), digits=4)) ± $(round(std(v), digits=4))"

    # 4. Reporte Final
    println("\n" * "╔" * "═"^65 * "╗")
    println("║" * " "^20 * "REPORTE ESTADÍSTICO FINAL (N=$n)" * " "^21 * "║")
    println("╠" * "═"^65 * "╣")
    println("  Dataset: $(nombres_instancia)")
    
    println("\n  [DESEMPEÑO DE CLASIFICACIÓN (10-Fold Externo)]")
    println("  - Accuracy:  ", fmt(accs))
    println("  - F1-Score:  ", fmt(f1s))
    println("  - Recall:    ", fmt(recs))
    println("  - Precision: ", fmt(precs))

    println("\n  [COMPLEJIDAD Y EFICIENCIA (Media ± Desviación)]")
    println("  - Tiempo Total:     ", fmt(tiempos), " s")
    println("  - Vars Etapa 1:     ", fmt(vars_e1))
    println("  - Términos Etapa 2: ", fmt(vars_e2))
    
    println("\n  [AHORRO RECURSOS]")
    avg_ahorro = mean(ahorros) * 100
    println("  - Eficiencia Memoria: $(round(avg_ahorro, digits=2))% de ahorro promedio")
    println("╚" * "═"^65 * "╝")

    return (
        metricas = (acc=accs, f1=f1s, rec=recs, prec=precs, t=tiempos, v1=vars_e1, v2=vars_e2),
        evolucion = (e1=historial_vars_e1, e2=historial_vars_e2)
    )
end

#### Preparador de resultados par aexportar ####
function preparar_fila_maestra(nombre, stats_lista)
    # Extraer métricas para cálculo
    accs = [r.acc_externo for r in stats_lista]
    f1s  = [r.f1_externo for r in stats_lista]
    recs = [r.rec_externo for r in stats_lista]
    pres = [r.prec_externo for r in stats_lista]
    ties = [r.tiempo_total for r in stats_lista]
    ve1  = [r.n_vars_e1 for r in stats_lista]
    ve2  = [r.n_vars_e2 for r in stats_lista]
    
    # Formato: Media (Desviación)
    f(v) = "$(round(mean(v), digits=4)) ($(round(std(v), digits=4)))"
    
    return (
        Dataset = nombre,
        Accuracy = f(accs),
        F1_Score = f(f1s),
        Recall = f(recs),
        Precision = f(pres),
        Tiempo_Total = f(ties),
        Vars_Etapa1 = f(ve1),
        Vars_Etapa2 = f(ve2),
        Ahorro_Memoria = "$(round(mean([r.ahorro_mem for r in stats_lista])*100, digits=2))%"
    )
end

#### Guardar resultados de tesis ####



function guardar_resultados_tesis(lista, nombre_dataset)
    # 1. Crear el DataFrame detallado (Asegurando que los nombres coincidan)
    df_detalle = DataFrame([
        (Corrida = i,
         Acc = r.acc_externo, 
         F1 = r.f1_externo, 
         Rec = r.rec_externo,      # <--- Aquí estaba el error si r no tenía este nombre
         Prec = r.prec_externo,
         Vars_E1 = r.n_vars_e1, 
         Nombres_E1 = r.nombres_e1, 
         Vars_E2 = r.n_vars_e2, 
         Nombres_E2 = r.nombres_e2, 
         Tiempo = r.tiempo_total, 
         Ahorro = r.ahorro_mem) for (i, r) in enumerate(lista)
    ])

    archivo_detalle = "Resultados_Detalle_$(nombre_dataset).csv"
    CSV.write(archivo_detalle, df_detalle)
    
    # 2. Resumen Estadístico (Media y Std)
    resumen = DataFrame(
        Metrica = ["Accuracy", "F1-Score", "Recall", "Precision", "Vars_E1", "Vars_E2", "Tiempo"],
        Media = [mean(df_detalle.Acc), mean(df_detalle.F1), mean(df_detalle.Rec), 
                 mean(df_detalle.Prec), mean(df_detalle.Vars_E1), mean(df_detalle.Vars_E2), mean(df_detalle.Tiempo)],
        Std_Dev = [std(df_detalle.Acc), std(df_detalle.F1), std(df_detalle.Rec), 
                   std(df_detalle.Prec), std(df_detalle.Vars_E1), std(df_detalle.Vars_E2), std(df_detalle.Tiempo)]
    )

    archivo_resumen = "Resumen_Estadistico_$(nombre_dataset).csv"
    CSV.write(archivo_resumen, resumen)
    return resumen
end




