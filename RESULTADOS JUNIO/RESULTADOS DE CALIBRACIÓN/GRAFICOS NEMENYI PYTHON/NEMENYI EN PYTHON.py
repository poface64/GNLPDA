import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scikit_posthocs as sp
from scipy.stats import friedmanchisquare, rankdata
from scikit_posthocs import critical_difference_diagram
import glob
import os

# ==============================================================================
# 1. CONFIGURACIÓN DE RUTAS Y LÓGICA DE FILTRADO (7 MÉTODOS)
# ==============================================================================

ruta_origen = r"C:\Users\Angeal\Desktop\GNLPDA\RESULTADOS JUNIO\RESULTADOS DE CALIBRACIÓN\CONSOLIDADO_GRAFICOS"
ruta_base_destino = r"C:\Users\Angeal\Desktop\GNLPDA\RESULTADOS JUNIO\RESULTADOS DE CALIBRACIÓN\GRAFICOS NEMENYI PYTHON"

# ------------------------------------------------------------------------------
# INTERRUPTOR DE CONTROL Y ASIGNACIÓN DINÁMICA DE CARPETA
# ------------------------------------------------------------------------------
MODO_CAMPEONES = True  # False -> Nemenyi Globales | True -> Nemenyi Campeones
CAMPEONES = ["GNLPDA", "SVM_POLY2", "SVM_POLY3",
              "SVM_RBF","KFDA"] # Definir aquí los métodos a conservar
if MODO_CAMPEONES:
    subcarpeta = "Nemenyi Campeones"
    sufijo_modo = "campeones"
else:
    subcarpeta = "Nemenyi Globales"
    sufijo_modo = "todos"

ruta_destino = os.path.join(ruta_base_destino, subcarpeta)
os.makedirs(ruta_destino, exist_ok=True)

# ------------------------------------------------------------------------------
# DICCIONARIO MAESTRO DE ETIQUETAS
# ------------------------------------------------------------------------------
diccionario_etiquetas = {
    "GNLPDA": "GNLPDA",
    "LDA": "LDA",
    "QDA": "QDA",
    "KFDA": "KFDA",
    "SVM_POLY2": "SVM(POL2)",
    "SVM_POLY3": "SVM(POL3)",
    "SVM_RBF": "SVM(Gauss)"
}

# ==============================================================================
# 2. FUNCIONES AUXILIARES (LETRA ESTADÍSTICA Y REDONDEO NUMÉRICO DE P-VALORES)
# ==============================================================================
def procesar_p_valor_numerico(p_val):
    if p_val < 0.001:
        return 0.001
    else:
        return round(float(p_val), 3)

def obtener_letras_significancia(nombres_metodos, avg_ranks, nemenyi_matrix, alpha=0.05):
    metodos_ordenados = avg_ranks.sort_values().index.tolist()
    n = len(metodos_ordenados)
    letras_dict = {m: "" for m in metodos_ordenados}
    letras_disponibles = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
    grupo_actual_idx = 0
    
    i = 0
    while i < n:
        if grupo_actual_idx >= len(letras_disponibles):
            break
        letra_actual = letras_disponibles[grupo_actual_idx]
        letras_dict[metodos_ordenados[i]] += letra_actual
        
        for j in range(i + 1, n):
            m1 = metodos_ordenados[i]
            m2 = metodos_ordenados[j]
            p_val = nemenyi_matrix.loc[m1, m2]
            if p_val >= alpha:
                letras_dict[m2] += letra_actual
                
        grupo_actual_idx += 1
        i += 1
        
    return letras_dict

# ==============================================================================
# 3. PROCESAMIENTO RECURSIVO POR DATASET
# ==============================================================================

patron_busqueda = os.path.join(ruta_origen, "*.csv")
archivos_datasets = glob.glob(patron_busqueda)

if not archivos_datasets:
    raise FileNotFoundError(f"No se encontraron archivos .csv en la ruta especificada: {ruta_origen}")

print("=" * 80)
print(f"Iniciando análisis individual para {len(archivos_datasets)} datasets con 7 métodos.")
print(f"Ruta de almacenamiento asignada de forma dinámica:\n--> {ruta_destino}")
print("=" * 80 + "\n")

resultados_friedman_global = []
registros_parejas_global = []
registros_tabla_letras = [] 

for ruta_archivo in archivos_datasets:
    nombre_dataset = os.path.basename(ruta_archivo).replace(".csv", "")
    print(f"Procesando Dataset: {nombre_dataset}")
    
    df_raw = pd.read_csv(ruta_archivo)
    
    # Exclusión excepcional de QDA para Ionosfera
    if "ionosphere" in nombre_dataset.lower():
        df_raw = df_raw[df_raw["Metodo"] != "QDA"]
        print("  [INFO] Filtro excepcional aplicado: Se omitió QDA para 'ionosphere'.")
    
    if MODO_CAMPEONES:
        df_raw = df_raw[df_raw["Metodo"].isin(CAMPEONES)]
        
    df_raw["Metodo"] = df_raw["Metodo"].map(diccionario_etiquetas).fillna(df_raw["Metodo"])
        
    df_pivot = df_raw.pivot(index="Bloque", columns="Metodo", values="Accuracy").dropna()
    nombres_metodos = df_pivot.columns.tolist()
    matrix_rendimiento = df_pivot.to_numpy()
    
    if matrix_rendimiento.shape[1] < 2:
        print(f"[SKIP] {nombre_dataset} no cuenta con suficientes métodos.\n")
        continue

    # --------------------------------------------------------------------------
    # 3.1 PRUEBA DE FRIEDMAN (CON GRADOS DE LIBERTAD k - 1)
    # --------------------------------------------------------------------------
    try:
        stat_acc, p_acc = friedmanchisquare(*[matrix_rendimiento[:, i] for i in range(matrix_rendimiento.shape[1])])
        es_significativo = p_acc < 0.05
        grados_libertad = matrix_rendimiento.shape[1] - 1
    except ValueError as e:
        print(f"[ERROR] No se pudo computar Friedman en {nombre_dataset}. Detalles: {e}\n")
        continue

    resultados_friedman_global.append({
        "Dataset": nombre_dataset,
        "Estadistico_Friedman": round(stat_acc, 4),
        "Grados_Libertad": grados_libertad,
        "p-valor": procesar_p_valor_numerico(p_acc),
        "Significativo_Alfa_0.05": "Sí" if es_significativo else "No"
    })
    
    ranks_acc = np.array([rankdata(-row, method="average") for row in matrix_rendimiento])
    avg_ranks_acc = pd.Series(ranks_acc.mean(axis=0), index=nombres_metodos)
    
    if es_significativo:
        # ----------------------------------------------------------------------
        # 3.2 PRUEBA POST-HOC DE NEMENYI
        # ----------------------------------------------------------------------
        nemenyi_acc = sp.posthoc_nemenyi_friedman(df_pivot)
        
        nombre_csv_nemenyi = f"Nemenyi_Matrix_{nombre_dataset}_{sufijo_modo}.csv"
        nemenyi_acc.map(procesar_p_valor_numerico).to_csv(os.path.join(ruta_destino, nombre_csv_nemenyi), index=True)
        
        diccionario_letras = obtener_letras_significancia(nombres_metodos, avg_ranks_acc, nemenyi_acc, alpha=0.05)
        
        for i in range(len(nombres_metodos)):
            for j in range(i + 1, len(nombres_metodos)):
                m1 = nombres_metodos[i]
                m2 = nombres_metodos[j]
                pareja = f"{m2} vs {m1}" if m2 == "GNLPDA" else f"{m1} vs {m2}"
                registros_parejas_global.append({
                    "Pareja": pareja, "Dataset": nombre_dataset, "p-value": procesar_p_valor_numerico(nemenyi_acc.iloc[i, j])
                })
        
        for m in nombres_metodos:
            rango_str = f"{avg_ranks_acc[m]:.2f} ({diccionario_letras[m]})"
            registros_tabla_letras.append({
                "Metodo": m, "Dataset": nombre_dataset, "Rango_Letras": rango_str
            })
            
        # ----------------------------------------------------------------------
        # 3.3 GRÁFICO DE DIFERENCIA CRÍTICA (Optimización Cromática Bicromática)
        # ----------------------------------------------------------------------
        fig, ax = plt.subplots(figsize=(7.5, 3.2))
        
        # Mapear paleta: Todos los métodos reciben #043B7B por defecto
        paleta_colores = {metodo: "#043B7B" for metodo in nombres_metodos}
        
        # Sobreescribir únicamente la propuesta GNLPDA con #02963E para romper simetría visual
        if "GNLPDA" in paleta_colores:
            paleta_colores["GNLPDA"] = "#02963E"
            
        critical_difference_diagram(
            ranks=avg_ranks_acc, sig_matrix=nemenyi_acc, label_props={"fontsize": 9.5},
            marker_props={"s": 45}, label_fmt_left="{label} ({rank:.2f})",
            color_palette=paleta_colores, left_only=True
        )
        ax = plt.gca()
        ax.xaxis.grid(False)
        plt.tight_layout()
        
        nombre_grafico = f"CDplot_Nemenyi_{nombre_dataset}_{sufijo_modo}.png"
        plt.savefig(os.path.join(ruta_destino, nombre_grafico), dpi=300)
        plt.close()
        print(f"  [OK] Archivos individuales exportados con éxito.")
    else:
        print("  -> Sin diferencias significativas. Reportando rangos puros sin letras.")
        for m in nombres_metodos:
            registros_tabla_letras.append({
                "Metodo": m, "Dataset": nombre_dataset, "Rango_Letras": f"{avg_ranks_acc[m]:.2f} (-)"
            })

# ==============================================================================
# 4. EXPORTACIÓN DE REPORTES GLOBALES CONSOLIDADOS (VALORES NUMÉRICOS)
# ==============================================================================

if resultados_friedman_global:
    pd.DataFrame(resultados_friedman_global).to_csv(
        os.path.join(ruta_destino, f"Resumen_Friedman_Datasets_{sufijo_modo}.csv"), index=False
    )

if registros_tabla_letras:
    df_letras_largo = pd.DataFrame(registros_tabla_letras)
    df_letras_compacto = df_letras_largo.pivot(index="Metodo", columns="Dataset", values="Rango_Letras")
    
    if "GNLPDA" in df_letras_compacto.index:
        orden_filas = ["GNLPDA"] + [m for m in df_letras_compacto.index if m != "GNLPDA"]
        df_letras_compacto = df_letras_compacto.reindex(orden_filas)
        
    ruta_letras_csv = os.path.join(ruta_destino, f"Resumen_Rangos_Letras_Nemenyi_{sufijo_modo}.csv")
    df_letras_compacto.to_csv(ruta_letras_csv, index=True)

if registros_parejas_global:
    df_parejas_largo = pd.DataFrame(registros_parejas_global)
    df_nemenyi_compacto = df_parejas_largo.pivot(index="Pareja", columns="Dataset", values="p-value")
    ruta_compacta = os.path.join(ruta_destino, f"Resumen_P_Values_Nemenyi_Compacto_{sufijo_modo}.csv")
    df_nemenyi_compacto.to_csv(ruta_compacta, index=True)

print("\n" + "=" * 80)
print(f"[INFO] Ejecución concluida con éxito. Archivos refinados guardados en:")
print(f"       --> {ruta_destino}")
print("=" * 80)

