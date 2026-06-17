
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scikit_posthocs as sp
from scipy.stats import friedmanchisquare, rankdata
from scikit_posthocs import critical_difference_diagram
import glob
import os

# ==============================================================================
# 1. CONFIGURACIÓN DE RUTAS Y CARGA AUTOMÁTICA DE DATOS (.csv)
# ==============================================================================

# Ruta de origen de los archivos de formato largo por dataset
ruta_origen = r"C:\Users\Angeal\Desktop\GNLPDA\RESULTADOS JUNIO\RESULTADOS DE CALIBRACIÓN\CONSOLIDADO_GRAFICOS"

# Ruta de destino para almacenar el gráfico de diferencia crítica resultante
ruta_destino = r"C:\Users\Angeal\Desktop\GNLPDA\RESULTADOS JUNIO\RESULTADOS DE CALIBRACIÓN\GRAFICOS NEMENYI PYTHON"

# Asegurar que la carpeta de destino exista
os.makedirs(ruta_destino, exist_ok=True)

# Buscar únicamente los archivos con extensión .csv en la ruta de origen
patron_busqueda = os.path.join(ruta_origen, "*.csv")
archivos_datasets = glob.glob(patron_busqueda)

if not archivos_datasets:
    raise FileNotFoundError(
        f"No se encontraron archivos .csv en la ruta especificada: {ruta_origen}"
    )

datos_consolidados = []

for archivo in archivos_datasets:
    # Obtener el nombre del dataset abstrayendo el nombre del archivo sin la extensión
    nombre_dataset = os.path.basename(archivo).replace(".csv", "")

    # Cargar el archivo CSV correspondiente
    df_dataset = pd.read_csv(archivo)

    # Agrupar por la columna 'Metodo' y calcular el promedio del 'Accuracy'
    resumen = (
        df_dataset.groupby("Metodo")["Accuracy"]
        .mean()
        .reset_index()
    )

    # Agregar metadato del nombre del dataset
    resumen["Dataset"] = nombre_dataset

    datos_consolidados.append(resumen)

# Consolidar todos los datasets en un único DataFrame estructurado de forma larga
df_largo = pd.concat(datos_consolidados, axis=0)

# Pivotar los datos para obtener el formato requerido por las pruebas:
# Filas: Datasets | Columnas: Métodos | Valores: Promedio de Accuracy
df_matriz = df_largo.pivot(
    index="Dataset",
    columns="Metodo",
    values="Accuracy"
)

print("=" * 70)
print(f"Se cargaron exitosamente {len(archivos_datasets)} archivos .csv")
print("Matriz de Accuracy consolidada (Promedios por Dataset):")
print(df_matriz)
print("=" * 70 + "\n")

# ==============================================================================
# 2. EJECUCIÓN DE LA PRUEBA DE FRIEDMAN
# ==============================================================================

nombres_metodos = df_matriz.columns.tolist()
accuracy_matrix = df_matriz.to_numpy()

# Evaluar la hipótesis nula mediante Friedman desempaquetando las columnas
stat_acc, p_acc = friedmanchisquare(
    *[
        accuracy_matrix[:, i]
        for i in range(accuracy_matrix.shape[1])
    ]
)

print("Resultado Prueba de Friedman (Accuracy):")
print(f"p-valor = {p_acc:.4e} | Estadístico = {stat_acc:.4f}")

if p_acc < 0.05:
    print(
        "-> Conclusión: Existen diferencias estadísticas significativas (p < 0.05)."
    )
    print(
        "   Se procede a ejecutar la prueba post-hoc de Nemenyi y generar el CD plot.\n"
    )

    # ==========================================================================
    # 3. PRUEBA POST-HOC DE NEMENYI
    # ==========================================================================
    nemenyi_acc = sp.posthoc_nemenyi_friedman(df_matriz)

    print("Matriz de p-valores resultante (Prueba de Nemenyi):")
    print(nemenyi_acc.round(4))
    print("\n" + "-" * 50)

    # ==========================================================================
    # 4. CÁLCULO DE RANGOS PROMEDIO
    # ==========================================================================
    # Se asignan los rangos (mayor Accuracy obtiene el rango 1)
    ranks_acc = np.array([
        rankdata(-row, method="average")
        for row in accuracy_matrix
    ])

    avg_ranks_acc = pd.Series(
        ranks_acc.mean(axis=0),
        index=nombres_metodos
    )

    print("Rangos promedio obtenidos por cada método (Menor rango es mejor):")
    print(avg_ranks_acc.round(4))
    print("-" * 50 + "\n")

    # ==========================================================================
    # 5. GENERACIÓN Y GUARDADO DEL GRÁFICO DE DIFERENCIA CRÍTICA (CD PLOT)
    # ==========================================================================
    fig, ax = plt.subplots(figsize=(7, 2.5))

    # Paleta de colores predefinida para los métodos del ecosistema
    colores_disponibles = [
        "red",
        "blue",
        "green",
        "darkorange",
        "purple",
        "brown",
        "cyan"
    ]

    paleta_colores = {
        metodo: colores_disponibles[i % len(colores_disponibles)]
        for i, metodo in enumerate(nombres_metodos)
    }

    # Destacar GNLPDA si existe
    if "GNLPDA" in paleta_colores:
        paleta_colores["GNLPDA"] = "crimson"

    critical_difference_diagram(
        ranks=avg_ranks_acc,
        sig_matrix=nemenyi_acc,
        label_props={"fontsize": 10},
        marker_props={"s": 40},
        label_fmt_left="{label} ({rank:.2f})",
        color_palette=paleta_colores,
        left_only=True
    )

    # Ajustes estéticos
    ax = plt.gca()
    ax.xaxis.grid(False)

    plt.tight_layout()

    # Definir la ruta completa y el nombre del archivo de salida
    nombre_grafico = "CDplot_Nemenyi_Accuracy.png"
    ruta_salida_completa = os.path.join(
        ruta_destino,
        nombre_grafico
    )

    # Guardar el gráfico en alta resolución
    plt.savefig(ruta_salida_completa, dpi=300)

    # Cerrar la figura para liberar memoria
    plt.close()

    print("[INFO] El flujo ha finalizado correctamente.")
    print(
        f"[INFO] Gráfico exportado con éxito en:\n"
        f"       {ruta_salida_completa}"
    )

else:
    print(
        "-> Conclusión: No se encontraron diferencias estadísticas significativas "
        "(p >= 0.05)."
    )
    print(
        "   No se requiere realizar la comparación post-hoc de Nemenyi ni el gráfico."
    )
    
    
##### Comparativa por dataset ####

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import scikit_posthocs as sp
from scipy.stats import friedmanchisquare, rankdata
from scikit_posthocs import critical_difference_diagram
import glob
import os

# ==============================================================================
# 1. CONFIGURACIÓN DE RUTAS Y MODO DE RENDIMIENTO (LOGICA DE CAMPEONES)
# ==============================================================================

# Ruta de origen de los archivos de formato largo por dataset
ruta_origen = r"C:\Users\Angeal\Desktop\GNLPDA\RESULTADOS JUNIO\RESULTADOS DE CALIBRACIÓN\CONSOLIDADO_GRAFICOS"

# Ruta de destino para almacenar los gráficos resultantes
ruta_destino = r"C:\Users\Angeal\Desktop\GNLPDA\RESULTADOS JUNIO\RESULTADOS DE CALIBRACIÓN\GRAFICOS NEMENYI PYTHON"

os.makedirs(ruta_destino, exist_ok=True)

# ------------------------------------------------------------------------------
# INTERRUPTOR: LÓGICA DE CAMPEONES
# ------------------------------------------------------------------------------
# MODO_CAMPEONES = False -> Evalúa TODOS los métodos presentes en cada archivo CSV.
# MODO_CAMPEONES = True  -> Filtra y evalúa únicamente los métodos listados en 'CAMPEONES'.
MODO_CAMPEONES = True  

CAMPEONES = ["GNLPDA", "SVM_POLY2", "SVM_POLY3",
              "SVM_RBF","KFDA"] # Definir aquí los métodos a conservar

# ==============================================================================
# 2. PROCESAMIENTO RECURSIVO POR DATASET
# ==============================================================================

patron_busqueda = os.path.join(ruta_origen, "*.csv")
archivos_datasets = glob.glob(patron_busqueda)

if not archivos_datasets:
    raise FileNotFoundError(f"No se encontraron archivos .csv en la ruta especificada: {ruta_origen}")

print("=" * 80)
print(f"Iniciando análisis por dataset de {len(archivos_datasets)} archivos encontrados.")
print(f"Configuración actual: MODO_CAMPEONES = {MODO_CAMPEONES}")
print("=" * 80 + "\n")

for ruta_archivo in archivos_datasets:
    nombre_dataset = os.path.basename(ruta_archivo).replace(".csv", "")
    print(f"Procesando Dataset: {nombre_dataset}")
    
    # Cargar datos de formato largo del dataset actual
    df_raw = pd.read_csv(ruta_archivo)
    
    # Aplicar filtro de la lógica de campeones si está activo
    if MODO_CAMPEONES:
        df_raw = df_raw[df_raw["Metodo"].isin(CAMPEONES)]
        if df_raw.empty:
            print(f"[WARN] Al filtrar por campeones, no quedaron métodos válidos para {nombre_dataset}. Saltando...")
            continue
            
    # Pivotar los datos para el análisis por bloques:
    # Filas: Bloques (corridas 1 a 30) | Columnas: Métodos | Valores: Accuracy
    df_pivot = df_raw.pivot(index="Bloque", columns="Metodo", values="Accuracy")
    
    # Manejo de valores faltantes por si alguna corrida falló
    df_pivot = df_pivot.dropna()
    
    nombres_metodos = df_pivot.columns.tolist()
    matrix_rendimiento = df_pivot.to_numpy()
    
    # Validar que existan al menos 2 métodos y suficientes observaciones para Friedman
    if matrix_rendimiento.shape[1] < 2:
        print(f"[SKIP] {nombre_dataset} no tiene suficientes métodos para realizar una comparativa.\n")
        continue

    # --------------------------------------------------------------------------
    # 3. PRUEBA DE FRIEDMAN (DENTRO DEL DATASET)
    # --------------------------------------------------------------------------
    try:
        stat_acc, p_acc = friedmanchisquare(*[matrix_rendimiento[:, i] for i in range(matrix_rendimiento.shape[1])])
    except ValueError as e:
        print(f"[ERROR] No se pudo ejecutar Friedman en {nombre_dataset}. Detalles: {e}\n")
        continue

    print(f"  -> Friedman p-valor = {p_acc:.4e} | Estadístico = {stat_acc:.4f}")
    
    if p_acc < 0.05:
        print("  -> Diferencias significativas detectadas. Generando gráfico de Nemenyi...")
        
        # ----------------------------------------------------------------------
        # 4. PRUEBA POST-HOC DE NEMENYI
        # ----------------------------------------------------------------------
        nemenyi_acc = sp.posthoc_nemenyi_friedman(df_pivot)
        
        # ----------------------------------------------------------------------
        # 5. CÁLCULO DE RANGOS PROMEDIO POR BLOQUE
        # ----------------------------------------------------------------------
        # Mayor accuracy obtiene rango 1, por ello se procesa con '-row'
        ranks_acc = np.array([rankdata(-row, method="average") for row in matrix_rendimiento])
        avg_ranks_acc = pd.Series(ranks_acc.mean(axis=0), index=nombres_metodos)
        
        # ----------------------------------------------------------------------
        # 6. GENERACIÓN Y GUARDADO DEL CD PLOT
        # ----------------------------------------------------------------------
        fig, ax = plt.subplots(figsize=(7, 2.5))
        
        colores_disponibles = ["red", "blue", "green", "darkorange", "purple", "brown", "cyan"]
        paleta_colores = {metodo: colores_disponibles[i % len(colores_disponibles)] for i, metodo in enumerate(nombres_metodos)}
        
        if "GNLPDA" in paleta_colores:
            paleta_colores["GNLPDA"] = "crimson"
            
        critical_difference_diagram(
            ranks=avg_ranks_acc,
            sig_matrix=nemenyi_acc,
            label_props={"fontsize": 10},
            marker_props={"s": 40},
            label_fmt_left="{label} ({rank:.2f})",
            color_palette=paleta_colores,
            left_only=True
        )
        
        ax = plt.gca()
        ax.xaxis.grid(False)
        plt.tight_layout()
        
        # Guardar el gráfico incluyendo el sufijo del modo correspondiente
        sufijo_modo = "campeones" if MODO_CAMPEONES else "todos"
        nombre_grafico = f"CDplot_Nemenyi_{nombre_dataset}_{sufijo_modo}.png"
        ruta_salida_completa = os.path.join(ruta_destino, nombre_grafico)
        
        plt.savefig(ruta_salida_completa, dpi=300)
        plt.close()
        print(f"  [OK] Gráfico guardado en: {nombre_grafico}\n")
    else:
        print("  -> Sin diferencias significativas (p >= 0.05). No se genera diagrama.\n")

print("=" * 80)
print("[INFO] El procesamiento automatizado por dataset ha concluido de forma exitosa.")
print("=" * 80)







