#!/bin/bash
# ejecutar_todo.sh

# Asegurar que se detenga si hay un error
set -e

DIRECTORIO_DATASETS="./DATASETS"
DIRECTORIO_PARTICIONES="./DATASETS/particiones"

for file in "$DIRECTORIO_DATASETS"/*.csv; do
    # Evitar procesar archivos de particiones si están en la misma carpeta
    if [[ "$file" == *"_partitions"* ]]; then
        continue
    fi

    nombre_base=$(basename "$file" .csv)
    archivo_particiones="$DIRECTORIO_PARTICIONES/${nombre_base}_partitions.csv"

    echo -e "\e[36mIniciando: ${nombre_base}.csv\e[0m"
    
    # Se le pasan DOS argumentos a Julia: el dataset original y su CSV de particiones
    julia -t 11 experimento_unicoR2.jl "$file" "$archivo_particiones"
    
    echo -e "\e[32mFinalizado y memoria liberada para: ${nombre_base}\e[0m"
    echo "------------------------------------------------"
done

echo -e "\e[32m¡Proceso completo en todos los datasets!\e[0m"
