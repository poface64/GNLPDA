
# ejecutar_todo.ps1
$datasets = Get-ChildItem ".\DATASETS\*.csv"

foreach ($file in $datasets) {
    Write-Host "Iniciando: $($file.Name)" -ForegroundColor Cyan
    # Llamada a Julia
    julia --project=@Angel -t 11 experimento_unicoREV1.jl $file.FullName
}

Write-Host "¡Proceso terminado!" -ForegroundColor Green



#### Automatización para UBUNTU ####

# 1. Definir la ruta de los datasets
DATASETS_DIR="./DATASETS"

# 2. Iterar sobre cada archivo .csv en la carpeta
for file in "$DATASETS_DIR"/*.csv; do
    # Extraer solo el nombre del archivo para el mensaje
    filename=$(basename "$file")
    
    echo -e "\e[36mIniciando: $filename\e[0m" # Color Cian
    
    # 3. Llamada a Julia con 11 hilos
    # Usamos --project=@Angel para mantener tu entorno
    julia --project=@Angel -t 11 experimento_unicoREV1.jl "$file"
    
    echo -e "\e[32m¡Finalizado: $filename!\e[0m" # Color Verde
done

echo ">>> Proceso masivo terminado con éxito."
