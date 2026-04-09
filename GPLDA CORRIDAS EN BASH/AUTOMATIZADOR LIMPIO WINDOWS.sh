
# ejecutar_todo.ps1
$datasets = Get-ChildItem ".\DATASETS\*.csv"

foreach ($file in $datasets) {
    Write-Host "Iniciando: $($file.Name)" -ForegroundColor Cyan
    # Llamada a Julia
    julia -t 11 experimento_unico.jl $file.FullName
}

Write-Host "¡Proceso terminado!" -ForegroundColor Green



