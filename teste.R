library(plumber)

# Plumber pega o arquivo api.R
pr <- plumb("plumber.R")

# Roda localmente na porta 8000
pr$run(port=8000)