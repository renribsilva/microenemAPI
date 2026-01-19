library(plumber)

# Cria o objeto Plumber a partir do arquivo plumber.R
pr <- plumb("plumber.R")

# Roda no localhost, porta 8000
pr$run(host="0.0.0.0", port=8000)
