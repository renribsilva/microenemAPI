# Usa a última versão estável do R
FROM rocker/r-ver:latest

# Instala dependências do sistema
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libsodium-dev \
    libxml2-dev

# Instala o pacote plumber
RUN R -e "install.packages('plumber')"

# Organiza os arquivos 
WORKDIR /app
COPY . /app

# Define a porta
EXPOSE 8080

# host '0.0.0.0'
CMD ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
