# Usa imagem estável do R
FROM rocker/r-ver:4.3.0

# Instala dependências do sistema
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    && apt-get clean

# Define o diretório de trabalho
WORKDIR /app

# Instala o pacote plumber
RUN R -e "install.packages('plumber', repos='https://cran.r-project.org')"

# Copia todos os arquivos da pasta local para o container
COPY . .

# Expõe a porta da API
EXPOSE 8080

# Comando para iniciar a API
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
