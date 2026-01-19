# Usa a última versão estável do R
FROM rocker/r-ver:latest

# 1. Instala dependências do sistema
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libsodium-dev \
    libxml2-dev

# 2. Instala o pacote plumber
RUN R -e "install.packages('plumber')"

# 3. Organiza os arquivos (evite usar '~/', use caminhos absolutos no Docker)
WORKDIR /app
COPY . /app

# 4. Define a porta. O Render trabalha bem com a 8080 ou 10000. 
# Importante: o EXPOSE aqui deve bater com o port no comando final.
EXPOSE 8080

# 5. O PONTO CHAVE: O comando deve "instanciar" e "rodar" o plumber
# host '0.0.0.0' é obrigatório para ser acessível fora do container.
CMD ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=8080)"]