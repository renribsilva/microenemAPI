ARG R_VERSION=latest
FROM rocker/r-ver:${R_VERSION}

# 1. Instalação de dependências do sistema
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalação do Plumber via pak (mais rápido)
RUN R -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/dev/')"
RUN Rscript -e "pak::pkg_install('plumber')"

# 3. Preparação do diretório de trabalho
WORKDIR /app
COPY . /app

# 4. Ajuste de porta para o Render
# O Render muitas vezes usa a porta 10000, mas a 8080 é o padrão seguro. 
# Vamos fixar em 8080.
EXPOSE 8080

# 5. ENTRYPOINT simplificado e direto
# Note que apontamos diretamente para o seu /app/plumber.R
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/plumber.R'); pr$run(host='0.0.0.0', port=8080)"]