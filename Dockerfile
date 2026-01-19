# Imagem oficial do R
FROM r-base:latest

# Instala o pacote plumber
RUN R -e "install.packages('plumber', repos='https://cran.r-project.org')"

# Copia o seu arquivo plumber.R para o container
COPY plumber.R /plumber.R

# Expõe a porta da API
EXPOSE 8000

# Roda a API
CMD ["R", "-e", "pr <- plumber::plumb('/plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
