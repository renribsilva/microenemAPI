FROM r-base:latest

# Instala plumber
RUN R -e "install.packages('plumber', repos='https://cran.r-project.org')"

# Copia o script da API
COPY plumber.R /plumber.R

# Expõe porta 8000
EXPOSE 8000

# Roda a API
CMD ["R", "-e", "pr <- plumber::plumb('/plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
