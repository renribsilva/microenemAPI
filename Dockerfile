# Source - https://stackoverflow.com/a
# Posted by Kevin Ushey, modified by community. See post 'Timeline' for change history
# Retrieved 2026-01-19, License - CC BY-SA 4.0

FROM rocker/r-ver:4.0.2

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev

# create the application folder
RUN mkdir -p ~/application

# copy everything from the current directory into the container
COPY "/" "application/"
WORKDIR "application/" 

# open port 80 to traffic
EXPOSE 80

# install plumber
RUN R -e "install.packages('plumber')"

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "plumber.R"]
