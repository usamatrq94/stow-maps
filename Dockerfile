# get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev
