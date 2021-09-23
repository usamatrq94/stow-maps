# get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries

RUN apt-get update && apt-get install -y \
    apt-utils \
    libcurl4-gnutls-dev \
    libssl-dev 

RUN R -e 'install.packages(c("shiny", "dplyr", "tidyr", "ggplot2", "aws.s3", "shinyBS", "shinyWidgets", "DT", "purrr"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'

COPY /home/ubuntu/stow-maps/app/* ./app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
