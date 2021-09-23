# get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries

RUN apt-get update && apt-get install -y \
    utils \
    libcurl4-gnutls-dev \
    libssl-dev 

RUN R -e 'install.packages(c(\
              "shiny", \
	      "dplyr", \
	      "tidyr", \
              "ggplot2", \
	      "aws.s3", \
	      "shinyBS", \
	      "shinyWidgets" \
	      "DT", \
	      "purrr" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'