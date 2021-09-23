# git repository 
# https://github.com/usamatrq94/stow-maps

# base R image https://hub.docker.com/u/rocker/
FROM rocker/r-base:latest

# installing packages and updating system libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# installing r and its packages
RUN install.r shiny
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("shiny", "dplyr", "tidyr", "ggplot2","xml2", "aws.s3", "shinyBS", "shinyWidgets", "DT", "purrr"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'
RUN R -e 'install.packages("aws.s3", repos = "https://cloud.R-project.org")'

# creating linux group and user
RUN addgroup --system app \
    && adduser --system --ingroup app app

# creating AWS credentials
RUN echo "aws_creds <- function(){ \
  return( \
    Sys.setenv( \
      "AWS_ACCESS_KEY_ID" = "YOUR_ACCESS_KEY_ID", \
      "AWS_SECRET_ACCESS_KEY" = "YOUR_SECRET_KEY", \
      "AWS_DEFAULT_REGION" = "YOUR_REGION" \ 
    ) \
  ) \
}" > ./app/aws_creds.R 

# copying files to container
WORKDIR /home/app/
COPY ./app/app.R .
COPY ./app/aws_creds.R .

# setting permissions for app user
RUN chown app:app -R /home/app

# setting user
USER app
EXPOSE 3838

# closing docker file
CMD ["R", "-e", "shiny::runApp('/home/app')"]
