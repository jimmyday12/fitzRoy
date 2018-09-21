FROM rocker/rstudio:3.4.1

RUN apt-get update \
    && apt-get install -y libxml2-dev zlib1g-dev \
    && R -e 'install.packages("tidyverse")' \
    && R -e 'install.packages("devtools")' \
    && R -e 'install.packages("shiny")' \
    && R -e 'install.packages("rs connect")' \
    && R -e 'devtools::install_github("jimmyday12/fitzRoy")'

