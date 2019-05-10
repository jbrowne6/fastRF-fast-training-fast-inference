FROM rocker/verse:latest

RUN apt-get update
RUN apt-get -y install libgsl0-dev

RUN R -e "install.packages('rerf', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('ranger', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('Rborist', repos = 'http://cran.us.r-project.org')"

