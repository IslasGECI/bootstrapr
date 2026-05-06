FROM islasgeci/base:22.04
COPY . /workdir

RUN Rscript -e "install.packages(c('comprehenr'), repos='http://cran.rstudio.com')"
