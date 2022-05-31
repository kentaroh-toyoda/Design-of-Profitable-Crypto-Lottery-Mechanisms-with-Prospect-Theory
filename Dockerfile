FROM rocker/tidyverse:4.2

USER root
COPY ./install_packages.R .
RUN Rscript ./install_packages.R

WORKDIR /workspace
COPY ./*.R ./

ENTRYPOINT ["Rscript"]
