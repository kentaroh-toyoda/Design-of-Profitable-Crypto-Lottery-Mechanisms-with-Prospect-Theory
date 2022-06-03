FROM rocker/tidyverse:4.2
USER root

RUN apt update
RUN apt install libxt6 -y # required for cairo-pdf
COPY ./install_packages.R .
RUN Rscript ./install_packages.R

WORKDIR /workspace
COPY ./*.R ./

ENTRYPOINT ["Rscript"]
