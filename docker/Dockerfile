FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends git

COPY DESCRIPTION /tmp/DESCRIPTION

RUN install2.r --error remotes && \
        Rscript -e 'remotes::install_deps("/tmp")' && \
        rm /tmp/DESCRIPTION

COPY . /src
RUN R CMD INSTALL /src && rm -rf /src
