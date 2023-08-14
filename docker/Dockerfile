FROM rocker/r-ver:latest

COPY DESCRIPTION /tmp/DESCRIPTION

RUN install2.r --error remotes && \
        Rscript -e 'remotes::install_deps("/tmp/DESCRIPTION")' && \
        rm /tmp/DESCRIPTION

COPY . /src
RUN R CMD INSTALL /src && rm -rf /src
