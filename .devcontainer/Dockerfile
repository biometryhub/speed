FROM ubuntu:18.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN apt-get update && apt-get install -y \
    software-properties-common \
    dirmngr \
    gnupg \
    curl \
    sudo \
    locales \
    ca-certificates \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev libglpk-dev \
    libgfortran3 \
    && locale-gen en_US.UTF-8

ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

# Add CRAN R repo
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys '51716619E084DAB9' && \
    add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/"

# Install R
RUN apt-get update && apt-get install -y r-base
RUN R -e 'install.packages(c("ggplot2", "testthat", "vdiffr", "igraph", "data.table", "sp"))'

ENTRYPOINT []
