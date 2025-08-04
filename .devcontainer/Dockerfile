FROM ubuntu:18.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies
RUN add-apt-repository ppa:ubuntu-toolchain-r/test && \
    apt-get update && apt-get install -y \
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
    libfontconfig1-dev libfreetype6-dev g++-9 libfribidi-dev gcc-9 \
    libgfortran3 gfortran libgfortran-9-dev \
    && locale-gen en_US.UTF-8 \
    update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 700 --slave /usr/bin/g++ g++ /usr/bin/g++-7 \
    update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 900 --slave /usr/bin/g++ g++ /usr/bin/g++-9


ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

# Add CRAN R repo
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys '51716619E084DAB9' && \
    add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/"

# Install R
RUN apt-get update && apt-get install -y r-base
RUN R -e 'install.packages(c("ggplot2", "testthat", "vdiffr", "igraph", "data.table", "sp", "matrixStats", "languageserver", "devtools"))'

ENTRYPOINT []
