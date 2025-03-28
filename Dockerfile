#    This file is part of genefunnel-benchmarks
#    Copyright (C) 2024-2025  Emir Turkes, UK DRI at UCL
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Emir Turkes can be contacted at emir.turkes@eturkes.com

FROM rocker/rstudio:4.3.3

LABEL org.opencontainers.image.authors="Emir Turkes emir.turkes@eturkes.com"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        zlib1g-dev \
        libglpk40 \
    && Rscript -e "install.packages('flexdashboard')" \
        -e "install.packages('stringr')" \
        -e "install.packages('conflicted')" \
        -e "install.packages('shinyMatrix')" \
        -e "install.packages('RcppArmadillo')" \
        -e "install.packages('Seurat')" \
        -e "install.packages('kableExtra')" \
        -e "install.packages('bench')" \
        -e "install.packages('BiocManager')" \
        -e "BiocManager::install('ComplexHeatmap')" \
        -e "BiocManager::install('GSVA')" \
        -e "BiocManager::install('splatter')" \
        -e "BiocManager::install('scater')" \
    && rm -Rf /tmp/downloaded_packages/ \
        /tmp/*.rds
