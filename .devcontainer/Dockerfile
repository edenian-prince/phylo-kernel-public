# Use the official Rocker R base image for version 4.3.3
FROM rocker/r-ver:4.3.3

# Update Ubuntu and install dependencies
RUN apt update && apt install -y \
    build-essential \
    cmake \
    gdal-bin \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libgdal-dev \
    libglpk-dev \
    libssl-dev \
    libudunits2-dev \
    libx11-dev \
    libxml2-dev \
    pandoc \
    libharfbuzz-dev \
    libfribidi-dev

# Set the working directory for the container
WORKDIR /workspace

# Copy all files to the container's workspace directory
COPY data /workspace/data
COPY example-data /workspace/example-data
COPY results /workspace/results
COPY figures /workspace/figures
COPY scripts /workspace/scripts
COPY .devcontainer /workspace/.devcontainer

# Restore R dependencies using renv
RUN cd scripts && Rscript -e "renv::restore()"

# Set the command to execute the R script
CMD ["/run.sh"]

