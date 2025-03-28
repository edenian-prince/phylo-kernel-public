#!/bin/bash

Rscript get_RR_from_fasta.R \
    --input-fasta="../data/synthetic_data/synthetic-fasta.fasta" \
    --input-metadata="../data/synthetic_data/synthetic-metadata.csv" \
    --name-group="group" \
    --n-mut-away=0 \
    --output-file-csv="../results/df_RR.csv" \
    --compute-subsample-CI=1 \
    --n-subsamples=1000 \
    --prop-subsample=0.8