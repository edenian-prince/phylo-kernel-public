# Fine-scale spatial and social patterns of SARS-CoV-2 transmission from identical pathogen sequences

Cécile Tran-Kiem<sup>1</sup>,
Miguel Paredes<sup>1,2</sup>,
Amanda Perofsky<sup>3,4</sup>,
Lauren Frisbie<sup>5</sup>,
Hong Xie<sup>6</sup>,
Kevin Kong<sup>6</sup>,
Amanda Weixler<sup>6</sup>,
Alexander Geninger<sup>1,6</sup>,
Pavitra Roychoudhury<sup>1,6</sup>,
JohnAric Peterson<sup>5</sup>,
Andrew Delgado<sup>5</sup>,
Holly Halstead<sup>5</sup>,
Drew MacKellar<sup>5</sup>,
Philip Dykema<sup>5</sup>,
Luis Gamboa<sup>3</sup>,
Chris Frazar<sup>7</sup>,
Erica Ryke<sup>7</sup>,
Jeremy Stone<sup>3</sup>,
David Reinhart<sup>3</sup>,
Lea Starita<sup>3,7</sup>,
Allison Thibodeau<sup>5</sup>,
Cory Yun<sup>5</sup>,
Frank Aragona<sup>5</sup>,
Allison Black<sup>5</sup>,
Cécile Viboud<sup>4</sup>,
Trevor Bedford <sup>1,8</sup>.

<sup>1</sup> Vaccine and Infectious Disease Division, Fred Hutchinson Cancer Center, Seattle, WA, USA <br>
<sup>2</sup> Department of Epidemiology, University of Washington, Seattle, WA, USA <br>
<sup>3</sup> Brotman Baty Institute, University of Washington, Seattle, WA, USA <br>
<sup>4</sup> Fogarty International Center, National Institutes of Health, Bethesda, MD, USA <br>
<sup>5</sup> Washington State Department of Health, Shoreline, WA, USA <br>
<sup>6</sup> Department of Laboratory Medicine and Pathology, University of Washington, Seattle, WA, USA<br>
<sup>7</sup> Department of Genome Sciences, University of Washington, Seattle, WA, USA <br>
<sup>8</sup> Howard Hughes Medical Institute, Seattle, WA, USA


## Abstract

## Install
The code is written in R and relies on some packages, which can be installed using:

```bash
Rscript ./scripts/install_requirements.R "scripts/requirements.txt"
```

## Computing relative risks of observing sequence at a defined genetic distance in two subgroups from user data
To facilitate the application of this method to other datasets, we provide the code developped to compute the relative risk of observing sequences at a defined genetic distance between different subgroups. We illustrate how this may be done starting from an arbitrary FASTA alignment and csv metadata file.

```bash
cd scripts/

## Generate the relative risk of observing sequences at a specified genetic distance. It takes the following arguments:
# --input-fasta: file path to the user-defined FASTA alignment
# --input-metadata: file path to the user-defined metadata file. The metadata should be a csv file with a column "sequence_name" containing the sequence names (matching those found in the alignment) and some associated metadata columns. 
# --name-group: name of the column in the metadata file denoting the groups between which we will generate the relative risk of observing sequences at a given genetic distance.
# --n-mut-away: genetic distance between sequences. 
# --output-file-csv: file path to save the dataframe with the RR of observing sequences

# --compute-subsample-CI: boolean (1 or 0) indicating whether to compute subsampled confidence interval around RR estimates. If not specified, default is 0. 
# --n-subsamples: number of draws used to compute CI. If not specified, default is 1000.
# --prop-subsample: proportion of . If not specified, default is 0.8. 

Rscript ./get_RR_from_fasta.R \
    --input-fasta="../data/synthetic_data/synthetic-fasta.fasta" \
    --input-metadata="../data/synthetic_data/synthetic-metadata.csv" \
    --name-group="group" \
    --n-mut-away=0 \
    --output-file-csv="../results/df_RR.csv" \
    --compute-subsample-CI=1 \
    --n-subsamples=100 \
    --prop-subsample=0.8
```

## Overview
This repository contains code and data associated with the above manuscript.
