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

Pathogen genomics can provide insights into disease transmission patterns, but new methods are needed to handle modern large-scale pathogen genome datasets. Genetically proximal viruses indicate epidemiological linkage and are informative about transmission events. Here, we leverage pairs of identical sequences using 114,298 SARS-CoV-2 genomes collected via sentinel surveillance from March 2021 to December 2022 in Washington State, USA, with linked age and residence information to characterize fine-scale transmission. The location of pairs of identical sequences is highly consistent with expectations from mobility and social contact data. Outliers in the relationship between genetic and mobility data can be explained by SARS-CoV-2 transmission between postal codes with male prisons, consistent with transmission between prison facilities. Transmission patterns between age groups vary across spatial scales. Finally, we use the timing of sequence collection to understand the age groups driving transmission. This work improves our ability to characterize transmission from large pathogen genome datasets.

## Install
The code is written in R and relies on some packages, which can be installed (a couple hours) using:

```bash
Rscript ./scripts/install_requirements.R "scripts/requirements.txt"
```

The analyses were performed using the following packages versions: ape (5.7-1), broom (1.0.5), colorspace (2.1-0), cowplot (1.1.3), doParallel (1.0.17), foreach (1.5.2), ggpubr (0.6.0), ggrepel (0.9.4), ggsignif (0.6.4), igraph (1.5.1), mgcv (1.9-0), purrr (1.0.2), RColorBrewer (1.1-3), Rcpp (1.0.11), reshape2 (1.4.4), seqinr (4.2-30), sf (1.0-14), spdep (1.2-8), tidyverse (2.0.0), vegan (2.6-4), viridis (0.6.4). 


## Computing relative risks of observing sequence at a defined genetic distance in two subgroups from user data
To facilitate the application of this method to other datasets, we provide the code developped to compute the relative risk of observing sequences at a defined genetic distance between different subgroups. We illustrate how this may be done starting from an arbitrary FASTA alignment and csv metadata file. The following command takes around 10 seconds to run on an Mac with an M2 Chip. 

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
# --prop-subsample: proportion of sequences subsampled in each replicate. If not specified, default is 0.8. 

Rscript ./get_RR_from_fasta.R \
    --input-fasta="../data/synthetic_data/synthetic-fasta.fasta" \
    --input-metadata="../data/synthetic_data/synthetic-metadata.csv" \
    --name-group="group" \
    --n-mut-away=0 \
    --output-file-csv="../results/df_RR.csv" \
    --compute-subsample-CI=1 \
    --n-subsamples=1000 \
    --prop-subsample=0.8
```

## Overview
This repository contains code and data associated with this manuscript.
To improve readibility and reuse of our code, we have splitted the different analyses in individual scripts with headers indicating which analyses are performed.
To facilitate reproduction of our results, here are the relationships between manuscript figures and files used to generated them:

#### Figure 1
- [toy_figure_clusters_id_seq.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/toy_figure_clusters_id_seq.R) for Figure 1A
- [plot_relationship_mutation_generation.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_relationship_mutation_generation.R) for Figure 1B
- [plot_identical_cluster_size_distribution.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_identical_cluster_size_distribution.R) for Figure 1C
- [plot_map_large_clusters.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_map_large_clusters.R) for Figure 1D. 
- [plot_signal_identical_sequences.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_signal_identical_sequences.R) for Figure 1E
- [plot_RR_within_county.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_within_county.R) for Figure 1G

#### Figure 2
- [plot_chloropleth_maps_RR_counties.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_chloropleth_maps_RR_counties.R) for Figure 2A
- [plot_RR_local_spread.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_local_spread.R) for Figure 2B-C
- [MDS_from_RR.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/MDS_from_RR.R) for Figure 2D
- [plot_RR_local_spread_east_west.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_local_spread_east_west.R) for Figure 2E
- [plot_RR_distance_east_west.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_distance_east_west.R) for Figure 2F
- [plot_direction_transmission_east_west.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_direction_transmission_east_west.R) for Figure 2G

#### Figure 3
- [gam_RR_seq_mobility.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/gam_RR_seq_mobility.R) for Figure 3A-B
- [plot_RR_zip_prisons_male.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses//plot_RR_zip_prisons_male.R) for Figure 3D-E
- [plot_prison_centrality.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/plot_prison_centrality.R) for Figure 3F
- []() for Figure 3G

#### Figure 4
- [compare_RR_seq_contacts.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/compare_RR_seq_contacts.R) for Figure 4A
- [plot_RR_age_geography.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/plot_RR_age_geography.R) for Figure 4B
- [plot_RR_age_geography_heatmaps.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/plot_RR_age_geography_heatmaps.R) for Figure 4C
- [plot_direction_age.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/plot_direction_age.R) for Figure 4D

#### Supplementary figures
- [plot_signal_identical_sequences_compare_null.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_signal_identical_sequences_compare_null.R) for Figure S1
- [plot_RR_within_county_different_periods.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_within_county_different_periods.R) for Figure S2A
- [plot_RR_within_county_different_variants.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_within_county_different_variants.R) for Figure S2B
- [simulate_cluster_identical_sequences_different_R.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/simulate_cluster_identical_sequences_different_R.R) for Figure S2C
- [plot_cor_RR_sequencing_effort.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_cor_RR_sequencing_effort.R) for Figure S3
- [plot_comparison_DTA_RR.R](https://github.com/blab/phylo-kernel-public/blob/main/remaster/scripts/plot_comparison_DTA_RR.R) for Figure S4
- [plot_heatmap_matrix_RR_counties.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_heatmap_matrix_RR_counties.R) for Figure S5
- [plot_chloropleth_maps_RR_counties.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_chloropleth_maps_RR_counties.R) for Figure S6
- [impact_subsampling_RR_East_West.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/impact_subsampling_RR_East_West.R) for Figure S7
- [plot_delay_pairs_east_west.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_delay_pairs_east_west.R) for Figure S8
- [compare_workflow_safegraph.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/compare_workflow_safegraph.R) for Figure S9
- [gam_RR_seq_mobility.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/gam_RR_seq_mobility.R) for Figure S10 and S13
- [simulate_clusters_identical_sequences_by_age.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/simulate_clusters_identical_sequences_by_age.R) for Figure S11
- [gam_RR_seq_mobility_over_time.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/gam_RR_seq_mobility_over_time.R) for Figure S14
- [compare_safegraph_across_waves.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/compare_safegraph_across_waves.R) for Figure S15
- [plot_RR_mobility_east_west.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/plot_RR_mobility_east_west.R) for Figure S16
- []() for Figure S17
- [plot_RR_age.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/plot_RR_age.R) for Figure S18
- [plot_RR_age_func_genetic_distance.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/plot_RR_age_func_genetic_distance.R) for Figure S19
- [plot_RR_age_geography.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/age_analyses/plot_RR_age_geography.R) for Figure S20
- []() for Figure S21
- []() for Figure S22
- []() for Figure S23
- [impact_unsampled_locations.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/impact_unsampled_locations.R) for Figure S24
- []() for Figure S25
- []() for Figure S26
- []() for Figure S27
- [SG_county_level_bias_WA_state_2020_2022.R](https://github.com/blab/phylo-kernel-public/blob/0ef6bfa9bd4bafd99377d03ae2d60881978f371f/scripts/mobility_analyses/SG_county_level_bias_WA_state_2020_2022.R) for Figure S28 and Figure S29
- []() for Figure S30
- [plot_cluster_charact_over_time.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_cluster_charact_over_time.R) for Figure S31
- [plot_time_series_epidemic_WA.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/presentation_framework/plot_time_series_epidemic_WA.R) for Figure S32
- []() for Figure S33

#### Supplementary tables
- []() for Table S1
- [plot_RR_local_spread_east_west.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/spatial_analyses/plot_RR_local_spread_east_west.R) for Table S2
- [compare_RR_seq_mobility.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/compare_RR_seq_mobility.R) for Table S3
- [compute_prison_population_vs_county.R](https://github.com/blab/phylo-kernel-public/blob/main/scripts/mobility_analyses/compute_prison_population_vs_county.R) for Table S4
