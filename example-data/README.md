# Reproducing the analysis on data publicly available on Genbank

For privacy reasons, we are not able to share the full set of metadata used in this analysis. 
To support people interested in applying this framework to their own dataset, we describe how this analysis could be applied to the subset of our data that are publicly available on NCBI. 
The folder ```example-data``` contains 2 files:
- ```aligned_open_sentinel_WA.fasta.zst``` containing the subset of the sequences we analysed that are publicly available on NCBI and aligned with the ncov Nextstrain workflow. 
- ```metadata_open_sentinel_WA.tsv``` with the associated metadata (curated by Nextstrain with the ncov-ingest pipeline)

## Computing the RR of observing identical sequences between counties
We first start by subsetting the metadata and alignment file to only retain records with county information.
```bash
# Subset the metadata
tsv-filter -H --str-ne county:"?" metadata_open_sentinel_WA.tsv > metadata_open_sentinel_WA_with_county.tsv

# Get the strain names with county information
tsv-select -H --f sequence_name metadata_open_sentinel_WA_with_county.tsv | sed 1d > strain_name_open_sentinel_WA_with_county.txt

# Subset the sequence data
unzstd aligned_open_sentinel_WA.fasta.zst
seqkit grep --pattern-file strain_name_open_sentinel_WA_with_county.txt aligned_open_sentinel_WA.fasta > aligned_open_sentinel_WA_with_county.fasta
```
We can then use the scripts ```../scripts/get_RR_from_fasta.R``` to compute the RR of observing identical sequences between counties and store the results in a ```example-results/``` folder.

```bash
mkdir ../example-results/
cd ../scripts/
Rscript ./get_RR_from_fasta.R --input-fasta="../example-data/aligned_open_sentinel_WA_with_county.fasta" \
    --input-metadata="../example-data/metadata_open_sentinel_WA_with_county.tsv" \
    --name-group="county" --n-mut-away=0 \
    --output-file-csv="../example-results/df_RR_county_open_data.tsv" \
    --compute-subsample-CI=0 \
    --n-subsamples=1000 \
    --prop-subsample=0.8
```


## Computing the RR of observing identical sequences between age groups
We first start by subsetting the metadata and alignment file to only retain records with age information.
```bash
# Subset the metadata
tsv-filter -H --str-ne age_decade:"?" metadata_open_sentinel_WA.tsv > metadata_open_sentinel_WA_with_age.tsv

# Get the strain names with age information
tsv-select -H --f sequence_name metadata_open_sentinel_WA_with_age.tsv | sed 1d > strain_name_open_sentinel_WA_with_age.txt

# Subset the sequence data
unzstd aligned_open_sentinel_WA.fasta.zst
seqkit grep --pattern-file strain_name_open_sentinel_WA_with_age.txt aligned_open_sentinel_WA.fasta > aligned_open_sentinel_WA_with_age.fasta
```

```bash
mkdir ../example-results/
cd ../scripts/
Rscript ./get_RR_from_fasta.R --input-fasta="../example-data/aligned_open_sentinel_WA_with_age.fasta" \
    --input-metadata="../example-data/metadata_open_sentinel_WA_with_age.tsv" \
    --name-group="age_decade" --n-mut-away=0 \
    --output-file-csv="../example-results/df_RR_age_open_data.tsv" \
    --compute-subsample-CI=0 \
    --n-subsamples=1000 \
    --prop-subsample=0.8
```

## Note on computation time for larger datasets
The script ```./get_RR_from_fasta.R``` uses the ```dist.dna``` function from the ```ape``` R package to generate pairs of identical sequences.
Using this strategy, the computation times can become long for larger datasets (including for 10,000 sequences).
An alternative approach is to rely on other software tools (such as [pairsnp](https://github.com/gtonkinhill/pairsnp)) or to parallelize the computations between lineages. 
A detailed tutorial is provided [here](https://github.com/CecileTK/tutorial-rr-identical-sequences/) on how to adapt the computations for large genomic datasets. 