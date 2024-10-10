# Phylogeographic Analysis of Simulated Outbreak

## Simulating synthetic outbreak data with the BEAST ReMASTER package

We use ReMASTER to simulate an SEIR-like epidemic between demes along with the sequences obtained from a subset of infected individuals.
Example XMLs can be generated using the function `write_xml_heterogeneous_sequencing` in the `scripts/utils_write_xml.R` file.
We provide a detailed description of how to use this function in the script `scripts/generate_synthetic_data.R`.
From this XML, we can simulate an outbreak, generate the associated sequence alignment (with metadata) and compute the RR of identical sequences falling in two groups (full description in `scripts/generate_synthetic_data.R`).


The results of the remaster simulations we use in the manuscript are stored in the `results_remaster` folder.
The corresponding ReMASTER XML files are stored in the `xmls_remaster` folder.

## Preparing the simulated data for BEAST

Simulated data for the preprint can be found in `data/preprint_data/input_for_dta_new_beta/`

To prepare the data for analysis in BEAST 1, go to the `scripts/` folder and run `./data_prep_biased.sh` and `./data_prep_unbiased.sh` in the command line. These two scripts will put all the date and location information into the strain name of the .fasta alignment which makes it easy to parse via BEAUTI for BEAST1. 
The already preprared fasta alignments can be found in `data/beast/`. 

All BEAST xmls used in this study can be found in `xmls/` and can be run directly using BEAST1 either throughout the GUI or the command line. 
Specifically, `empirical_tree_xmls/` contains the xmls for the phylogeographic analysis using the simulated trees as input. 
To create these xmls yourself, you can use `scripts/creating_empirical_dta_xmls.ipynb` which uses the xml templates found in `xmls/xml_templates/`. 
Of note, for now, you need to specify in the code if you want to use the template for the asymmetrical or symmetrical migration matrix. 
Finally, if creating your own empirical tree xml, you should run `./change_taxon_id.sh` in the command line which is found under `xmls/`. 
This script will make sure that the taxon names in the BEAST XML match the taxon names in the empirical tree file. Make sure that when running the empirical tree in BEAST, that you run it in the same folder as the tree files are located in, which can be found in `data/preprint_data/input_for_dta_new_beta/`.

`estimated_tree_xmls/` contains all the already formatted xmls to run the combined phylogenetic and phylogeographic analysis.
 `just_tree_xmls/` contains the xmls for just running the phylogenetic analysis without any phylogeography.

All the resulting log files found in this analysis can be found under `results/`

To extract the mean migration rates from the log files, you can use `scripts/extracting_mig_rates.ipynb`. 
The resulting migration rate tables used in this analysis can be found in `results/migration_rate_tables`.


Figure S4 can be reproduce using `scripts/plot_comparison_DTA_RR.R` and Table S1 using `scripts/get_table_comparison_DTA_RR.R`.