library(ape)
library(seqinr)
library(data.tree)
library(treeio) # Needs to be installed using BiocManager
library(purrr)
library(insect)
library(gdata)
library(tidyverse)

source('utils_sim.R')
source('utils_write_xml.R')
source('utils_RR.R')
################################
# Define simulation parameters #
################################

n_demes <- 5 # Number of demes
sequencing_density <- 0.1 # Scaling factor for the sequencing rate
pop_per_deme <- 100000 # Population size per deme
seed <- 345 # Seed for the random number generator
is_biased <- 1 # Binary indicator for bias in sequencing (equal to 1 if heterogeneous sequencing, 0 if homogeneous)
sequence_length <- 3000 # Length of simulated genome

mat_migration <- readRDS('../input_files/migration_matrix_5_demes.rds') # Migration matrix with migration rates between demes
reference_p_sample_per_group <- readRDS('../input_files/p_sample_per_group_5_demes.rds') # Reference values for sequencing rates

if(is_biased > 0.5){
  # Heterogeneous sequencing
  # The probability for an infection to sequenced is equal to the reference value scaled by sequencing_density
  p_sample_per_group <- reference_p_sample_per_group * sequencing_density
} else{
  # Homogeneous sequencing
  # The probability for an infection to sequenced is equal to the mean reference value across groups scaled by sequencing_density
  # This ensures that the mean probability of sequencing at the population level is comparable in the homogeneous and heterogeneous sequencing scenarios.
  p_sample_per_group <- rep(mean(reference_p_sample_per_group), n_demes) * sequencing_density
}

## Define initial values for the simulations
vec_S_init_per_deme <- rep(pop_per_deme, n_demes) # Initial number of susceptible individuals in S (per deme)

## Model parameters
mutation_rate <- 0.00003 # Pathogen substitution rate
rate_out_of_E <- 0.33 # Rate out which individuals exit the E compartment
rate_out_of_I <- 0.33 # Rate out which individuals exit the I compartment

R0 <- 2.0 # Basic reproduction number
beta_rate_SEIR <- R0 * rate_out_of_I
beta_rate = R0 * rate_out_of_I / (pop_per_deme) # Remaster rate for the force of infection (needs to be scaled by population size)


#####################
# Generate XML file #
#####################
# Name of the scenario
# (this will be used to define the name of the XML file and the directory where the results will be saved)
scenario_name <- 'my-scenario-test'

dir_results <- paste0('../results_remaster/', scenario_name)
ifelse(! dir.exists(dir_results), dir.create(dir_results), '')

xml_file_path <- paste0('../xmls_remaster/', scenario_name, '.xml')

## Generate XML
write_xml_heterogeneous_sequencing(output_file = xml_file_path, dir_results = dir_results,
                                   mat_migration = mat_migration,
                                   sequence_length = sequence_length, mutation_rate = mutation_rate, 
                                   beta_rate = beta_rate, 
                                   rate_out_of_E = rate_out_of_E, 
                                   rate_out_of_I = rate_out_of_I, 
                                   p_sample_per_deme = p_sample_per_group,
                                   n_demes = n_demes, min_sample_per_deme = -1 , 
                                   vec_S_init_per_deme = vec_S_init_per_deme)

#################################
# Run simulations from XML file #
#################################
set.seed(seed + 100*sequencing_density + is_biased*10)

# Simulate output with ReMASTER
my_beast_path <- '/Applications/BEAST2.7.5/bin/beast' # Needs to be updated to the local BEAST path
simulate_from_xml(beast_path = my_beast_path, xml_file_path)

# Process output of ReMASTER
get_alignment_metadata(xml_file_path = xml_file_path, dir_results = dir_results)

############################################
# Get RR from pairs of identical sequences #
############################################
# Load distance matrix
dist_mat <- readRDS(paste0(dir_results, '/dist_mat.rds'))

# Get pairs of identical sequences from the matrix
df_pairs_id_seq <- get_df_pairs_identical_seq_from_dist_mat(dist_mat)

# Load metadata
metadata_seq <- read_csv(paste0(dir_results, '/metadata.csv')) %>% 
  select(- '...1')

# Compute RR
df_RR <- get_RR_from_df_pairs(df_pairs_id_seq, metadata_seq)
df_RR
