library(ape)
library(tidyverse)
source('utils_RR.R')

args <- commandArgs(trailingOnly = T)

## Check whether necessary arguments are specified
check_flag_is_in_arg <- function(name_flag){
  grep_in_args <- grep(name_flag, args, value = TRUE)
  return( ! (length(grep_in_args) == 0))
}

if(! check_flag_is_in_arg('--input-fasta*')){
  stop('ERROR: --input-fasta needs to be specified!')
}
if(! check_flag_is_in_arg('--input-metadata*')){
  stop('ERROR: --input-metadata needs to be specified!')
}
if(! check_flag_is_in_arg('--name-group*')){
  stop('ERROR: --name-group needs to be specified!')
}
if(! check_flag_is_in_arg('--n-mut-away*')){
  stop('ERROR: --n-mut-away needs to be specified!')
}
if(! check_flag_is_in_arg('--output-file-csv*')){
  stop('ERROR: --output-file-csv needs to be specified!')
}

input_file_sequence <- as.character(strsplit(grep('--input-fasta*', args, value = TRUE), split = '=')[[1]][[2]])
input_file_metadata <- as.character(strsplit(grep('--input-metadata*', args, value = TRUE), split = '=')[[1]][[2]])
name_group <- as.character(strsplit(grep('--name-group*', args, value = TRUE), split = '=')[[1]][[2]])
n_mut_away <- as.numeric(strsplit(grep('--n-mut-away*', args, value = TRUE), split = '=')[[1]][[2]])
output_file_csv <- as.character(strsplit(grep('--output-file-csv*', args, value = TRUE), split = '=')[[1]][[2]])

## Read arguments regarding CI computations
if(check_flag_is_in_arg('--compute-subsample-CI*')){
  bool_compute_CI <- as.logical(as.numeric(strsplit(grep('--compute-subsample-CI*', args, value = TRUE), split = '=')[[1]][[2]]))
} else{
  print('--compute-subsample-CI was not specified by the user and was hence set to its default value 0.')
  bool_compute_CI <- FALSE
}

if(bool_compute_CI){
  if(check_flag_is_in_arg('--n-subsamples*')){
    n_subsamples <- as.numeric(strsplit(grep('--n-subsamples*', args, value = TRUE), split = '=')[[1]][[2]])
  } else{
    print('--n-subsamples was not specified by the user and was hence set to its default value of 1000.')
    n_subsamples <- 1000
  }
  
  if(check_flag_is_in_arg('--prop-subsample*')){
    prop_subsample <- as.numeric(strsplit(grep('--prop-subsample*', args, value = TRUE), split = '=')[[1]][[2]])
  } else{
    print('--prop-subsample was not specified by the user and was hence set to its default value of 0.8')
    prop_subsample <- 0.8
  }
}

## Load alignment
print(paste('Reading FASTA file:', input_file_sequence))
sequence_data <- read.FASTA(input_file_sequence)

## Load metadata
print(paste('Reading metadata file:', input_file_metadata))
metadata_data <- read.csv(input_file_metadata)

## Compute dataframe with pairs at a given genetic distance
print(paste('Computing pairs of sequences', n_mut_away, 'mutations apart'))
df_pairs_identical_sequences <- get_df_pairs_given_genetic_distance_from_fasta(sequence_data, n_mut_away = n_mut_away)

## Compute relative risk of observing identical sequences in two groups name_group
print(paste('Generating relative risk of observing sequences between', name_group))
df_RR <- get_df_RR(df_pairs = df_pairs_identical_sequences, metadata_seq = metadata_data, name_group = name_group)

## Compute uncertainty around RR
if(bool_compute_CI){
  df_uncertainty_RR <- get_df_uncertainty_RR(df_pairs = df_pairs_identical_sequences, metadata_seq = metadata_data,
                                             name_group = name_group, prop_subsample = prop_subsample, n_subsamples = n_subsamples)
  df_uncertainty_RR <- df_uncertainty_RR %>% 
    group_by(n_mutations, group_1, group_2) %>% 
    summarise(lower_RR_subsample = quantile(RR, 0.025),
              median_RR_subsample = quantile(RR, 0.5),
              upper_RR_subsample = quantile(RR, 0.975),)
  
  df_RR_to_save <- df_RR %>% 
    select(n_mutations, group_1, group_2, RR) %>% 
    left_join(df_uncertainty_RR, by = c('n_mutations', 'group_1', 'group_2'))
  
} else{
  df_RR_to_save <- df_RR %>% select(n_mutations, group_1, group_2, RR)
}

## Save df_RR
print(paste('Saving dataframe with RR at', output_file_csv))
write.csv(df_RR_to_save, output_file_csv, row.names = F)

