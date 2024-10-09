# Simulate from ReMASTER
simulate_from_xml <- function(beast_path, xml_file_path){
  system(paste0(beast_path, ' -overwrite ', xml_file_path))
}

write_fasta_from_tree_data <- function(fasta_file_path, tree_data){
  mat_seq <- Reduce('rbind', tree_data)
  rownames(mat_seq) <- names(tree_data)
  
  write.FASTA(x = as.DNAbin(mat_seq), file = fasta_file_path)
}

# Function to parse the Nexus from the remaster simulation and save a fasta file
parse_nexus_remaster_save_fasta <- function(nexus_file_path, fasta_file_path){
  con <- file(nexus_file_path, "r")
  
  pre_sequences <- readLines(con, n = 11)
  n_sequences <- strsplit(strsplit(x = pre_sequences[4], split = '=')[[1]][2], split = ';')[[1]][1] %>% 
    as.numeric()
  
  list_name_seq <- vector('list', n_sequences)
  list_seq <- vector('list', n_sequences)
  
  for(i_seq in 1:n_sequences){
    line_seq <- readLines(con, n = 1)
    line_seq <- substr(x = line_seq, start = 3, stop = nchar(line_seq))
    line_seq <- strsplit(line_seq, ' ')[[1]]
    list_name_seq[[i_seq]] <- line_seq[1]
    list_seq[[i_seq]] <- strsplit(line_seq[2], split = ';')[[1]][1]
  }
  
  close(con)
  
  names(list_seq) <- Reduce('c', list_name_seq)
  
  write.FASTA(char2dna(list_seq), file = fasta_file_path)
}

# Function to save metadata file from the phylo_tree output from remaster
write_metadata_from_phylo_tree <- function(metadata_file_path, phylo_tree){
  metadata_seq <- phylo_tree %>% 
    filter(isTip == T, !is.na(time)) %>%
    rename(subgroup = type) %>% 
    mutate(subgroup = substr(subgroup, start = 3, stop = nchar(subgroup))) %>% 
    select(label, time, subgroup) %>% 
    as_tibble() %>% 
    filter(! is.na(subgroup))
  
  write.csv(metadata_seq, metadata_file_path)
}

# Function to compute a distamce matrix from the sequence data
get_dist_mat <- function(sequence_data){
  
  if(length(unique(sapply(sequence_data, length))) > 1){
    stop('Sequences should be aligned and have the same length!')
  }
  
  dist_mat <- dist.dna(sequence_data, model = 'N',
                       as.matrix = T, pairwise.deletion = T)
  
  return(dist_mat)
}

# Process output of remaster to generate synthetic data
get_alignment_metadata <- function(xml_file_path, dir_results){
  
  # Read transmission tree (with associated node characteristics)
  phylo_tree <- read.beast(file = paste0(dir_results, '/', scenario_name, '.trees'))
  
  # Save alignment data from nexus
  parse_nexus_remaster_save_fasta(paste0(dir_results, '/', scenario_name, '.nexus'), 
                                  paste0(dir_results, '/alignment.fasta'))
  
  write_metadata_from_phylo_tree(paste0(dir_results, '/metadata.csv'), phylo_tree)
  
  # Read fasta file
  seq_data <- read.FASTA(paste0(dir_results, '/alignment.fasta'))
  dist_mat <- get_dist_mat(seq_data)
  saveRDS(dist_mat, paste0(dir_results, '/dist_mat.rds'))
  
}

