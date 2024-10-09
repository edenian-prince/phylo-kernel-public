write_xml_heterogeneous_sequencing <- function(output_file, 
                                               dir_results, mat_migration,
                                               sequence_length = 3000,
                                               mutation_rate = 0.00005,
                                               beta_rate = 0.0001,
                                               rate_out_of_E = 0.33,
                                               rate_out_of_I = 0.33,
                                               p_sample_per_deme = c(0.1, 0.1, 0.1),
                                               n_demes = 3,
                                               min_sample_per_deme = 50,
                                               vec_S_init_per_deme = c(1e5, 1e5, 1e5)){
  cat('<beast version="2.0" namespace="beast.base.inference.parameter:beast.base.inference:remaster">','\n',
      file = output_file, sep = '')
  cat('\t', file = output_file, sep = '', append = T)
  cat('<run spec="Simulator" nSims="1">', '\n', 
      file = output_file, sep = '', append = T)
  cat('\t\t', file = output_file, sep = '', append = T)
  cat('<simulate spec="feast.simulation.SimulatedAlignment" ', 
      'outputFileName="', dir_results,
      '/$(filebase).nexus" sequenceLength="', format(sequence_length, scientific = F), 
      '">', '\n',
      file = output_file, sep = '', append = T)
  cat('\t\t\t', file = output_file, sep = '', append = T)
  cat('<siteModel spec="beast.base.evolution.sitemodel.SiteModel" mutationRate="', format(mutation_rate, scientific = F), '">', '\n',
      file = output_file, sep = '', append = T)
  cat('\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<substModel spec="beast.base.evolution.substitutionmodel.JukesCantor"/>', '\n', 
      file = output_file, sep = '', append = T)
  cat('\t\t\t', file = output_file, sep = '', append = T)
  cat('</siteModel>', '\n', 
      file = output_file, sep = '', append = T)
  cat('\n\n', file = output_file, sep = '', append = T)
  cat('\t\t\t', file = output_file, sep = '', append = T)
  
  cat('<tree spec="SimulatedTree" id="tree">', '\n', file = output_file, sep = '', append = T)
  cat('\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<trajectory spec="StochasticTrajectory" id="traj" mustHave="', file = output_file, sep = '', append = T)
  for(deme in 0:(n_demes - 1)){
    cat('sample[', deme, ']&gt;', min_sample_per_deme, sep = '', file = output_file, append = T)
    if(deme != n_demes - 1){
      cat(' &amp;&amp; ', sep = '', file = output_file, append = T)
    }
  }
  cat('">', '\n\n', file = output_file, sep = '', append = T)
  
  ## Population initialization
  cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<population spec="RealParameter" id="S" value="', file = output_file, sep = '', append = T)
  cat(format(vec_S_init_per_deme, scientific = F), file = output_file,append = T)
  cat('"/>', '\n', file = output_file, sep = '', append = T)
  
  cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<population spec="RealParameter" id="E" value="', file = output_file, sep = '', append = T)
  cat(rep(0, n_demes), file = output_file,append = T)
  cat('"/>', '\n', file = output_file, sep = '', append = T)
  
  cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<population spec="RealParameter" id="I" value="', file = output_file, sep = '', append = T)
  cat(c(1, rep(0, n_demes - 1)), file = output_file,append = T)
  cat('"/>', '\n', file = output_file, sep = '', append = T)
  
  cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<population spec="RealParameter" id="R" value="', file = output_file, sep = '', append = T)
  cat(rep(0, n_demes), file = output_file,append = T)
  cat('"/>', '\n', file = output_file, sep = '', append = T)
  
  cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
  cat('<samplePopulation spec="RealParameter" id="sample" value="', file = output_file, sep = '', append = T)
  cat(rep(0, n_demes), file = output_file,append = T)
  cat('"/>', '\n', file = output_file, sep = '', append = T)
  cat('\n', file = output_file, sep = '', append = T)
  
  # S to E transition
  for(deme in 0:(n_demes - 1)){
    cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
    cat('<reaction spec="Reaction" rate="', format(beta_rate, scientific = F), '"> ',
        'I[', deme, '] + S[', deme, '] -> I[', deme, '] + E[', deme, '] </reaction>',
        '\n',
        file = output_file, sep = '', append = T)
  }
  cat('\n', file = output_file, sep = '', append = T)
  
  # E to I transitions
  for(deme in 0:(n_demes - 1)){
    cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
    cat('<reaction spec="Reaction" rate="', format(rate_out_of_E, scientific = F), '"> ',
        'E[', deme, '] -> I[', deme, '] </reaction>',
        '\n',
        file = output_file, sep = '', append = T)
  }
  cat('\n', file = output_file, sep = '', append = T)
  
  # Migration rates
  for(deme_origin in 0:(n_demes - 1)){
    for(deme_destination in (0:(n_demes - 1))){
      if(deme_origin != deme_destination){
        cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
        cat('<reaction spec="Reaction" rate="', format(mat_migration[deme_origin + 1, deme_destination + 1], scientific = F), '"> ',
            'I[', deme_origin, '] -> I[', deme_destination, '] </reaction>',
            '\n',
            file = output_file, sep = '', append = T)
      }
    }
  }
  cat('\n', file = output_file, sep = '', append = T)
  
  # I to R transitions
  for(deme in 0:(n_demes - 1)){
    cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
    cat('<reaction spec="Reaction" rate="', format(rate_out_of_I * (1. - p_sample_per_deme[deme + 1]), scientific = F), '"> ',
        'I[', deme, '] -> R[', deme, '] </reaction>',
        '\n',
        file = output_file, sep = '', append = T)
  }
  cat('\n', file = output_file, sep = '', append = T)
  
  # I to sample transitions
  for(deme in 0:(n_demes - 1)){
    cat('\t\t\t\t\t', file = output_file, sep = '', append = T)
    cat('<reaction spec="Reaction" rate="', format(rate_out_of_I * p_sample_per_deme[deme + 1], scientific = F), '"> ',
        'I[', deme, '] -> sample[', deme, '] </reaction>',
        '\n',
        file = output_file, sep = '', append = T)
  }
  cat('\n', file = output_file, sep = '', append = T)
  
  cat('\t\t\t\t', '</trajectory>', '\n', file = output_file, sep = '', append = T)
  cat('\t\t\t', '</tree>', '\n', file = output_file, sep = '', append = T)
  cat('\t\t', '</simulate>', '\n\n', file = output_file, sep = '', append = T)
  
  cat('\t\t', '<logger spec="Logger" fileName="',
      dir_results, 
      '/$(filebase).traj">', '\n', file = output_file, sep = '', append = T)
  cat('\t\t\t', '<log idref="traj"/>', '\n', file = output_file, sep = '', append = T)
  cat('\t\t', '</logger>', '\n\n', file = output_file, sep = '', append = T)
  
  cat('\t\t', ' <logger spec="Logger" mode="tree" fileName="',
      dir_results, '/$(filebase).trees">', '\n', file = output_file, sep = '', append = T)
  cat('\t\t\t', '<log spec="TypedTreeLogger" tree="@tree" removeSingletonNodes="true"/>', '\n', file = output_file, sep = '', append = T)
  cat('\t\t', '</logger>', '\n\n', file = output_file, sep = '', append = T)
  
  cat('\t\t', ' <logger spec="Logger">', '\n', file = output_file, sep = '', append = T)
  cat('\t\t\t', '<log spec="beast.base.evolution.tree.TreeStatLogger" tree="@tree"/>', '\n', file = output_file, sep = '', append = T)
  cat('\t\t', '</logger>', '\n\n', file = output_file, sep = '', append = T)
  
  cat('\t', '</run>', '\n', file = output_file, sep = '', append = T)
  cat('</beast>', '\n', file = output_file, sep = '', append = T)
}