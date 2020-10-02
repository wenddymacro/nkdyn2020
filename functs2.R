################ Functions ####################################################

irfs <- function(model, irf_directory = './models/irfs'){
  # imports names and data for IRFs for all models
  
  invisible(require(R.matlab))
  invisible(require(dplyr))
  
  dire_nams <- paste0(irf_directory,'/', model, '_irf_names.mat')
  dire_dat <- paste0(irf_directory,'/', model, '_irf_data.mat')
  
  # read in names
  noms <- readMat(dire_nams) %>% 
    .$irf.names %>% 
    unlist()
  
  irf_data <- readMat(dire_dat) %>% 
    .$irf.data %>% 
    lapply(t) %>% 
    bind_cols()
  
  names(irf_data) <- noms
  
  irf_data <- irf_data %>% add_column(mod = model,
                                      quarter = 0:(nrow(.)-1))
  
  mp <- irf_data %>%
    dplyr::select(mod, quarter, ends_with('_mp_shock')) %>%
    pivot_longer(cols = ends_with('_mp_shock'), 
                 values_to = c('value'), 
                 names_to = 'var',
                 names_repair = 'unique') %>%
    mutate(var = sub(pattern = '_mp_shock', 
                     replacement = '', 
                     x = var),
           shock = 'mp')
  
  tfp <- irf_data %>%
    dplyr::select(mod, quarter, ends_with('_tfp_shock')) %>%
    pivot_longer(cols = ends_with('_tfp_shock'), 
                 values_to = c('value'), 
                 names_to = 'var',
                 names_repair = 'unique') %>%
    mutate(var = sub(pattern = '_tfp_shock', 
                     replacement = '', 
                     x = var),
           shock = 'tfp')
  
  irf_data <- bind_rows(mp, tfp)
  
  return(irf_data)
}

irfs_z <- function(model, irf_directory = './models/irfs'){
  # imports names and data for IRFs for all models
  
  invisible(require(R.matlab))
  invisible(require(dplyr))
  
  dire_nams <- paste0(irf_directory,'/', model, '_irf_names.mat')
  dire_dat <- paste0(irf_directory,'/', model, '_irf_data.mat')
  
  # read in names
  noms <- readMat(dire_nams) %>% 
    .$irf.names %>% 
    unlist()
  
  irf_data <- readMat(dire_dat) %>% 
    .$irf.data %>% 
    lapply(t) %>% 
    bind_cols()
  
  names(irf_data) <- noms
  
  irf_data <- irf_data %>% add_column(mod = model,
                                      quarter = 0:(nrow(.)-1))
  
  z <- irf_data %>%
    dplyr::select(mod, quarter, ends_with('_e_z')) %>%
    pivot_longer(cols = ends_with('_e_z'), 
                 values_to = c('value'), 
                 names_to = 'var',
                 names_repair = 'unique') %>%
    mutate(var = sub(pattern = '_e_z', 
                     replacement = '', 
                     x = var),
           shock = 'z')
  
  return(z)
}


simpi <- function(model, simpi_directory = './models/simuls', all_var = F){
  # imports names and data for IRFs for all models
  
  invisible(require(R.matlab))
  invisible(require(dplyr))
  
  dire_nams <- paste0(simpi_directory,'/', model, '_sim_names.mat')
  dire_dat <- paste0(simpi_directory,'/', model, '_sim_data.mat')
  
  # read in names
  noms <- readMat(dire_nams) %>% 
    .$sim.names %>% 
    unlist()
  
  sim_data <- readMat(dire_dat) %>% 
    .$sim.data %>% 
    data.frame() %>% 
    as_tibble()
  
  names(sim_data) <- noms
    
  if (all_var) {
    sim_data <- sim_data %>% 
      add_column(mod = model,
                 iter = 1:nrow(.)) %>% 
      pivot_longer(cols = -c('mod', 'iter'), 
                   names_to = c('var'))
  }else{
    sim_data <- sim_data %>% 
      add_column(mod = model,
                 iter = 1:nrow(.)) %>% 
      dplyr::select(mod, iter, pi)
  }
  
  return(sim_data)
}
