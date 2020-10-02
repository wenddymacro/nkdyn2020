################ Directories ###################################################

# setup wd and dependent ones
d_irfs <- 'models/irfs'
d_sims <- 'models/simuls'
d_plots <- 'plots'
d_tabs <- 'tables'

# compile paths
d_irfs <- file.path(getwd(), d_irfs)
d_sims <- file.path(getwd(), d_sims)
d_plots <- file.path(getwd(), d_plots)
d_tabs <- file.path(getwd(), d_tabs)

# create dirs
dir.create(d_irfs, showWarnings = F)
dir.create(d_sims, showWarnings = F)
dir.create(d_plots, showWarnings = F)
dir.create(d_tabs, showWarnings = F)


################ Functions #####################################################

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

lm_custom <- function(data, vars = c('pi'), .lags, interc = F){
  
  # this function takes a df, selects a provided list of vars,
  # runs a linear model with provided lags, outputs the results
  
  # ad hoc version of the function
  invisible(require(dplyr))
  
  transformed_data <- data %>% 
    dplyr::select(!!vars) %>% 
    lagger_bis(lag = .lags)
  
  formulavars <- formula.maker(df = transformed_data,
                               intercept = interc,
                               y = vars)
  
  out_lm <- lm(data = transformed_data,
               formula = formulavars)
  return(out_lm)
  
}

optilags <- function(data, .maxlags){
  # given a series and an upper bound, computes the optimal number of lags
  # according to BIC
  
  data_v <- data %>% 
    dplyr::select(pi) %>% 
    as.matrix()
  
  optilags <- ur.df(y = data_v, 
                    lags = .maxlags, 
                    selectlags = 'BIC') %>% 
    slot(., 'optilags')
  
  return(optilags)
}

fm_apply <- function(foo, n){
  # shorthand to use in future_pmap
  # calls for atom foo
  return(sapply(rep(foo, n), list))
}

instant_pkgs <- function(pkgs) { 
  ## Function loading or installing packages in
  ## current R instance.
  ## Developed by Jaime M. Montana Doncel - V1
  
  
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach)) suppressPackageStartupMessages(library(need_to_attach[i], character.only = TRUE))
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
}

lagger_bis <- function(series, lag, na.cut=F){
  # Takes a time series and creates a matrix with given number
  # of lags, also generating appropriate names
  # 
  matrix <- embed(as.matrix(series), lag+1)
  matrix <- as.data.frame(matrix)
  names(matrix) <- c(names(series), paste(names(series), 1:lag, sep='.'))
  
  # conditional to remove NAs from output
  if (na.cut){
    matrix <- na.omit(matrix)
  }
  
  # output
  return(matrix)
}

formula.maker <- function(df, y, intercept = T){
  # provided with a df and a dependent variable name
  # this generates a formula for estimation in R, y is the 
  # dependent variable, all the others are considered
  # independent and explanatory ones
  
  if (intercept){
    fomu <- as.formula(paste(y, 
                             paste(names(df)[names(df)!=y], collapse='+'),
                             # paste(c(0,names(df)[names(df)!=y]), collapse='+'),
                             # this prevents to have a constant but breaks the
                             # functioning of the code 
                             sep='~'))
  } else {
    fomu <- as.formula(paste(y,
                             paste(c(0,names(df)[names(df)!=y]), collapse='+'),
                             # this prevents to have a constant but breaks the
                             # functioning of the code somewhere
                             sep='~'))
  }
  
  
  attr(fomu, which='.Environment') <- .GlobalEnv
  return(fomu)
}

##### Packages Loader #####

pkgs <- c('tidyverse',
          'stargazer',
          'broom',
          'devtools', 
          'readxl',
          'R.matlab', 
          'matlabr', 
          'tictoc')
# fill pkgs with names of the packages to install

invisible(instant_pkgs(pkgs))

invisible(devtools::install_github('ceschi/urcabis'))



#### housekeeping ####
rm(pkgs)