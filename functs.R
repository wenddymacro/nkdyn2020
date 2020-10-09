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

lm_custom <- function(data, .lags, interc = F){
  
  # this function takes a df, selects a provided list of vars,
  # runs a linear model with provided lags, outputs the results
  
  # ad hoc version of the function
  invisible(require(dplyr))
  
  transformed_data <- data %>% 
    dplyr::select('pi') %>% 
    lagger_bis(lag = .lags)
  
  formulavars <- formula.maker(df = transformed_data,
                               intercept = interc,
                               y = 'pi')
  
  out_lm <- lm(data = transformed_data,
               formula = formulavars)
  return(out_lm)
  
}

optilags <- function(data, .maxlags, .type = 'drift'){
  # given a series and an upper bound, computes the optimal number of lags
  # according to BIC
  
  data_v <- data %>% 
    dplyr::select(pi) %>% 
    as.matrix()
  
  optilags <- urca::ur.df(y = data_v, 
                          type = .type,
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

plot_lags <- function(lm_ar, .name, .dir = d_plots, selector = T){
  
  # function to plot along coefficients on lags and significance bars
  # selects only past the first lag by default
  
  invisible(require(broom))
  invisible(require(dplyr))
  invisible(require(ggplot2))
  invisible(require(stringi))
  invisible(require(cowplot))
  
  if (length(lm_ar$coefficients)>3){
    if (selector){
      tidy_lm <- tidy(lm_ar) %>% 
        filter(term != '(Intercept)', 
               term != 'pi.1') %>% 
        mutate(idx = sub('pi.', '', term) %>% 
                 as.numeric())
      
      filnam <- file.path(.dir, paste0(.name,'_lagplot.pdf'))
      
    }else{
      tidy_lm <- tidy(lm_ar) %>% 
        filter(term != '(Intercept)') %>% 
        mutate(idx = sub('pi.', '', term) %>% 
                 as.numeric())
      
      filnam <- file.path(.dir, paste0(.name,'_alllagplot.pdf'))
      
    }
    
    # max_est <- max(abs(tidy_lm$estimate))
    
    plt_lin <- tidy_lm %>% 
      ggplot() +
      geom_ribbon(aes(x = idx,
                      ymin = (estimate - 2*std.error),
                      ymax = (estimate + 2*std.error)),
                  colour = 'red',
                  alpha = .25,
                  size = .5) +
      geom_line(aes(x = idx,
                    y = estimate),
                size = .8,
                alpha = 1,
                colour = 'black') +
      geom_hline(yintercept = 0,
                 colour = 'black') + 
      theme_minimal() + ylab('Coefficient est.') +
      theme(axis.title.x = element_blank())
    
    plt_bar <- tidy_lm %>% 
      ggplot() + 
      geom_col(aes(y = p.value,
                   x = idx),
               colour = 'blue',
               fill = 'blue',
               width = .5) + 
      geom_hline(yintercept = .05) +
      theme_minimal() + xlab('Lag') +
      ylab('Significance')
    
    cwplt <- cowplot::plot_grid(plt_lin,
                                plt_bar,
                                nrow = 2,
                                align = 'hv')
    
    ggsave(filename = filnam,
           device = 'pdf', 
           plot = cwplt, 
           units = 'in',
           width = 8,
           height = 9*8/16)
    
    print(cwplt)
  }else{
      cat('\n\nToo few lags.\n\n')
    }
  
}

pir_sim <- function(llm, nam, pval=.01){
  # takes a lm model, stores n of lags,
  # sum over coefs, computes sum of covar matrix, 
  # repeats the same for significant lags
  # outputs tibble
  
  invisible(require(broom))
  invisible(require(dplyr))
  invisible(require(tibble))
  
  tdy_llm <- llm %>% 
    tidy()
  
  const_flag <- attr(attr(llm$model, "terms"), "intercept")
  
  if (const_flag) {
    
    tbl_alllags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)') %>% 
        dplyr::select(estimate) %>% 
        sum(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'all_lags',
      mod = nam,
      lags = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)') %>% 
        nrow()
    )
    
    tbl_siglags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)',
               p.value <= pval) %>% 
        dplyr::select(estimate) %>% 
        sum(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'sig_lags',
      mod = nam,
      lags = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)',
                      p.value <= pval) %>% 
        nrow()
    )
    
  }else{
    
    tbl_alllags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)') %>% 
        dplyr::select(estimate) %>% 
        sum(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'all_lags',
      mod = nam,
      lags = tdy_llm %>% 
        nrow()
    )
    
    tbl_siglags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(p.value <= pval) %>% 
        dplyr::select(estimate) %>% 
        sum(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'sig_lags',
      mod = nam,
      lags = tdy_llm %>% 
        dplyr::filter(p.value <= pval) %>% 
        nrow()
    )
    
  }
  
return(bind_rows(tbl_alllags, tbl_siglags))
  
}

pir_sim_sq <- function(llm, nam, pval=.01){
  # takes a lm model, stores n of lags,
  # sum over coefs, computes sum of covar matrix, 
  # repeats the same for significant lags
  # outputs tibble
  
  invisible(require(broom))
  invisible(require(dplyr))
  invisible(require(tibble))
  
  tdy_llm <- llm %>% 
    tidy()
  
  const_flag <- attr(attr(llm$model, "terms"), "intercept")
  
  if (const_flag) {
    
    tbl_alllags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)') %>% 
        dplyr::select(estimate) %>% 
        `^`(., 2) %>% 
        sum() %>% 
        sqrt() ,
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'all_lags',
      mod = nam,
      lags = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)') %>% 
        nrow()
    )
    
    tbl_siglags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)',
                      p.value <= pval) %>% 
        dplyr::select(estimate) %>%
        `^`(., 2)  %>%  
        sum()%>% 
        sqrt(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'sig_lags',
      mod = nam,
      lags = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)',
                      p.value <= pval) %>% 
        nrow()
    )
    
  }else{
    
    tbl_alllags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(term != '(Intercept)') %>% 
        dplyr::select(estimate) %>% 
        `^`(., 2) %>% 
        sum() %>% 
        sqrt(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'all_lags',
      mod = nam,
      lags = tdy_llm %>% 
        nrow()
    )
    
    tbl_siglags <- tibble(
      rho = tdy_llm %>% 
        dplyr::filter(p.value <= pval) %>% 
        dplyr::select(estimate) %>%
        `^`(., 2) %>% 
        sum() %>% 
        sqrt(),
      se = vcov(llm) %>% 
        .[2:nrow(.), 2:ncol(.)] %>% 
        sum(),
      tag = 'sig_lags',
      mod = nam,
      lags = tdy_llm %>% 
        dplyr::filter(p.value <= pval) %>% 
        nrow()
    )
    
  }
  
  return(bind_rows(tbl_alllags, tbl_siglags))
  
}

##### Packages Loader #####

pkgs <- c('tidyverse',
          'stargazer',
          'broom',
          'devtools', 
          'readxl',
          'R.matlab', 
          'matlabr', 
          'tictoc',
          'furrr')
# fill pkgs with names of the packages to install

invisible(instant_pkgs(pkgs))

invisible(devtools::install_github('ceschi/urcabis'))



#### housekeeping ####
rm(pkgs)