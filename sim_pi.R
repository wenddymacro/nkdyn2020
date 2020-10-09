###### Econometrics on simulated inflation
# imports simulated series 
# and estimates AR(p*)
# plus plots results


models <- c('gali_standard',
            'gali_gamma1',
            'gali_gammainf',
            'liq_notp',
            'liq_tp',
            'ascardone14', 
            'sw07')

models_zshock <- c('liq_notp_zshock',
                   'liq_tp_zshock')

mod_select <- c(1,4:7)

##### IRFs #####################################################################
irfs_all <-future_map(.x = models,
                .f = irfs) %>% 
              bind_rows() %>% 
              arrange(mod, shock, var, quarter)
  
irfs_zshock <-future_map(.x = models_zshock,
              .f = irfs_z) %>% 
            bind_rows() %>% 
            arrange(mod, var, quarter)

##### simulated inflation ######################################################

sim_inflation <-future_map(.x = c(models, models_zshock),
                     .f = simpi) 


##### set exo lags and list #####

k = 5; llags = 120; n = length(mod_select); 

infl <- list(names = models[mod_select],
      			 exolags = list(),
      			 optilags = list(),
             optik = list(),
             plots = list(),
      			 sim_inflation = sim_inflation[mod_select]
						)


##### AR5 OLS ##################################################################
infl[['exolags']] <- future_pmap(.l = list(data = infl$sim_inflation,
                                    .lags = fm_apply(k, n),
                                    interc = fm_apply(T, n)),
                          .f = lm_custom)

##### Optimal lags #############################################################
infl[['optilags']] <- future_pmap(.l = list(data = infl$sim_inflation,
                                            .maxlags = llags,
                                            .type = 'none'),
                                  .f = optilags)

##### ARk ######################################################################
infl[['optik']] <- future_pmap(.l = list(data = infl$sim_inflation,
                                  .lags = infl[['optilags']],
                                  interc = fm_apply(T, n)),
                        .f = lm_custom)

# ratio of significant lags
infl[['sigk']] <- infl$optik %>% 
  lapply(FUN = function(x){
                  return(100*
                (  x %>%
                    tidy() %>% 
                    filter(term != '(Intercept)',
                           p.value < .01) %>% 
                    nrow())
                  /
                (  x %>% 
                    tidy() %>% 
                    filter(term != '(Intercept)') %>% 
                    nrow())
                  ) %>% round()
                          }
         )


##### tentative persistence computation ########################################
infl$sim_persistence <- future_pmap(.l = list(llm = infl$optik,
                                       nam = infl$names),
                             .f = pir_sim) %>% 
  bind_rows()

infl$sim_persistence_rsq <- future_pmap(.l = list(llm = infl$optik,
                                           nam = infl$names),
                                 .f = pir_sim_sq) %>% 
  bind_rows()
