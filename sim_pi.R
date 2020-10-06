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

##### IRFs #####################################################################
irfs_all <- map(.x = models,
                .f = irfs) %>% 
              bind_rows() %>% 
              arrange(mod, shock, var, quarter)
  
irfs_zshock <- map(.x = models_zshock,
              .f = irfs_z) %>% 
            bind_rows() %>% 
            arrange(mod, var, quarter)

##### simulated inflation ######################################################

sim_inflation <- map(.x = c(models, models_zshock),
                     .f = simpi) 


##### set exo lags and list #####

k = 5; llags = 120; n = length(sim_inflation);

infl <- list(names = c(models, models_zshock),
      			 exolags = list(),
      			 optilags = list(),
             optik = list(),
             plots = list(),
      			 sim_inflation = sim_inflation
						)


##### AR5 OLS ##################################################################
infl[['exolags']] <- pmap(.l = list(data = infl$sim_inflation,
                                    .lags = fm_apply(k, n),
                                    interc = fm_apply(T, n)),
                          .f = lm_custom)

##### Optimal lags #############################################################
infl[['optilags']] <- pmap(.l = list(data = infl$sim_inflation,
                                     .maxlags = llags),
                           .f = optilags)

##### ARk ######################################################################
infl[['optik']] <- pmap(.l = list(data = infl$sim_inflation,
                                  .lags = infl[['optilags']],
                                  interc = fm_apply(T, n)),
                        .f = lm_custom)

infl$sim_persistence <- pmap(.l = list(llm = infl$optik,
                                       nam = infl$names),
                             .f = pir_sim) %>% 
  bind_rows()

infl$sim_persistesce_rsq <- pmap(.l = list(llm = infl$optik,
                                           nam = infl$names),
                                 .f = pir_sim_sq) %>% 
  bind_rows()
