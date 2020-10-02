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
  
irfs_z <- map(.x = models_zshock,
              .f = irfs_z) %>% 
  bind_rows() %>% 
  arrange(mod, var, quarter)

##### simulated inflation ######################################################

sim_inflation <- map(.x = c(models, models_zshock),
                     .f = simpi) %>% 
  bind_rows() 


##### set exo lags and list #####

k = 5; llags = 80; n = length(sim_inflation);

infl <- list(names = list(c(models, models_zshock)),
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




for (i in 1:ncol(sim_inflation)){
  tic(infl[['names']][[i]])



  # retrieve optimal lags from tweaked urca, Bayes Info Criterion
  infl[['optilags']][[i]] <- ur.df(sim_inflation[[i]] %>% dplyr::select(pi) %>% as.matrix(),
                              lags = llags,
                              selectlags = 'BIC') %>% 
                            slot(., 'optilags')

  # estimate AR with optimal lags according to BIC
  infl[['optik']][[i]] <- lm(data = sim_inflation[[i]] %>% lagger_bis(lag = infl[['optilags']][[i]]),
                             formula = formula.maker(df = sim_inflation[[i]] %>% 
                                                       dplyr::select(pi) %>% 
                                                                     lagger_bis(lag=infl[['optilags']][[i]]),
                                                        y = 'pi')
                                ) 
  
  df <- infl[['optik']][[i]]  %>% broom::tidy() %>% .[-(1:2),]
  df$term <- 2:(nrow(df)+1)


# stock plots for the coefficients
  infl[['plots']][[i]]  <- ggplot(data = df,
                                  aes(x = term, y = estimate)) +
                           geom_col(aes(y = p.value/10, x = term), colour = 'blue', alpha = 1, width = .1)+
                           geom_hline(aes(yintercept = 0.01/10), colour = 'blue', size = .8)+
                           geom_line(colour = 'black', size = 1) +
                           geom_line(aes(y = estimate + 2*std.error), colour = 'red') + 
                           geom_line(aes(y = estimate - 2*std.error), colour = 'red') +
                           geom_hline(aes(yintercept = 0), colour = 'black', size = .1)+
                           ylim(-.03, .1)+
                           theme_bw() + ylab('Coefficient estimate') + xlab('Lags')+
                           ggtitle(paste0(infl[['names']][[i]], ': optimal lags estimates')) +
                           labs(subtitle = 'Point estimate is the black line, red lines are 2*SE bands, blue bars are p-values, horizontal blue line is 1% sign. threshold.')

  ggsave(filename = paste0(infl[['names']][[i]], ' optilags.pdf'),
         plot = infl[['plots']][[i]],
         device = 'pdf',
         height = 8, width = 14.16, units = 'in',
         path = file.path(graphs_dir))

  cat('\nDone with model '); toc()


  gc()
}


# housekeeping 
  rm(df, i, k, llags)