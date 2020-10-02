##### Visuals #################################################################

# script reproducing IRFs and other plots from the paper

##### Fig.1 ###################################################################
# comparison of Gali and liq_tp to TFP shock
fig1 <- cowplot::plot_grid(nrow = 3,
                           
                           fig1_ygap = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
                                    var == 'y_gap') %>% 
                             ggplot(data = .) +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.21.2, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Output gap'),
                           
                           
                           fig1_pi = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
                                    var == 'pi') %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = 1.21.2, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Inflation'),
                           
                           
                           fig1_polr = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
                                    var %in% c('i', 's')) %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = 1.21.2, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none') + 
                             ylab('Policy rate') +
                             xlab('Quarters')
                           
                           )


##### Fig. 2 ###################################################################
# comparison of Gali and liq_tp to MP shock

fig2 <- cowplot::plot_grid(nrow = 3,
                           fig2_ygap = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'mp',
                                    var == 'y_gap') %>% 
                             ggplot(data = .) +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Output gap'),
                           
                           
                           fig2_pi = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'mp',
                                    var == 'pi') %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = 1.2, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Inflation'),
                           
                           
                           fig2_polr = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'mp',
                                    var %in% c('i', 's')) %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = 1.2, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none') + 
                             ylab('Policy rate') +
                             xlab('Quarters')
                           
                           )


##### Fig. 3 ###################################################################
# Comparison of liq_tp & liq_notp, all vars
#' *TODO: fix line size*
fig3 <- cowplot::plot_grid(nrow = 3, ncol = 2, byrow = F,
                           
                           ygap = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Output Gap'),
                           
                           polr = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 's') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Policy Rate'),
                           
                           bond = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'b') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none') +
                             ylab('Liquid Bonds') +
                             xlab('Quarters'),
                           
                           pi = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'pi') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Inflation'),
                           
                           mon = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Money Holdings'),
                           
                           ygap = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'z') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none') +
                             ylab('Real Liquidity') +
                             xlab('Quarters')
                           
                           )

##### Fig. 3.1 #################################################################
# comparison liq_tp, liq_notp to mp shock

fig3.1 <- cowplot::plot_grid(nrow = 3, ncol = 2, byrow = F,
                           
                           ygap = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Output Gap'),
                           
                           polr = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 's') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Policy Rate'),
                           
                           bond = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'b') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none') +
                             ylab('Liquid Bonds') +
                             xlab('Quarters'),
                           
                           pi = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'pi') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Inflation'),
                           
                           mon = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'm') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Money Holdings'),
                           
                           ygap = irfs_all %>% 
                             filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'z') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none') +
                             ylab('Real Liquidity') +
                             xlab('Quarters')
                           
)


##### Fig.4 ####################################################################♦
# liq_tp, liq_notp: liquidity dry-up

fig4 <- cowplot::plot_grid(nrow = 3, ncol = 2, byrow = F,
                           
                           ygap = irfs_z %>% 
                             filter(var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Output Gap'),
                           
                           polr = irfs_z %>% 
                             filter(var == 's') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Policy Rate'),
                           
                           bond = irfs_z %>% 
                             filter(var == 'b') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none') +
                             ylab('Liquid Bonds') +
                             xlab('Quarters'),
                           
                           pi = irfs_z %>% 
                             filter(var == 'pi') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Inflation'),
                           
                           mon = irfs_z %>% 
                             filter(var == 'm') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Money Holdings'),
                           
                           ygap = irfs_z %>% 
                             filter(var == 'z') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = 1.2,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none') +
                             ylab('Real Liquidity') +
                             xlab('Quarters')
 
)

##### Save to pdf ##############################################################