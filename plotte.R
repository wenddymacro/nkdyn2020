##### Visuals #################################################################

# script reproducing IRFs and other plots from the paper

##### Fig.1 ###################################################################
# comparison of Gali and liq_tp to TFP shock
fig1 <- cowplot::plot_grid(nrow = 3, align = 'v',
                           
                           fig1_ygap = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
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
                           
                           
                           fig1_pi = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
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
                           
                           
                           fig1_polr = irfs_all %>% 
                             filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
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


##### Fig. 2 ###################################################################
# comparison of Gali and liq_tp to MP shock

fig2 <- cowplot::plot_grid(nrow = 3,  
                           align = 'v',
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

fig3 <- cowplot::plot_grid(nrow = 3, 
                           ncol = 2, 
                           byrow = F, 
                           align = 'hv',
                           
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
                           
                           liq = irfs_all %>% 
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

fig3.1 <- cowplot::plot_grid(nrow = 3, ncol = 2, 
                             byrow = F, 
                             align = 'hv',
                           
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
                           
                           liq = irfs_all %>% 
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

fig4 <- cowplot::plot_grid(nrow = 3, ncol = 2, 
                           byrow = F, 
                           align = 'hv',
                           
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
                           
                           liq = irfs_z %>% 
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

##### Figs lags ################################################################
# plots autocorrelation with significance for the nine models

##### Save to pdf ##############################################################

ggsave(filename = file.path(d_plots, 'tp_gali_tfp.pdf'),
       plot = fig1,
       device = 'pdf',
       units = 'in',
       width = 6,
       height = 9*6/16)

ggsave(filename = file.path(d_plots, 'tp_gali_mp.pdf'),
       plot = fig2,
       device = 'pdf',
       units = 'in',
       width = 8,
       height = 9*8/16)

ggsave(filename = file.path(d_plots, 'tp_notp_tfp.pdf'),
       plot = fig3,
       device = 'pdf',
       units = 'in',
       width = 8,
       height = 9*8/16)

ggsave(filename = file.path(d_plots, 'tp_notp_mp.pdf'),
       plot = fig3.1,
       device = 'pdf',
       units = 'in',
       width = 8,
       height = 9*8/16)

ggsave(filename = file.path(d_plots, 'tp_notp_z.pdf'),
       plot = fig4,
       device = 'pdf',
       units = 'in',
       width = 8,
       height = 9*8/16)

