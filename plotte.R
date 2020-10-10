##### Visuals #################################################################

# script reproducing IRFs and other plots from the paper

##### Fig.1 ###################################################################
# comparison of Gali and liq_tp to TFP shock
fig1 <- cowplot::plot_grid(nrow = 3, align = 'hv',
                           
                           fig1_ygap = irfs_all %>% 
                             dplyr::filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
                                    var == 'y_gap') %>% 
                             ggplot(data = .) +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Output gap'),
                           
                           
                           fig1_pi = irfs_all %>% 
                             dplyr::filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
                                    var == 'pi') %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = .8, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Inflation'),
                           
                           
                           fig1_polr = irfs_all %>% 
                             dplyr::filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'tfp',
                                    var %in% c('i', 's')) %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = .8, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Policy rate')
                           ) +
        cowplot::draw_figure_label(label = 'Quarters', 
                                   position = 'bottom')


##### Fig. 2 ###################################################################
# comparison of Gali and liq_tp to MP shock

fig2 <- cowplot::plot_grid(nrow = 3,  
                           align = 'hv',
                           fig2_ygap = irfs_all %>% 
                             dplyr::filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'mp',
                                    var == 'y_gap') %>% 
                             ggplot(data = .) +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Output gap'),
                           
                           
                           fig2_pi = irfs_all %>% 
                             dplyr::filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'mp',
                                    var == 'pi') %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = .8, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Inflation'),
                           
                           
                           fig2_polr = irfs_all %>% 
                             dplyr::filter(mod %in% c('gali_standard', 'liq_tp'), 
                                    shock == 'mp',
                                    var %in% c('i', 's')) %>% 
                             ggplot() +
                             geom_line(aes(x = quarter, 
                                           y = value, 
                                           group = mod, 
                                           linetype = mod), 
                                       size = .8, 
                                       alpha = 1, 
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                                   theme_bw() +
                                   theme(legend.position = 'none', 
                                         axis.title.x = element_blank()) + 
                                   ylab('Policy rate')
                        ) +
        cowplot::draw_figure_label(label = 'Quarters', 
                                   position = 'bottom')


##### Fig. 3 ###################################################################
# Comparison of liq_tp & liq_notp, all vars

fig3 <- cowplot::plot_grid(nrow = 3, 
                           ncol = 2, 
                           byrow = F, 
                           align = 'hv',
                           
                           ygap = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Output Gap'),
                           
                           polr = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 's') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Policy Rate'),
                           
                           bond = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'b') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Liquid Bonds'),
                           
                           pi = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'pi') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Inflation'),
                           
                           mon = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Money Holdings'),
                           
                           liq = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'mp',
                                    var == 'z') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() +
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) + 
                             ylab('Real Liquidity')
                        ) +
        cowplot::draw_figure_label(label = 'Quarters', 
                                   position = 'bottom')

##### Fig. 3.1 #################################################################
# comparison liq_tp, liq_notp to mp shock

fig3.1 <- cowplot::plot_grid(nrow = 3, ncol = 2, 
                             byrow = F, 
                             align = 'hv',
                           
                           ygap = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Output Gap'),
                           
                           polr = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 's') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Policy Rate'),
                           
                           bond = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'b') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none', 
                                   axis.title.x = element_blank()) +
                             ylab('Liquid Bonds'),
                           
                           pi = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'pi') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Inflation'),
                           
                           mon = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'm') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Money Holdings'),
                           
                           liq = irfs_all %>% 
                             dplyr::filter(mod %in% c('liq_tp', 'liq_notp'),
                                    shock == 'tfp',
                                    var == 'z') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Real Liquidity')
                           
) +
        cowplot::draw_figure_label(label = 'Quarters', 
                                   position = 'bottom')


##### Fig.4 ####################################################################♦
# liq_tp, liq_notp: liquidity dry-up

fig4 <- cowplot::plot_grid(nrow = 3, ncol = 2, 
                           byrow = F, 
                           align = 'hv',
                           
                           ygap = irfs_zshock %>% 
                             dplyr::filter(.$var == 'y_gap') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Output Gap'),
                           
                           polr = irfs_zshock %>% 
                             dplyr::filter(var == 's') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Policy Rate'),
                           
                           bond = irfs_zshock %>% 
                             dplyr::filter(var == 'b') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Liquid Bonds'),
                           
                           pi = irfs_zshock %>% 
                             dplyr::filter(var == 'pi') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Inflation'),
                           
                           mon = irfs_zshock %>% 
                             dplyr::filter(var == 'm') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Money Holdings'),
                           
                           liq = irfs_zshock %>% 
                             dplyr::filter(var == 'z') %>%
                             ggplot() +
                             geom_line(aes(x = quarter,
                                           y = value,
                                           group = mod,
                                           linetype = mod),
                                       size = .8,
                                       alpha = 1,
                                       colour = 'black') +
                             geom_hline(yintercept = 0) +
                             theme_bw() + 
                             theme(legend.position = 'none',
                                   axis.title.x = element_blank()) +
                             ylab('Real Liquidity')
 
                        ) + 
        cowplot::draw_figure_label(label = 'Quarters', 
                                   position = 'bottom')

##### Figs lags ################################################################
# plots autocorrelation with significance for the nine models

ac_plots <- pmap(.l = list(lm_ar = infl$optik,
                          .name = infl$names),
                .f = plot_lags
                )

ac_plots_p1 <- pmap(.l = list(lm_ar = infl$optik,
                              .name = infl$names,
                              selector = fm_apply(F,n)),
                    .f = plot_lags
                   )

##### Tab from stargazer #######################################################


sink('oo')
stargazer(infl$exolags,
          covariate.labels = c('Const.', paste0('\\nth{', 1:5, '} lag')),
          # column.labels = c('\\citet{gali15}', 
          #                   'Liq. $\\gamma=1.8$', 
          #                   'Liq. $\\gamma=.5$', 
          #                   '\\citet{ascardone14}', 
          #                   '\\citet{smetswouters07}'),
          column.sep.width = "1pt",
          align = T,
          font.size = 'small',
          out.header = F,
          header = F,
          initial.zero = F,
          model.numbers = T,
          keep.stat = c('adj.rsq', 'bic'),
          dep.var.labels = 'Simulated Inflation',
          df = F,
          notes.align = 'r',
          float = F,
          intercept.bottom = F, 
          nobs = F,
          label = 'tab:ar5_reg'
         ) %>% 
   write(x = .,
         file = file.path(d_tabs, 'ar5_reg.tex')) %>% 
   capture.output()
sink(NULL)
unlink('oo')

##### Simulated persistence ####################################################

persi <- infl$sim_persistence %>% 
        dplyr::filter(mod %in% models) %>% 
        ggplot() + 
        geom_col(aes(x = mod, 
                     y = rho, 
                     fill = tag), 
                 position = 'dodge') +
        theme_minimal() + 
        ylab('Sum of coefficients')+ 
        scale_color_viridis_d(aesthetics = 'fill', 
                              begin = .3, 
                              end = .7, 
                              option = 'B')+
        geom_text(aes(label = lags, 
                      x = interaction(mod), 
                      y = rho+.05, 
                      group = tag),
                  position = position_dodge(1))+
        xlab('Models') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank(), 
              legend.direction = 'horizontal')

persi_2 <- infl$sim_persistence_rsq %>%
        dplyr::filter(mod %in% models) %>%
        ggplot() +
        geom_col(aes(x = mod, 
                     y = rho, 
                     fill = tag), 
                 position = 'dodge') +
        theme_minimal() + ylab('Sum of coefficients') +
        scale_color_viridis_d(aesthetics = 'fill', 
                              label = c('All lags', 'Only Sign. Lags'),
                              begin = .5, 
                              end = .7, 
                              option = 'B')+
        geom_text(aes(label = lags, 
                      x = interaction(mod), 
                      y = rho+.05, 
                      group = tag),
                  position = position_dodge(1))+
        xlab('Models') + 
        theme(legend.position = 'bottom', 
              legend.title = element_blank(), 
              legend.direction = 'horizontal')


##### Limit cases for NKDSGE ###################################################
# TODO: needs improving!
plot_extrank <- irfs_all %>%
        filter(mod %in% c('gali_gammainf')) %>%
        ggplot(aes(x = quarter,
                   y = value,
                   group = var,
                   colour = mod))+
        geom_line() +
        facet_wrap(mod + shock ~ var, scales = 'free')

plot_extraNK_1 <- cowplot::plot_grid(nrow = 2,
                                     
                                     tfp_all = irfs_all %>% 
                                             filter(mod == 'gali_gamma1',
                                                    shock == 'tfp') %>% 
                                             ggplot(aes(x = quarter,
                                                        y = value,
                                                        group = var))+
                                             geom_line(size = 1) +
                                             geom_hline(yintercept = 0) +
                                             facet_wrap(.~var, scales = 'free')+
                                             theme_bw() + 
                                             ggtitle('TFP shock') +
                                             theme(legend.position = 'none', 
                                                   axis.title = element_blank(),
                                                   plot.title = element_text(hjust = .5)),
                                     
                                     mp_all = irfs_all %>% 
                                             filter(mod == 'gali_gamma1',
                                                    shock == 'mp') %>% 
                                             ggplot(aes(x = quarter,
                                                        y = value,
                                                        group = var))+
                                             geom_line(size = 1) +
                                             geom_hline(yintercept = 0) +
                                             facet_wrap(.~var, scales = 'free')+
                                             theme_bw() +
                                             ggtitle('Interest Rate Shock') +
                                             theme(legend.position = 'none', 
                                                   axis.title = element_blank(),
                                                   plot.title = element_text(hjust = .5))
) + 
        cowplot::draw_figure_label(label = 'Quarters',
                                   position = 'bottom')

plot_extraNK_inf <- cowplot::plot_grid(nrow = 2,
                                     
                                     tfp_all = irfs_all %>% 
                                             filter(mod == 'gali_gammainf',
                                                    shock == 'tfp') %>% 
                                             ggplot(aes(x = quarter,
                                                        y = value,
                                                        group = var))+
                                             geom_line(size = 1) +
                                             geom_hline(yintercept = 0) +
                                             facet_wrap(.~var, scales = 'free')+
                                             theme_bw() + 
                                             ggtitle('TFP shock') +
                                             theme(legend.position = 'none', 
                                                   axis.title = element_blank(),
                                                   plot.title = element_text(hjust = .5)),
                                     
                                     mp_all = irfs_all %>% 
                                             filter(mod == 'gali_gammainf',
                                                    shock == 'mp') %>% 
                                             ggplot(aes(x = quarter,
                                                        y = value,
                                                        group = var))+
                                             geom_line(size = 1) +
                                             geom_hline(yintercept = 0) +
                                             facet_wrap(.~var, scales = 'free')+
                                             theme_bw() +
                                             ggtitle('Interest Rate Shock') +
                                             theme(legend.position = 'none', 
                                                   axis.title = element_blank(),
                                                   plot.title = element_text(hjust = .5))
) + 
        cowplot::draw_figure_label(label = 'Quarters',
                                   position = 'bottom')

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

ggsave(filename = file.path(d_plots, 'tp_notp_mp.pdf'),
       plot = fig3,
       device = 'pdf',
       units = 'in',
       width = 8,
       height = 9*8/16)

ggsave(filename = file.path(d_plots, 'tp_notp_tfp.pdf'),
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


##### housekeeping #############################################################
fig_list <- list(fig1, 
                 fig2, 
                 fig3, 
                 fig3.1, 
                 fig4,
                 ac_plots,
                 ac_plots_p1,
                 persi,
                 persi_2,
                 plot_extraNK_1,
                 plot_extraNK_inf)

rm(fig1, 
   fig2, 
   fig3, 
   fig3.1, 
   fig4,
   ac_plots,
   ac_plots_p1,
   persi,
   persi_2,
   plot_extraNK_1,
   plot_extraNK_inf)