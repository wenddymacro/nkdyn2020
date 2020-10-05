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
                           
                           ygap = irfs_zshock %>% 
                             filter(.$var == 'y_gap') %>%
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
                           
                           polr = irfs_zshock %>% 
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
                           
                           bond = irfs_zshock %>% 
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
                           
                           pi = irfs_zshock %>% 
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
                           
                           mon = irfs_zshock %>% 
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
                           
                           liq = irfs_zshock %>% 
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

acplots <- pmap(.l = list(lm_ar = infl$optik,
                          .name = infl$names),
                .f = plot_lags
                )

##### Tab from stargazer #######################################################


sink('oo')
stargazer(infl$exolags[c(1,4,5,6,7)],
                    # type = 'latex', 
                    covariate.labels = c('Const.', paste0('\\nth{', 1:5, '} lag')), 
                    # style = 'aer',
                    column.labels = c('\\citet{gali15}', 
                                      'Liq. TP', 
                                      'Liq. no TP', 
                                      '\\citet{ascardone14}', 
                                      '\\citet{smetswouters07}'),
                    align = T,
                    font.size = 'small',
                    out.header = F,
                    header = F,
                    initial.zero = F,
                    model.numbers = F,
                    omit.stat = c('ser', 
                                  # 'adj.rsq', 
                                  'f'),
                    # dep.var.labels.include = F, 
                    dep.var.labels = 'Simulated Inflation',
                    df = F,
                    notes.align = 'l',
                    float = T,
                    intercept.bottom = F, 
                    nobs = F, 
                    # keep.stat = 'bic',
                    label = 'tab:ar5_reg'
                    ) %>% 
   gsub(pattern = '\\caption{}',
        replacement = '',
        x = ., 
        fixed = T) %>%
   gsub(pattern = '\\end{tabular}', 
        replacement = '', 
        x = ., 
        fixed = T) %>% 
   gsub(pattern = "\\textit{Note:}  & \\multicolumn{5}{l}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\",
        replacement = '\\end{tabular}
                       \\caption{$AR\\left(5\\right)$ estimates on simulated data from 4 models. 
                        Only technological and monetary policy shocks are allowed and each model 
                        is simulated for 500000 periods, after discarding the first 100000 iterations. All shocks
                        are set to have zero mean, equal variance, and are iid. Second and third columns
                        present estimates for our model with liquidity, complying to the Taylor Principle
                        and violating it, respectively. Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}',
        fixed=T,
        x = .) %>% 
   gsub(pattern = '\n ', 
        replacement = '', 
        fixed = T, 
        x = .) %>% 
   write(x = .,
         file = file.path(d_tabs, 'ar5_reg.tex')) %>% 
   capture.output()
sink(NULL)
unlink('oo')
                     
   

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



##### housekeeping #############################################################
fig_list <- list(fig1, 
                 fig2, 
                 fig3, 
                 fig3.1, 
                 fig4,
                 unlist(acplots))

rm(fig1, 
   fig2, 
   fig3, 
   fig3.1, 
   fig4)