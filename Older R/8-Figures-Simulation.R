###########################################################################
###########################################################################
## 8 - FIGURE 1 SIMULATION - RECONCILING INDIVIDUALS WITH THE POPULATION ##
###########################################################################
###########################################################################
## Returns:
## Simulated figures showing:
##    a) All individuals following the functional response
##    b) Some individuals following the functional response,
##       while others do not change their behaviour and exhibit
##       the opposite response
##    c) Mean R-squared values between the functional response model
##       and (1) the individuals that follow the FR, (2) the
##       individuals that don't

### Packages ----
libs <- c('data.table', 'tidyverse', 'grid', 'cowplot', 'gridExtra')
lapply(libs, require, character.only = TRUE)

indivs <- data.table(id=LETTERS[seq(1:10)], 
                     intercepts=sample(seq(0,50,0.1), 10, replace=TRUE), 
                     slopes=c(seq(-0.5,0.75,length.out =5), seq(0.75,2,length.out =5)),
                     h1_avail=seq(50,150, length.out=10)) %>%
  mutate(reorder_slopes=sample(slopes))

h2_avail <- seq(50,150,1)

indiv_eqs <- data.table()
for(i in unique(indivs$id)){
  indiv_sub <- indivs[id==i]
  beta <- indiv_sub$intercepts + indiv_sub$slopes*h2_avail
  indiv_sel <- data.table(id=i, 
                          h2=h2_avail, 
                          h1=indiv_sub$h1_avail, 
                          beta, 
                          slope=indiv_sub$slopes,
                          new_slope=indiv_sub$reorder_slopes,
                          intercept=indiv_sub$intercepts)
  indiv_eqs <- rbind(indiv_eqs, indiv_sel)
}

indiv_eqs_fr <- indiv_eqs %>% 
  select(colnames(indiv_eqs)[!colnames(indiv_eqs) %in% 'new_slope'])
indiv_eqs_no_fr <- indiv_eqs %>% 
  select(colnames(indiv_eqs)[!colnames(indiv_eqs) %in% 'slope']) %>% 
  rename(slope=new_slope)


fr_low <- indiv_eqs[id %in% c('A','B','C','D','E')] %>%
  group_by(h2) %>% summarise(beta=mean(beta))

fr_mean <- indiv_eqs %>%
  group_by(h2) %>% summarise(beta=mean(beta))

fr_high <- indiv_eqs[id %in% c('F','G','H','I','J')] %>%
  group_by(h2) %>% summarise(beta=mean(beta))

fr_plots <- list()
for(plot_type in c('fr', 'no_fr')){
  
  dat <- get(paste('indiv_eqs', plot_type, sep='_'))
  anno <- ifelse(plot_type=='fr', 'A', 'B')
  
  p <- ggplot() + 
    geom_line(data=dat[dat$h2 < 140 & dat$h2 > 60], aes(x=h2, y=beta, group=id, colour=slope), size=0.5, lineend='round') +
    geom_line(data=fr_low, size=1, lineend='round', aes(x=h2, y=beta)) +
    geom_line(data=fr_mean, size=1, lineend='round', aes(x=h2, y=beta)) +
    geom_line(data=fr_high, size=1, lineend='round', aes(x=h2, y=beta)) +
    scale_colour_viridis_c(breaks=c(-.25,1.75), labels=c('L', 'H')) +
    # Theme
    theme(panel.background = element_rect(fill=NA), axis.text = element_blank(), 
          axis.title = element_blank(), axis.ticks= element_blank(), 
          legend.text = element_text(size=15), legend.title=element_blank()) +
    # annotate labels
    annotate('text', x=55,y=350, label=anno, size=6) +
    annotate('text', x=155, y=max(fr_low$beta), label='L', size=5) +
    annotate('text', x=155, y=max(fr_mean$beta), label='M', size=5) +
    annotate('text', x=155, y=max(fr_high$beta), label='H', size=5) +
    # add axis arrows
    # y axis
    geom_segment(aes(x=30, xend=30, y=-150, yend = 400), size=1, arrow = arrow(length = unit(0.3,"cm")), colour='#3C4489') +
    # x axis
    geom_segment(aes(x=30, xend=170, y=-150, yend = -150), size=1, arrow = arrow(length = unit(0.3,"cm")), colour='#3C4489') +
    xlim(30,170)
  
  if(plot_type=='fr'){
    p <- p + theme(legend.position = 'none')
  }
  
  fr_plots[[plot_type]] <- p
  
}

# tiff("figures/fig1.tiff", width = 6, height = 3, units = 'in', res = 300)
grid.arrange(grobs=c(fr_plots['fr'], fr_plots['no_fr']), ncol=2, nrow=1, widths=c(5.5,6.5),
             left=textGrob(expression('Selection for h'[1]), gp=gpar(col="#000000", fontsize=15), rot=90), 
             bottom=textGrob(expression('Availability h'[1]), gp=gpar(col="#000000", fontsize=15)), align=h)

