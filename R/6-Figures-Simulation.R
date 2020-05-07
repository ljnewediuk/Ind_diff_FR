############################################################################################
############################################################################################
## 5 - FIGURE 1 SIMULATION - RECONCILING INDIVIDUALS WITH THE POPULATION
############################################################################################
############################################################################################
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
libs <- c('data.table', 'ggplot2', 'grid', 'cowplot', 'gridExtra')
lapply(libs, require, character.only = TRUE)


indivs_pos <- data.table(id=c(1:10), intercepts=sample(seq(0,50,0.1), 10, replace=TRUE), slopes=sample(seq(1,2,0.1), 10))
indivs_neg <- data.table(id=c(1:10), intercepts=sample(seq(150,200,0.1), 10, replace=TRUE), slopes=sample(seq(-2,-1,0.1), 10))

availabilities <- seq(50,150,1)

indiv_pos_eqs <- data.table()

for(i in 1:nrow(indivs_pos)){
  indivs_sub <- indivs_pos[id==i]
  betas <- indivs_sub$intercepts + indivs_sub$slopes*availabilities
  indiv_sel <- data.table(id=rep(i, length(availabilities)), type=rep('positive', length(availabilities)), availability=availabilities, beta=betas)
  indiv_pos_eqs <- rbind(indiv_pos_eqs, indiv_sel)
}

indiv_neg_eqs <- data.table()

for(i in 1:nrow(indivs_neg)){
  indivs_sub <- indivs_neg[id==i]
  betas <- indivs_sub$intercepts + indivs_sub$slopes*availabilities
  indiv_sel <- data.table(id=rep(i, length(availabilities)), type=rep('negative', length(availabilities)), availability=availabilities, beta=betas)
  indiv_neg_eqs <- rbind(indiv_neg_eqs, indiv_sel)
}

##########################################
### PLOT INDIVIDUALS WITH DIFFERENT FR ###

for(fr_dir in c('positive', 'negative')){
  
  if(fr_dir=='positive'){
    most <- indiv_pos_eqs
    few <- indiv_neg_eqs
    anno <- '(d)'
  } else {
    most <- indiv_neg_eqs
    few <- indiv_pos_eqs
    anno <- '(c)'
  }

fr_plot <- ggplot(most[id %in% sample(seq(1,10,1),4)], aes(x=availability, y=beta)) + 
geom_smooth(method=lm, se=FALSE, fullrange=TRUE, col='black', size=3, linetype='solid') + 
geom_line(size=2, lineend='round', aes(group=id, colour='#D63118')) + xlim(30,170) +
geom_line(data=few[id %in% sample(seq(1,10,1),2)], aes(x=availability, y=beta, group=id), colour='#FF0000', size=2, lineend='round', linetype='dashed') +
# Theme
theme(panel.background = element_rect(fill=NA), axis.text = element_blank(), axis.title = element_blank(), axis.ticks= element_blank(), legend.position='none') +
# annotate labels
annotate('text', x=55,y=350, label=anno, size=10) +
# add axis arrows
# y axis
geom_segment(aes(x=30, xend=30, y=-200, yend = 400), size=1.5, arrow = arrow(length = unit(0.6,"cm")), colour='#3C4489') +
# x axis
geom_segment(aes(x=30, xend=170, y=-200, yend = -200), size=1.5, arrow = arrow(length = unit(0.6,"cm")), colour='#3C4489') 

assign(paste(fr_dir, 'different_fr', sep='_'), fr_plot)

}

#####################################
### PLOT INDIVIDUALS WITH SAME FR ###

for(fr_dir in c('positive', 'negative')){
  
  if(fr_dir=='positive'){
    most <- indiv_pos_eqs
    anno <- '(b)'
  } else {
    most <- indiv_neg_eqs
    anno <- '(a)'
  }
  
  fr_plot <- ggplot(most[id %in% sample(seq(1,10,1),6)], aes(x=availability, y=beta)) + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE, col='black', size=2.5, linetype='solid') + 
    geom_line(size=1.5, lineend='round', aes(group=id, colour='#D63118')) + xlim(30,170) +
    # Theme
    theme(panel.background = element_rect(fill=NA), axis.text = element_blank(), axis.title = element_blank(), axis.ticks= element_blank(), legend.position='none') +
    # annotate labels
    annotate('text', x=55,y=350, label=anno, size=10) +
    # add axis arrows
    # y axis
    geom_segment(aes(x=30, xend=30, y=-200, yend = 400), size=1.5, arrow = arrow(length = unit(0.6,"cm")), colour='#3C4489') +
    # x axis
    geom_segment(aes(x=30, xend=170, y=-200, yend = -200), size=1.5, arrow = arrow(length = unit(0.6,"cm")), colour='#3C4489') 
  
  assign(paste(fr_dir, 'same_fr', sep='_'), fr_plot)
  
}



# Plot layout
layout_plots <- rbind(c(1,2), c(3,4), c(5,6))

low_grob <- textGrob(expression('High h'[1]), gp=gpar(col='#000000', fontsize=30))

high_grob <- textGrob(expression('Low h'[1]), gp=gpar(col='#000000', fontsize=30))

y_grob <- textGrob(expression(beta ~ h[2]), gp=gpar(fontface="bold", col="#000000", fontsize=30), rot=90)

x_grob <- textGrob(expression('Availability h'[2]), gp=gpar(fontface="bold", col="#000000", fontsize=30))

FR_plots <- list(low_grob, high_grob, negative_same_fr, positive_same_fr, negative_different_fr, positive_different_fr)

tiff("figures/fig1.tiff", width = 6, height = 6, units = 'in', res = 300)

grid.arrange(grobs=FR_plots, ncol=2, nrow=3, left=y_grob, bottom=x_grob, align=h, heights=c(0.75,3,3))



