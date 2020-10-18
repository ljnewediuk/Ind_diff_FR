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

# Create individuals with increasing selection with availability
indiv_pos_eqs <- data.table()

for(i in 1:nrow(indivs_pos)){
  indivs_sub <- indivs_pos[id==i]
  betas <- indivs_sub$intercepts + indivs_sub$slopes*availabilities
  indiv_sel <- data.table(id=rep(i, length(availabilities)), type=rep('positive', length(availabilities)), availability=availabilities, beta=betas)
  indiv_pos_eqs <- rbind(indiv_pos_eqs, indiv_sel)
}

# Create individuals with decreasing selection with availability
indiv_neg_eqs <- data.table()

for(i in 1:nrow(indivs_neg)){
  indivs_sub <- indivs_neg[id==i]
  betas <- indivs_sub$intercepts + indivs_sub$slopes*availabilities
  indiv_sel <- data.table(id=rep(i, length(availabilities)), type=rep('negative', length(availabilities)), availability=availabilities, beta=betas)
  indiv_neg_eqs <- rbind(indiv_neg_eqs, indiv_sel)
}

# Create vector of mean selection at each availability
indiv_all_eqs <- rbind(indiv_neg_eqs, indiv_pos_eqs)
mean_all_eqs <- data.table()
for(i in unique(indiv_all_eqs$availability)){
  sub_row <- indiv_all_eqs[availability==i]
  means <- mean(sub_row$beta)
  means_by_avail <- data.table(availability=i, beta=means)
  mean_all_eqs <- rbind(mean_all_eqs, means_by_avail)
  if(i==max(unique(indiv_all_eqs$availability))){
    extrap_means <- data.table(availability=c(30,170), beta=rep(means,2))
  mean_all_eqs <- rbind(mean_all_eqs, extrap_means)
  }
}

mean_all_eqs <- mean_all_eqs[order(availability)]

##########################################
### PLOT INDIVIDUALS WITH DIFFERENT FR ###

for(fr_dir in c('positive', 'negative')){
  
  if(fr_dir=='positive'){
    most <- indiv_pos_eqs
    few <- indiv_neg_eqs
    anno <- 'D'
  } else {
    most <- indiv_neg_eqs
    few <- indiv_pos_eqs
    anno <- 'C'
  }

fr <- rbind(most[id %in% sample(seq(1,10,1),6)], few[id %in% sample(seq(1,10,1),2)])

fr_plot <- ggplot(data=rbind(most[id %in% sample(seq(1,10,1),6)], few[id %in% sample(seq(1,10,1),2)]), aes(x=availability, y=beta)) + 
geom_smooth(method=lm, se=FALSE, fullrange=TRUE, col='black', size=2, linetype='solid') + 
geom_smooth(data=mean_all_eqs, size=2, linetype='dotted', col='black', aes(x=availability, y=beta), se=F) +
geom_line(data=most[id %in% sample(seq(1,10,1),6)], size=1, lineend='round', aes(group=id, colour='#D63118')) + xlim(30,170) +
geom_line(data=few[id %in% sample(seq(1,10,1),2)], aes(x=availability, y=beta, group=id), colour='#FF0000', size=1, lineend='round', linetype='dashed') +
# Theme
theme(panel.background = element_rect(fill=NA), axis.text = element_blank(), axis.title = element_blank(), axis.ticks= element_blank(), legend.position='none') +
# annotate labels
annotate('text', x=55,y=350, label=anno, size=8) +
# add axis arrows
# y axis
geom_segment(aes(x=30, xend=30, y=-200, yend = 400), size=1, arrow = arrow(length = unit(0.3,"cm")), colour='#3C4489') +
# x axis
geom_segment(aes(x=30, xend=170, y=-200, yend = -200), size=1, arrow = arrow(length = unit(0.3,"cm")), colour='#3C4489') 

assign(paste(fr_dir, 'different_fr', sep='_'), fr_plot)

}

#####################################
### PLOT INDIVIDUALS WITH SAME FR ###

for(fr_dir in c('positive', 'negative')){
  
  if(fr_dir=='positive'){
    most <- indiv_pos_eqs
    anno <- 'B'
  } else {
    most <- indiv_neg_eqs
    anno <- 'A'
  }
  
  fr_plot <- ggplot(most[id %in% sample(seq(1,10,1),8)], aes(x=availability, y=beta)) + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE, col='black', size=2.5, linetype='solid') + 
    geom_line(size=1, lineend='round', aes(group=id, colour='#D63118')) + xlim(30,170) +
    # Theme
    theme(panel.background = element_rect(fill=NA), axis.text = element_blank(), axis.title = element_blank(), axis.ticks= element_blank(), legend.position='none') +
    # annotate labels
    annotate('text', x=55,y=350, label=anno, size=8) +
    # add axis arrows
    # y axis
    geom_segment(aes(x=30, xend=30, y=-200, yend = 400), size=1, arrow = arrow(length = unit(0.3,"cm")), colour='#3C4489') +
    # x axis
    geom_segment(aes(x=30, xend=170, y=-200, yend = -200), size=1, arrow = arrow(length = unit(0.3,"cm")), colour='#3C4489') 
  
  assign(paste(fr_dir, 'same_fr', sep='_'), fr_plot)
  
}



# Plot layout
layout_plots <- rbind(c(1,2), c(3,4), c(5,6))

low_grob <- textGrob(expression('High h'[1]), gp=gpar(col='#000000', fontsize=20))

high_grob <- textGrob(expression('Low h'[1]), gp=gpar(col='#000000', fontsize=20))

y_grob <- textGrob(expression(beta ~ h[2]), gp=gpar(fontface="bold", col="#000000", fontsize=20), rot=90)

x_grob <- textGrob(expression('Availability h'[2]), gp=gpar(fontface="bold", col="#000000", fontsize=20))

FR_plots <- list(low_grob, high_grob, negative_same_fr, positive_same_fr, negative_different_fr, positive_different_fr)

# tiff("figures/fig1.tiff", width = 6, height = 6, units = 'in', res = 300)

grid.arrange(grobs=FR_plots, ncol=2, nrow=3, left=y_grob, bottom=x_grob, align=h, heights=c(0.75,3,3))



