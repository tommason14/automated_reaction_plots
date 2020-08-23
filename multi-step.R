library(ggplot2)
library(latex2exp)

##########################################################################################
#  Change the energies, and make sure the levels on line 29 match the species on line 8  #
##########################################################################################

df <- data.frame(
  species = c('Reactants', 'TS1', 'Int1', 'TS2', 'Int2'),
  energies = c(0,78.6889, 14.706,50.5968,-20.8656))

#################################
#  Plot is automated from here  #
#################################

theming <- theme_light() +
  theme(panel.grid = element_blank(),
        panel.spacing = unit(0.6, 'cm'),
        text = element_text(size = 14),
        axis.title.y = element_text(margin = margin(c(0,0.4,0,0), unit='cm')),
        axis.title.x = element_text(margin = margin(c(0.4,0,0,0), unit='cm')))

no_x <- theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())

plot_graph <- function(df){

    offset = 0.2 
    df$species <- factor(df$species, levels = c('Reactants', 'TS1', 'Int1', 'TS2', 'Int2'))

    p = ggplot(df) +
      aes(x = as.numeric(species) - offset,
          xend = as.numeric(species) + offset,
          y = energies,
          yend = energies) +
      geom_segment(size=1)
    
    # add connecting lines along rxn coord
    for (num in 2:nrow(df)-1){
      line = paste("geom_segment(aes(x=",num + offset,", xend = ",num + 1 - offset,",",
                               "y = ",df$energies[num],", yend =", df$energies[num+1],"),
                               linetype = 'dashed')")

      p <- p + eval(parse(text = line))
    }

    for (num in 2:nrow(df)-1){
      # from energy to middle of gap
      if ((df$energies[num + 1] - df$energies[num]) > 0){
        gap = df$energies[num + 1] - df$energies[num]
        line = paste("geom_segment(aes(x =",num ,", xend = ",num ,", y = ",df$energies[num] + 1,", yend = ", df$energies[num] + gap/2,
               "), color='grey70', arrow = arrow(length = unit(0.1,'cm'), ends='first'))")
        p <- p + eval(parse(text = line))  
      } else{
        gap = df$energies[num + 1] - df$energies[num]
        line = paste("geom_segment(aes(x =",num ,", xend = ",num ,", y = ",df$energies[num] -1,", yend = ", df$energies[num + 1] - gap/2,
                     "), color='grey70', arrow = arrow(length = unit(0.1,'cm'), ends='first'))")
        p <- p + eval(parse(text = line))  
      }
      # mid of gap to next energy
      if ((df$energies[num + 1] - df$energies[num]) > 0){
        gap = df$energies[num + 1] - df$energies[num]
        line = paste("geom_segment(aes(x =",num ,", xend = ",num ,", y = ",df$energies[num] + gap/2 + 5,", yend = ", df$energies[num + 1] -1,
                     "), color='grey70', arrow = arrow(length = unit(0.1,'cm'), ends='last'))")
        p <- p + eval(parse(text = line))  
      } else{
        gap = df$energies[num + 1] - df$energies[num]
        line = paste("geom_segment(aes(x =",num ,", xend = ",num ,", y = ",df$energies[num] -1 + gap/2 - 5,", yend = ", df$energies[num + 1] + 1,
                     "), color='grey70', arrow = arrow(length = unit(0.1,'cm'), ends='last'))")
        p <- p + eval(parse(text = line))  
      }
      # energy barriers
      if ((df$energies[num + 1] - df$energies[num]) > 0){
        gap = df$energies[num + 1] - df$energies[num]
        line = paste(
          "annotate('text',",
                   "x =", num,", y = ",df$energies[num] + gap/2 + 3,", color = 'grey70',",
                   "label = TeX(paste('$\\\\Delta G_{",num,"}$ = ",round(abs(gap), 1)," kJ mol$^{-1}$')))"
        )
        p <- p + eval(parse(text = line))  
      } else{
        line = paste(
          "annotate('text',",
          "x =", num,", y = ",df$energies[num] + gap/2 - 3,", color = 'grey70',",
          "label = TeX(paste('$\\\\Delta G_{",num,"}$ = ",round(abs(gap), 1)," kJ mol$^{-1}$')))"
        )
        p <- p + eval(parse(text = line))  
      }
    }
      
      p + labs(x = 'Reaction Coordinate', y = TeX('Rel. $\\Delta G$ (kJ mol$^{-1}$)')) +
      theming + no_x
}

plot_graph(df) + ggsave('multi-step.png', dpi=300, width=12, height=8)
