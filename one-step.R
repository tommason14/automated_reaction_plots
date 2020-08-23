library(ggplot2)
library(latex2exp)

##############################
#  Change the energies only  #
##############################

df <- data.frame(
  species = c('Reactants', 'TS', 'Product'),
  energies = c(0, 114.7863, 8.0996))

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
    df$species <- factor(df$species, levels = c('Reactants', 'TS', 'Product'))

    # factor levels dictate row order
    react = df$energies[1]
    ts = df$energies[2]
    prod = df$energies[3]
    
    ggplot(df) +
      aes(x = as.numeric(species) - offset,
          xend = as.numeric(species) + offset,
          y = energies,
          yend = energies) +
      geom_segment(size=1) +

      # barrier energy
      geom_segment(aes(x = 1, xend = 1, y = 0.5, yend = ts/2),
                   color='grey70') +
      geom_segment(aes(x=1,xend=1, y= (ts/2) + 5, yend = ts),
                   color = 'grey70',
                   arrow = arrow(length = unit(0.3, 'cm'),
                                 type = 'closed'))+
      geom_segment(aes(x = 0.8, xend = 1.8, y = ts, yend=ts),
                   linetype = 'dashed',
                   color = 'grey70')+
      annotate('text',
               x = 1.0, y = (ts/2)+3, color = 'grey70', # middle of gap
               label = TeX(paste('$\\Delta G^{‡}$ =', round(ts, 1), 'kJ mol$^{-1}$')))+

      # reactants -> TS along rxn coordinate
      geom_segment(aes(x=1.2, xend=1.8, y=0, yend=ts),
                   linetype='dashed')+

      # ts -> prod along reaction coordinate
      geom_segment(aes(x=2.2,xend=2.8,y=ts,yend=prod),
                   linetype='dashed')+
      annotate('text',
               x = 3, y = prod-2, color = 'grey70',
               label = TeX(paste('$\\Delta G_{r}$ =', round(prod, 1), 'kJ mol$^{-1}$')))+
      labs(x = 'Reaction Coordinate', y = TeX('Rel. $\\Delta G$ (kJ mol$^{-1}$)')) +
      theming + no_x
}

plot_graph(df) + ggsave('one-step.png', dpi=300, width=8, height=7)
