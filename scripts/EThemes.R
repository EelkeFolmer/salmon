margin <- ggplot2::margin

ETheme <- function (base_size = 12, base_family = "") {
  theme(axis.title.x =      element_blank(),
        axis.title.y =      element_blank(),
        axis.text.x =       element_blank(),
        axis.text.y =       element_blank(),
        axis.line =         element_line(colour = "black", size = 0.5),
        axis.ticks =        element_blank(), #element_line(colour = "black", size = 0.5),
        #axis.ticks.length = element_blank(), #unit(0.15, "cm"),
        
        plot.background = element_rect(colour = NA, fill = "transparent"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_line(colour = "black", size = 0.2),
        plot.title = element_text(family = base_family, size = base_size * 1, hjust=0.5) )
}

