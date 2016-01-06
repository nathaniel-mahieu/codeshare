theme_nate <- function(base_size = 16)
{
  theme_grey(base_size = base_size) %+replace%
    theme(
      strip.background = element_rect(colour="grey95"), 
      strip.text = element_text(base_size*0.9, colour="grey40", face="plain"),
      
      panel.background = element_blank(),
      panel.grid.major = element_line(colour="grey95"),
      panel.grid.minor = element_line(colour="grey99"),
      plot.title = element_text(size = rel(1.2)),
      
      axis.title.y = element_text(vjust=1, angle=90),
      axis.title = element_text(colour="grey40"),
      axis.line = element_line(colour="grey95"),
      axis.ticks = element_line(colour="grey95"),
      
      legend.position="top",
      legend.key = element_blank(),
      legend.text = element_text(colour="grey40"),
      legend.title  = element_text(colour="grey40", face="bold")
    )
}

theme_set(theme_nate())