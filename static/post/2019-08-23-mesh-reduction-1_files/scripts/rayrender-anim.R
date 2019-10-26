
# Generate the watch data

# Generate the letter objects

# Function to generate background image

# Function to generate glass bars, mark which one to 

make_background <- function(text) {
  dpi <- 72
  width <- 800
  height <- width/3*2

  p <- ggplot(subset(code.txt, .id==1)) +
    geom_text(
      aes(x=0, y=y.raw, label=code),
      hjust=0, size=12, family='mono'
    ) + 
    labs(y=NULL, x=NULL) +
    theme(
      axis.text.x=element_blank(), axis.text.y=element_blank(),
      axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
      panel.grid=element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      plot.margin=unit(c(0, 0, height/3/dpi, 0), "inches"),
    ) +
    coord_cartesian(ylim=c(0,12), xlim=c(0, 60)) +
    NULL

  ggsave(
    filename=sprintf('~/Downloads/ray-anim/img-%04d.png', i),
    plot=p,
    width=width/dpi, height=height/dpi, units='in', device='png',
    dpi=dpi, bg='grey50'
  )

}
