# Set-UP ------------------------------------------------------------------
require(RColorBrewer)
require(ggplot2)
data("esoph")

# tileplotRL -----------------------------------------------------------
tileplotRL = function(){
  list(
    geom_tile(color="grey"),
    theme_bw(),
    theme(axis.text.x = element_text(size = 10, face = 'plain', angle = 90, hjust = 0),
          axis.text.y = element_text(size = 10, face = 'plain'),
          axis.title = element_text(size = 20, face = 'plain'),
          plot.title = element_text(size = 20, face = 'plain')),
          scale_fill_gradient(low = "blue", high = "orange")
  )
}

# Testing -----------------------------------------------------------------
ggplot(esoph, aes(x=agegp, y=alcgp, fill = ncases)) + 
  tileplotRL()
