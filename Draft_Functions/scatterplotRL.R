

# Set-UP ------------------------------------------------------------------
require(RColorBrewer)
require(ggplot2)
data("mtcars")

# scatterplotRL -----------------------------------------------------------
scatterplotRL = function(){
  list(
    geom_point(),
    theme_bw(),
    theme(axis.text.x = element_text(size = 10, face = 'plain'),
          axis.text.y = element_text(size = 10, face = 'plain'),
          axis.title = element_text(size = 20, face = 'plain'),
          plot.title = element_text(size = 20, face = 'plain')),
    scale_color_gradient(low = "blue", high = "orange")
  )
}

# Testing -----------------------------------------------------------------
ggplot(mtcars, aes(x=wt, y=mpg, color = cyl)) + 
  scatterplotRL() +
  xlab('Hi Karina') +
  scale_color_gradient(low = "white", high = "red")
