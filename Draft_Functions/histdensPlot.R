require(ggplot2)
require(RColorBrewer)

#autohist ----------------------------------------
autohist<- function(data, mapping, density=FALSE, mean=FALSE){
  alpha <- ifelse(is.null(mapping$fill), 1, 0.6)
  bw <- 2* IQR(data[,toString(mapping$x)]) / (length(data[,toString(mapping$x)])^(1/3))
  
  if(density){ 
    p <-  ggplot(data, mapping)+ 
      geom_histogram(position="identity", aes_string(y="..density.."), alpha=alpha, binwidth = bw)+
      geom_density(alpha=0)
  }else{
    p <- ggplot(data, mapping)+ 
      geom_histogram(position="identity", alpha=alpha, binwidth = bw)
  }
  
  if(mean){
    if(is.null(mapping$colour) && is.null(mapping$fill)){
      p <- p+geom_vline(aex(xintercept=mean(datap[,toString(mapping$x)])), color="red", linetype="dashed")+
        theme_classic()
    }
    else{
      if(is.null(mapping$colour)) mapping$colour <- mapping$fill
      mu <- stats::aggregate(data[,toString(mapping$x)], by=list(grp=data[,toString(mapping$colour)]), mean)
      names(mu) <- c("group", "mean")
      p <- p+geom_vline(data=mu, aes(xintercept=mean, color=group), linetype="dashed")+
        theme_classic()
    }
  }
  p
}

#autodensity ------------------------------------------------------------------
autodensity <- function(data, mapping, jitter=FALSE, mean=FALSE){
  alpha <- ifelse(is.null(mapping$fill), 1, 0.6)
  p <- ggplot(data, mapping) + geom_density(alpha=alpha) 
  if(jitter){
    p <- p+
      geom_rug(aes(y=0), position = position_jitter(height = 0))
      # geom_jitter(aes(y=-0.001, shape=1), size=3, position = position_jitter(height = 0))+
      # scale_shape_identity()
  }
  if(mean){
    if(is.null(mapping$colour) && is.null(mapping$fill)){
      p <- p+geom_vline(aex(xintercept=mean(datap[,toString(mapping$x)])), color="red", linetype="dashed")
    }
    else{
      if(is.null(mapping$colour)) mapping$colour <- mapping$fill
      mu <- stats::aggregate(data[,toString(mapping$x)], by=list(grp=data[,toString(mapping$colour)]), mean)
      names(mu) <- c("group", "mean")
      p <- p+geom_vline(data=mu, aes(xintercept=mean, color=group), linetype="dashed")
    }
  }
  p
}

# Testing---------------------------------------------
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
)
data <- mtcars

autohist(df, aes(weight, fill=sex, color=sex), F, T)
autodensity(df, aes(weight, fill=sex, color=sex), T, T)
autodensity(df, aes(weight, color=sex), F, T)




