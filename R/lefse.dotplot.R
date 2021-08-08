#' dot plot using data from lefse.res.clean function based on ggplot2
#'
#' @param data the input data from lefse.res.clean func
#' @param p.size the size of point
#' @param col the colour of point
#' @param legend.position the position of legend
#' @param axis.text.color the color of axis test
#' @param ... other parameters
#'
#' @author ZhonghuiGai
#' @return a ggplot obj
#' @export
#'
#' @examples
lefse.dotplot <- function(data, p.size = 3){
  stopifnot(inherits(data, "lefse"))
  data$species = factor(data$species, levels=data$species)
  name <- unique(data$direction)
  name <- as.character(name)
  num1 <- sum(data$direction== name[1])
  num2 <- sum(data$direction == name[2])
  require(ggplot2)
  p <- ggplot2::ggplot(data = data, aes(x = species, y = round(lda, digits = 1), color = direction))+
    geom_point(aes(color = direction), size = p.size)
  p <- p + coord_flip()+
    theme(panel.grid = element_line(color = 'gray80', size = 0.1),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          panel.background = element_rect(color = "white", fill = 'transparent'),
          plot.title = element_text(size=14, face="bold"),
          axis.text.x = element_text(size = 14, face = "bold", color = "black"),
          axis.text.y = element_text(size = 13, face = "bold.italic", color = "black"),
          axis.title.y = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_text(size = 12, face = "bold", color = "white"),
          axis.title.x = element_text(size = 14, face = "bold", angle = 0, hjust = 0.5),
          legend.background = element_blank())+
    ylab("LDA SCORE (log10)") + xlab(NULL)
  if(length(name) == 2){
    p <- p +
      geom_vline(xintercept = num1 + 0.5, linetype = 2, color = "gray60")
  }
  if(length(name) == 3){
    p <- p +
      geom_vline(xintercept = num1 + 0.5, linetype = 2, color = "gray60")+
      geom_vline(xintercept = num1 + num2 + 0.5, linetype = 2, color = "gray60")
  }
  if(length(name) == 4){
    num3 <- sum(data$direction == name[3])
    p <- p +
      geom_vline(xintercept = num1 + 0.5, linetype = 2, color = "gray60")+
      geom_vline(xintercept = num1 + num2 + 0.5, linetype = 2, color = "gray60")+
      geom_vline(xintercept = num1 + num2 + num3 + 0.5, linetype = 2, color = "gray60")
  }
  if(FALSE){
    if(length(name) == 2){
      color <- c(rep(col[name[1]], num1), rep(col[name[2]], num2))
    }else if(length(name) == 3){
      num3 <- sum(data$direction == name[3])
      color <- c(rep(col[name[1]], num1), rep(col[name[2]], num2), rep(col[name[3]], num3))
    }else if(length(name) == 4){
      num3 <- sum(data$direction == name[3])
      num4 <- sum(data$direction == name[4])
      color <- c(rep(col[name[1]], num1), rep(col[name[2]], num2),
                 rep(col[name[3]], num3), rep(col[name[4]], num4))
    }
    p <- p +
      theme(axis.text.y = element_text(color = color))
  }
  return(p)
}
