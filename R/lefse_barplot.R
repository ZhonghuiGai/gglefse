#' Draw a bar plot using the lefse analysis results
#'
#' @param data a data frame containing markers, grouping and LDA_value information
#' @param size the width of bar
#' @param col the col of bars, default value if NULL
#' @param tpye the color type of gfun::scale_color_manual2
#'
#' @return a ggplot2 bar plot
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' library(gfun)
#' lefse.genus <- data.in("g_LDA.xlsx", rowname = F)
#' colnames(lefse.genus)[2] <- "group"
#' lefse.genus$markers <- factor(lefse.genus$markers, levels = lefse.genus$markers)
#' lefse.genus$group <- factor(lefse.genus$group, levels = c("HC", "A", "B"), labels = c("HC", "No-NAs", "TDF"))
#' lefse_barplot(data = lefse.genus) +  ggtheme::theme_size()
lefse_barplot <- function(data, size = 0.5, col = NULL, tpye = "npg"){
  # step 1 check the input data
  cat("This function uses markers, group, and LDA_value variables!\n")
  stopifnot("markers" %in% colnames(data))
  stopifnot("group" %in% colnames(data))
  stopifnot("LDA_value" %in% colnames(data))
  library(ggplot2)
  # step 2 draw the plot
  p <- ggplot(data = data, aes(x = markers, y = LDA_value, fill = group)) +
    geom_col(colour = "white", size = 0.5) + coord_flip() + ggtheme::theme_pub() +
    scale_y_continuous(expand = c(0, 0)) +
    xlab(NULL) +
    ylab("LDA SCORE (log 10)") +
    theme(axis.text.y = element_text(face = "bold.italic"),
          legend.text = element_text(face = "bold"),
          axis.ticks.y = element_blank()) +
    geom_hline(yintercept = c(2, 3), color = "gray90", size = 0.5)
  # step 3 change the colors of bar
  if (is.null(col)) {
    p <- p + gfun::scale_color_manual2(type = type)
  } else {
    p <- p + scale_fill_manual(values = col)
  }
    return(p)
}
