#' Draw a bar plot using the lefse analysis results
#'
#' @param data a data frame containing markers, grouping and LDA_value information
#' @param size the width of bar
#' @param col the col of bars, default value if NULL
#'
#' @return ggplot2 bar plot
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' library(gfun)
#' lefse.genus <- data.in("g_LDA.xlsx", rowname = F)
#' colnames(lefse.genus)[2] <- "group"
#' lefse.genus <- lefse.genus[8:21, ]
#' lefse.genus$group <- factor(lefse.genus$group, levels = c("A", "B"), labels = c("No-NAs", "TDF"))
#' lefse_barplot_2grp(data = lefse.genus) +  ggtheme::theme_size()
lefse_barplot_2grp <- function(data, size = 0.5, col = NULL){
  # step 1 check the input data
  cat("This function uses markers, group, and LDA_value variables!\n")
  message("This function only used two groups!\n")
  stopifnot("markers" %in% colnames(data))
  stopifnot("group" %in% colnames(data))
  stopifnot("LDA_value" %in% colnames(data))
  stopifnot(length(unique(data$group)) == 2)
  # step 2 transform the LDA_value to negtive
  data$LDA_value <- with(data,
                         ifelse(grepl(unique(group)[1], group),
                                LDA_value, -LDA_value))
  # step 3 sort the data
  data <- data[order(data$LDA_value), ]
  data$markers <- factor(data$markers, levels = data$markers)
  # step 4 make the plot
  p <- ggplot(data = data, aes(x = markers, y = LDA_value, fill = group)) +
    geom_col(color = "white", size = size) +
    coord_flip() +
    xlab(NULL) + ylab("LDA SCORE (log 10)")  +
    ggtheme::theme_pub() +
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()) +
    geom_hline(yintercept = c(-3, -2, 2, 3), color = "gray85") +
    geom_text(aes(y = ifelse(LDA_value > 0, -0.1, 0.1), label = markers),
              fontface = 4, size = 4.5,
              hjust = ifelse(data$LDA_value > 0, 1, 0))
  # step 5 change the colors of bar
  if (is.null(col)) {
    p <- p + ggsci::scale_fill_aaas()
  } else {
    p <- p + scale_fill_manual(values = col)
  }
  return(p)
}








