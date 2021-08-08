#' lefse analysis by using the data from lefse.rawdata function
#'
#' @param input the input data for lefse analysis
#'
#' @return lefse analysi results and plot
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' lefse(input = "lefse.txt")
lefse <- function(input = "lefse.txt"){
  # lefse step 1
  lfs.com <- paste("~/miniconda3/bin/lefse-format_input.py", input, "A_lefse.in -c 1 -o 1000000")
  system(lfs.com)
  cat("Format input data for lefse analysis finished! Now we have the A_lefse_in for next analysis!")
  # lefse step 2
  system("~/miniconda3/bin/run_lefse.py A_lefse.in B_lefse.res -l 2.0")
  cat("Wrote lefse.res finished! The B_lefse.res can also be used for ploting by ggplot2!")
  # lefse step 3
  system("~/miniconda3/bin/lefse-plot_res.py B_lefse.res C_lefse.lda.pdf --format pdf --dpi 150 --width 16")
  cat("Plot!")
}
