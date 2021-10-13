#' Wraping long string to multiple lines, adapted from yulab.utils
#'
#' @title str_wrap
#' @param string input string to be wrapped
#' @param width the length characters of each line
#'
#' @return a wrapped characters
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' x <- c("this is a   looooooooong text", "this is a shorter text")
#' str_wrap(string = x, width = 10)
str_wrap <- function(string, width = getOption("width")) {
  result <- vapply(string, FUN = ins.enter, FUN.VALUE = character(1), width = width)
  names(result) <- NULL
  result
}


ins.enter <- function(string, width) {
  string <- gsub("\\s+", " ", string)
  words <- vector()
  i <- 1
  while(nchar(string) > width) {
    if (!grepl(" ", string)) break
    y <- gregexpr(" ", string)[[1]]
    n <- nchar(string)
    y <- c(y, n)
    idx <- which(y < width)
    # When the length of first word > width
    if (length(idx) == 0) idx <- 1
    # Split the string into two pieces
    # The length of first piece is small than width
    words[i] <- substring(string, 1, y[idx[length(idx)]] - 1)
    string <- substring(string, y[idx[length(idx)]] + 1, n)
    i <- i + 1
  }
  words[i] <- string
  paste0(words, collapse="\n")
}
