#' clean lefse analysis result for ggplot2
#'
#' @param lefse.res the first result of lefse analysis using lefse function
#'
#' @return a data frame
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' lefse.res.clean(lefse.res = "B_lefse.res")
lefse.res.clean <- function(lefse.res = "B_lefse.res"){
  # step 1 import data
  lefse.out <- read.table(lefse.res, header = FALSE, sep = "\t")
  colnames(lefse.out) <- c("taxon", "log.max.pct", "direction", "lda", "p.value")
  lefse.out <- lefse.out[!is.na(lefse.out$lda), ] # select the significant results
  lefse.out <- lefse.out[lefse.out$p.value < 0.05, ]
  # step 2 split taxon variable, only keep the family, genus and species info
  temp <- strsplit(lefse.out$taxon, split = ".f__")  |> sapply("[", 2)
  lefse.out$taxon <- temp
  lefse.out <- lefse.out[!is.na(lefse.out$taxon), ]
  lefse.out$taxon <- paste0("f_", lefse.out$taxon)
  lefse.out$family <- strsplit(lefse.out$taxon, split = ".g__")  |> sapply("[", 1)
  # step 2.2
  temp2 <- strsplit(lefse.out$taxon, split = ".g__")  |> sapply("[", 2)
  lefse.out$genus <- temp2  |> strsplit(split = ".s__") |> sapply("[", 1)
  lefse.out$genus <- paste0("g_", lefse.out$genus)
  lefse.out$genus <- gsub("g_NA", NA, lefse.out$genus)
  # step 2.3
  lefse.out$species <- temp2  |> strsplit(split = ".s__") |> sapply("[", 2)
  lefse.out$species <- gsub("_", " ", lefse.out$species)
  # step 2.3.2
  lefse.out2 <- lefse.out[!duplicated(lefse.out$lda), ]
  for (i in 1:nrow(lefse.out2)) {
    if (is.na(lefse.out2$species[i]) & is.na(lefse.out2$genus[i])) {
      lefse.out2$species[i] <- lefse.out2$family[i]
    } else if (is.na(lefse.out2$species[i]) & !is.na(lefse.out2$genus[i])) {
      lefse.out2$species[i] <- lefse.out2$genus[i]
    }
  }
  # step 4 clean data
  lefse.cleanresult <- data.frame(lefse.out2[, c(6,7,8,3,4,5)])
  lefse.cleanresult <- lefse.cleanresult[order(lefse.cleanresult$direction, lefse.cleanresult$lda), ]
  class(lefse.cleanresult) <- c("data.frame", "lefse")
  return(lefse.cleanresult)
}
