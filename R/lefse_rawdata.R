#' Prepare data for downstream Lefse analysis using asv.table, taxa info and the grouping metadata
#'
#' @param asv.path the path of ASVs_table obtained using usearch
#' @param taxa.path the path of ASV_taxa info obtained suing usearch
#' @param group.path the grouping metadata
#' @param lefse the logical value for which data to save
#'
#' @return a txt text file for lefse analysis
#' @export
#'
#' @author ZHonghuiGai
#' @examples
#' lefse_rawdata(asv.path = "ASVs_norm.txt", taxa.path = "ASV_tax2.txt", group.path = "group.txt", lefse = TRUE)
lefse_rawdata <- function(asv.path, taxa.path, group.path, lefse = TRUE){
  # step 1 import the ASV table
  ASV <- read.delim(asv.path)
  colnames(ASV)[1] <- "ASV"
  rownames(ASV) <- ASV[, 1]
  # step 2 import the taxa table
  taxa <- read.delim(taxa.path, header = FALSE)
  colnames(taxa) <- c("ASV", "class")
  rownames(taxa) <- taxa[, 1]
  taxa$class <- gsub(";", "|", taxa$class)
  # step 3 merge data
  stopifnot(nrow(ASV) == nrow(taxa))
  stopifnot(all(rownames(ASV) %in% rownames(taxa)))
  ASV <- ASV[rownames(taxa), ]
  if (all(rownames(ASV) ==rownames(taxa))) {
    ASV.taxa <- data.frame(class = taxa$class, ASV[, -1])
    rownames(ASV.taxa) <- NULL
  }
  # step 4 import the grouping metadata
  grp <- read.delim(group.path, header = FALSE)
  rownames(grp) <- grp[, 1]
  grp <- grp[colnames(ASV.taxa)[-1], ]
  stopifnot(length(colnames(ASV.taxa)[-1]) == length(grp[, 1]))
  stopifnot(colnames(ASV.taxa)[-1] %in% grp[, 1])
  rownames(grp) <- NULL
  # step 5 substitute the first row of ASV.taxa with the grouping information
  ASV.taxa.lefse <- rbind(colnames(ASV.taxa), ASV.taxa)
  colnames(ASV.taxa.lefse) <- NULL
  ASV.taxa.lefse[1, ] <- c("class", grp[, 2])
  if (lefse) {
    write.table(ASV.taxa.lefse, "lefse.txt", quote = FALSE,
                sep = "\t", col.names = FALSE, row.names = FALSE)
  } else {
    write.table(ASV.taxa, "lefse.txt", quote = FALSE,
                sep = "\t", col.names = FALSE, row.names = FALSE)
  }
}
