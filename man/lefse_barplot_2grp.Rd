% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lefse.barplot_2grp.R
\name{lefse_barplot_2grp}
\alias{lefse_barplot_2grp}
\title{Draw a bar plot using the lefse analysis results}
\usage{
lefse_barplot_2grp(data, size = 0.5, col = NULL)
}
\arguments{
\item{data}{a data frame containing markers, grouping and LDA_value information}

\item{size}{the width of bar}

\item{col}{the col of bars, default value if NULL}
}
\value{
ggplot2 bar plot
}
\description{
Draw a bar plot using the lefse analysis results
}
\examples{
library(gfun)
lefse.genus <- data.in("g_LDA.xlsx", rowname = F)
colnames(lefse.genus)[2] <- "group"
lefse.genus <- lefse.genus[8:21, ]
lefse.genus$group <- factor(lefse.genus$group, levels = c("A", "B"), labels = c("No-NAs", "TDF"))
lefse_barplot_2grp(data = lefse.genus) +  ggtheme::theme_size()
}
\author{
Zhonghui Gai
}
