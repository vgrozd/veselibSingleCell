#' Counting of cells that express a gene/feature
#'
#' Takes a count matirx and returns feature metadata
#'
#'
#'
#'@param counts A count matrix; more options TBA
#'@export

nCells <- function(counts){
  return(as.numeric(rowSums(counts>0)))
}
