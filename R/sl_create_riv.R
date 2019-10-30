#' Create random index vectors
#'
#' @param dim integer, number of dimensions (vector length)
#' @param inds integer, number of rows
#' @param sparsity integer, number of elements in vector
#'
#' @return matrix, each row is an item with dim dimensions
#' @export
#'
#' @examplesÎ
sl_create_riv <- function(dim,inds,sparsity){
  base_vector <- c(rep(0,(dim-sparsity)),rep(1,(sparsity/2)),rep(-1,(sparsity/2)))
  temp_matrix <- matrix(0,ncol=dim,nrow=inds)
  for(i in 1:inds){
    temp_matrix[i,] <- sample(base_vector)
  }
  return(temp_matrix)
}