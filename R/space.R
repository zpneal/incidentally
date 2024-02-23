#' Examine the space of incidence matrices with fixed marginals
#'
#' @param R vector of row sums
#' @param C vector of column sums
#'
#' @return a list containing (a) a list of all matrices in the space and (b) a matrix of their perturbations
#'
#' @details This implementation *attempts* to ennumerate all matrices in the space, but a complete ennumeration is
#'    not guaranteed. The cardinality of the space is often unknown and can be quite large. Therefore, exercise
#'    caution when examining the space of matrices larger than 5x5.
#'
#' @export
#'
#' @examples
#' space(R = c(2,1,1), C = c(1,1,2))
space <- function(R, C) {

  if (sum(R) != sum(C)) {stop("Row sums must equal column sums")}

  #Ennumerate all matrices in the space
  M <- incidentally::incidence.from.vector(R,C)  #Generate one matrix
  mat <- t(as.vector(M))
  for (i in 1:(length(M)*1000)) {mat <- rbind(mat,t(as.vector(backbone::fastball(M))))}
  mat <- unique(mat)

  #Compute perturbation between each matrix in the space
  perturb <- matrix(NA, nrow(mat), nrow(mat))
  for (row in 1:nrow(perturb)) {
    for (col in 1:ncol(perturb)) {
      perturb[row,col] <- sum(mat[row,]!=mat[col,])/length(M)
    }
  }

  #Assemble matrices into a list
  matrices <- list()
  for (i in 1:nrow(mat)) {matrices[[i]] <- matrix(mat[i,], nrow = length(R), ncol = length(C))}

  #Return a list containing (a) a list of all matrices in the space and (b) a matrix of their perturbations
  return(list(matrices = matrices, perturbations = perturb))
}
