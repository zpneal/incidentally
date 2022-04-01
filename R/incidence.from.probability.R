#' Generates an incidence matrix with a given cell-filling probability
#'
#' `incidence.from.probability` generates a random incidence matrix in which each cell is filled
#'    with a 1 with a given probability.
#'
#' @param R integer: number of rows
#' @param C integer: number of columns
#' @param P numeric: probability that a cell contains a 1; if P = 0 a probability will be chosen randomly
#' @param constrain boolean: ensure that no rows or columns sum to 0 (i.e., contain all 0s) or to 1 (i.e., contain all 1s)
#' @param class string: the class of the returned backbone graph, one of c("matrix", igraph").
#'
#' @return
#' An incidence matrix of class `matrix` or a bipartite graph of class {\link{igraph}}.
#'
#' @export
#'
#' @examples
#' I <- incidence.from.probability(R = 10, C = 10)
#' I <- incidence.from.probability(R = 10, C = 10, P = .5)
#' I <- incidence.from.probability(R = 10, C = 10, P = .5, class = "igraph")
incidence.from.probability <- function(R, C, P=0, constrain = TRUE, class="matrix") {

  #Parameter check
  if (!is.numeric(R) | !is.numeric(C) | !is.numeric(P)) {stop("R, C, and P must be numeric")}
  if (R<0 | C<0 | R%%1!=0 | C%%1!=0) {stop("R and C must be positive integers")}
  if (P<0 | P>1) {stop("P must be between 0 and 1")}

  #Check or pick probability
  if (constrain) {
    minP <- max(R,C) / (R*C)  #Minimum probability if marginal constraints are imposed
    maxP <- ((R*C) - max(R,C)) / (R*C)  #Maximum probability
    if (P == 0) {P <- stats::runif(1, min = minP, max = maxP)}  #If unspecified, pick a probability
    if (P < minP | P > maxP) {stop("P is outside the allowed range for this size bipartite network")}
  } else {if (P == 0) {P <- stats::runif(1)}}

  #Create an incidence matrix
  I <- matrix(stats::rbinom(R*C,1,P),R,C)

  #Repeat until constraints are satisfied
  if (constrain) {
    while (min(rowSums(I))==0 | max(rowSums(I))==C | min(colSums(I))==0 | max(colSums(I))==R ) {I <- matrix(stats::rbinom(R*C,1,P),R,C)}
  }

  #Convert to desired class and return
  if (class == "igraph"){I <- igraph::graph_from_incidence_matrix(I)}
  return(I)
}
