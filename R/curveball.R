#' Randomize a binary matrix using the curveball algorithm
#'
#' `curveball` randomizes a binary matrix, preserving the row and column sums
#'
#' @param M a binary matrix
#' @param trades integer: number of trades; the default is 5 * nrow(M) (approx. mixing time)
#'
#' @return
#' A random binary matrix with same row sums and column sums as M
#'
#' @details
#' Strona et al. (2014) provided an initial implementation of the Curveball algorithm in R. `curveball()` is a modified R
#'    implementation that is slightly more efficient. For an even more efficient algorithm, see `backbone::fastball()`.
#'
#' @references {Strona, Giovanni, Domenico Nappo, Francesco Boccacci, Simone Fattorini, and Jesus San-Miguel-Ayanz. 2014. A Fast and Unbiased Procedure to Randomize Ecological Binary Matrices with Fixed Row and Column Totals. *Nature Communications, 5*, 4114. \doi{10.1038/ncomms5114}}
#' @references {Godard, Karl and Neal, Zachary P. 2022. fastball: A fast algorithm to sample bipartite graphs with fixed degree sequences. \href{https://arxiv.org/abs/2112.04017}{*arXiv:2112.04017*}}
#'
#' @export
#' @examples
#' M <- incidence.from.probability(5,5,.5)  #A matrix
#' Mrand <- curveball(M)  #Random matrix with same row/col sums
#' all.equal(rowSums(M), rowSums(curveball(M)))
#' all.equal(colSums(M), colSums(curveball(M)))
curveball <- function(M, trades = 5*nrow(M)) {

  #Parameter check
  if (!methods::is(M, "matrix")) {stop("Input must be a matrix")}
  if (any(M%%1!=0) | any(M<0)) {stop("Input matrix must contain only 0s and 1s")}
  if (!is.numeric(trades)) {stop("trades must be numeric")}
  if (trades%%1!=0 | trades < 1) {stop("trades must be a positive integer")}

  #Curveball, adapted from Strona et al. (2014)
  RC=dim(M)
  R=RC[1]
  C=RC[2]
  M <- lapply(asplit(M == 1, 1), which)
  l_M=length(M)
  for (rep in 1:trades){
    AB=sample(1:l_M,2)
    a=M[[AB[1]]]
    b=M[[AB[2]]]
    ab=intersect(a,b)
    l_ab=length(ab)
    l_a=length(a)
    l_b=length(b)
    if ((l_ab %in% c(l_a,l_b))==F){
      tot=setdiff(c(a,b),ab)
      l_tot=length(tot)
      tot=sample(tot, l_tot, replace = FALSE, prob = NULL)
      L=l_a-l_ab
      M[[AB[1]]] = c(ab,tot[1:L])
      M[[AB[2]]] = c(ab,tot[(L+1):l_tot])}
    }
  rm=matrix(0,R,C)
  for (row in 1:R){rm[row,M[[row]]]=1}
  rm
}
