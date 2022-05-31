#' Randomize an incidence matrix or bipartite graph using the curveball algorithm
#'
#' `curveball` randomizes an incidence matrix or bipartite graph, preserving the row and column sums
#'
#' @param M a binary matrix of class `matrix` or `Matrix`, or a bipartite graph of class {\link{igraph}}.
#' @param trades integer: number of trades; the default is 5 * nrow(M) (approx. mixing time)
#' @param class string: Return object as `matrix`, `Matrix`, `igraph`. If `NULL`, object is returned in the same class as `M`.
#'
#' @return
#' An incidence matrix of class `matrix` or `Matrix`, or a bipartite graph of class {\link{igraph}}.
#'
#' @details
#' Strona et al. (2014) provided an initial implementation of the Curveball algorithm in R. `curveball()` is a modified R
#'    implementation that is slightly more efficient. For an even more efficient algorithm, see `backbone::fastball()`.
#'
#' @references {Strona, Giovanni, Domenico Nappo, Francesco Boccacci, Simone Fattorini, and Jesus San-Miguel-Ayanz. 2014. A Fast and Unbiased Procedure to Randomize Ecological Binary Matrices with Fixed Row and Column Totals. *Nature Communications, 5*, 4114. \doi{10.1038/ncomms5114}}
#' @references {Godard, Karl and Neal, Zachary P. 2022. fastball: A fast algorithm to sample bipartite graphs with fixed degree sequences. \href{https://arxiv.org/abs/2112.04017}{*arXiv:2112.04017*}}
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *OSF Preprints* \doi{10.31219/osf.io/ectms}}
#'
#' @export
#' @examples
#' M <- incidence.from.probability(5,5,.5)  #A matrix
#' Mrand <- curveball(M)  #Random matrix with same row/col sums
#' all.equal(rowSums(M), rowSums(curveball(M)))
#' all.equal(colSums(M), colSums(curveball(M)))
curveball <- function(M, trades = 5*nrow(M), class = NULL) {

  #Parameter check
  if (is.null(class) & methods::is(M, "igraph")) {class <- "igraph"}
  if (is.null(class) & methods::is(M, "matrix")) {class <- "matrix"}
  if (is.null(class) & methods::is(M, "Matrix")) {class <- "Matrix"}
  if (!(class %in% c("matrix", "Matrix", "igraph"))) {stop("class must be one if c(\"matrix\", \"Matrix\", \"igraph\")")}
  if (!methods::is(M, "matrix") & !methods::is(M, "Matrix") & !methods::is(M, "igraph")) {stop("Input must be a matrix, Matrix, or igraph")}
  if (methods::is(M,"Matrix")){M <- as.matrix(M)}
  if (methods::is(M,"igraph")){M <- igraph::as_incidence_matrix(M)}
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

  if (class == "igraph") {rm <- igraph::graph_from_incidence_matrix(rm)}
  if (class == "Matrix"){rm <- Matrix::Matrix(rm)}
  return(rm)
}
