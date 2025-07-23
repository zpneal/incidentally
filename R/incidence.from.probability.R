#' Generates an incidence matrix with a given cell-filling probability
#'
#' `incidence.from.probability` generates a random incidence matrix in which each cell is filled
#'    with a 1 with a given probability.
#'
#' @param R integer: number of rows
#' @param C integer: number of columns
#' @param P numeric: probability that a cell contains a 1; if P = 0 a probability will be chosen randomly
#' @param constrain boolean: ensure that no rows or columns sum to 0 (i.e., contain all 0s) or to 1 (i.e., contain all 1s)
#' @param class string: the class of the returned backbone graph, one of c("matrix", "Matrix", "igraph").
#' @param narrative boolean: TRUE if suggested text & citations should be displayed.
#'
#' @return
#' An incidence matrix of class `matrix` or `Matrix`, or a bipartite graph of class \link[igraph]{igraph}.
#'
#' @references {Neal, Z. P., Domagalski, R., and Sagan, B. 2021. Comparing alternatives to the fixed degree sequence model for extracting the backbone of bipartite projections. *Scientific Reports, 11*, 23929. \doi{10.1038/s41598-021-03238-3}}
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *CRAN* \doi{10.32614/CRAN.package.incidentally}}
#'
#' @export
#'
#' @examples
#' I <- incidence.from.probability(R = 10, C = 10)
#' I <- incidence.from.probability(R = 10, C = 10, P = .5)
#' I <- incidence.from.probability(R = 10, C = 10, P = .5,
#'      class = "igraph", narrative = TRUE)
incidence.from.probability <- function(R, C, P=0, constrain = TRUE, class="matrix", narrative = FALSE) {

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
  if (class == "Matrix"){I <- Matrix::Matrix(I)}

  #Display narrative if requested
  if (narrative) {
    version <- utils::packageVersion("incidentally")
    if (class == "igraph") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a random bipartite graph with ", R, " agents and ", C, " artifacts, where there was a ", round(P,2), " probability that a given agent was connected to a given artifact.")}
    if (class != "igraph") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a random incidence matrix with ", R, " rows and ", C, " columns, where there was a ", round(P,2), " probability that the ith row and kth column contained a 1.")}
    message("")
    message("=== Suggested manuscript text and citations ===")
    message(text)
    message("")
    message("Neal, Z. P. (2022). incidentally: An R package to generate incidence matrices and bipartite graphs. OSF Preprints. https://doi.org/10.31219/osf.io/ectms")
  }

  return(I)
}
