#' Generates an incidence matrix with row and column sums that follow given distributions
#'
#' `incidence.from.distribution` generates a random incidence matrix with row and column
#'     sums that approximately follow beta distributions with given parameters.
#'
#' @param R integer: number of rows
#' @param C integer: number of columns
#' @param P numeric: probability that a cell contains a 1
#' @param rowdist vector length 2: Row marginals will approximately follow a Beta(a,b) distribution
#' @param coldist vector length 2: Column marginals will approximately follow a Beta(a,b) distribution
#' @param class string: the class of the returned backbone graph, one of c("matrix", "Matrix", "igraph").
#' @param narrative boolean: TRUE if suggested text & citations should be displayed.
#'
#' @return
#' An incidence matrix of class `matrix` or `Matrix`, or a bipartite graph of class {\link{igraph}}.
#'
#' @references {Neal, Z. P., Domagalski, R., and Sagan, B. 2021. Comparing alternatives to the fixed degree sequence model for extracting the backbone of bipartite projections. *Scientific Reports, 11*, 23929. \doi{10.1038/s41598-021-03238-3}}
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *OSF Preprints* \doi{10.31219/osf.io/ectms}}
#'
#' @export
#'
#' @examples
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'      rowdist = c(10000,10000), coldist = c(10000,10000))  #Constant
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'      rowdist = c(1,1), coldist = c(1,1))  #Uniform
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'      rowdist = c(1,10), coldist = c(1,10))  #Right-tailed
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'      rowdist = c(10,1), coldist = c(10,1))  #Left-tailed
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'      rowdist = c(10,10), coldist = c(10,10),
#'      narrative = TRUE)  #Normal
incidence.from.distribution <- function(R, C, P, rowdist = c(1,1), coldist = c(1,1), class="matrix", narrative = TRUE) {

  # Vector of N integers with sum S and approximately distributed as beta(a,b) ####
  integers <- function(N, S, a, b){
    for (i in 1:500000) {  #Attempt up to 500000 times
      if (a < b) {  #For right-tailed distributions
        rand <- rep(1,N)  #Start with vector of 1s, to set a floor
        plus <- stats::rbeta(N,a,b)  #Distribution of amount to add to the floor
        Sstar <- S-N  #Total value that can be added to the floor
        rand <- rand + round(plus * (Sstar / sum(plus)))  #Add extra value to floor
        }
      if (a >= b) {  #For symmetric and left-tailed distributions
        rand <- stats::rbeta(N,a,b)  #Distribution of values
        rand <- round(rand * (S / sum(rand)))  #Normalize to match desired total
        }
      if (sum(rand) == S & min(rand) != 0) {break}  #Stop when an allowable vector is generated
      }
    if (sum(rand) == S | min(rand) != 0) {return(rand)} else {stop("These marginal distributions are not possible")}
    }

  #Parameter check
  if (!is.numeric(R) | !is.numeric(C) | !is.numeric(P)) {stop("R, C, and P must be numeric")}
  if (R<0 | C<0 | R%%1!=0 | C%%1!=0) {stop("R and C must be positive integers")}
  if (P<0 | P>1) {stop("P must be between 0 and 1")}

  # Generate bipartite
  ones <- round(R * C * P)  #Number of 1s in matrix, given dimensions and density
  Rseq <- integers(R,ones,rowdist[1],rowdist[2])  #Create a vector of agent degrees; if not possible, error
  Cseq <- integers(C,ones,coldist[1],coldist[2])  #Create a vector of artifact degrees; if not possible, error
  I <- incidentally::incidence.from.vector(Rseq,Cseq)  #Create incidence matrix

  #Verify and randomize
  if (all.equal(Rseq,rowSums(I)) == TRUE | all.equal(Cseq,colSums(I)) == TRUE) {
    I <- incidentally::curveball(I)
    if (class == "igraph"){I <- igraph::graph_from_incidence_matrix(I)}
    if (class == "Matrix"){I <- Matrix::Matrix(I)}
  } else {stop("These distributions are not possible")}

  #Display narrative if requested
  if (narrative) {
    version <- utils::packageVersion("incidentally")
    if (class == "igraph") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a random bipartite graph with ", R, " agents whose degrees are approximately distributed as B(", rowdist[1], ",", rowdist[2],"), and ", C, " artifacts whose degrees are approximately distributed as B(", coldist[1], ",", coldist[2],").")}
    if (class != "igraph") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a random incidence matrix with ", R, " rows whose sums are approximately distributed as B(", rowdist[1], ",", rowdist[2],"), and ", C, " columns whose sums are approximately distributed as B(", coldist[1], ",", coldist[2],").")}
    message("")
    message("=== Suggested manuscript text and citations ===")
    message(text)
    message("")
    message("Neal, Z. P. (2022). incidentally: An R package to generate incidence matrices and bipartite graphs. OSF Preprints. https://doi.org/10.31219/osf.io/ectms")
  }

  return(I)
}
