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
#' @param class string: the class of the returned backbone graph, one of c("matrix", "igraph")
#'
#' @return
#' An incidence matrix of class `matrix` or a bipartite graph of class {\link{igraph}}.
#' 
#' @export
#'
#' @examples
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'   rowdist = c(1,1), coldist = c(1,1))  #Uniform
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'   rowdist = c(1,10), coldist = c(1,10))  #Right-tailed
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'   rowdist = c(10,1), coldist = c(10,1))  #Left-tailed
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'   rowdist = c(10,10), coldist = c(10,10))  #Normal
#' I <- incidence.from.distribution(R = 100, C = 100, P = 0.1,
#'   rowdist = c(10000,10000), coldist = c(10000,10000))  #Constant
incidence.from.distribution <- function(R, C, P, rowdist = c(1,1), coldist = c(1,1), class="matrix") {

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
  R <- integers(R,ones,rowdist[1],rowdist[2])  #Create a vector of agent degrees; if not possible, error
  C <- integers(C,ones,coldist[1],coldist[2])  #Create a vector of artifact degrees; if not possible, error
  I <- incidence.from.vector(R,C)  #Create incidence matrix

  #Verify, randomize, and return
  if (all.equal(R,rowSums(I)) == TRUE | all.equal(C,colSums(I)) == TRUE) {
    I <- curveball(I)
    if (class == "igraph"){I <- igraph::graph_from_incidence_matrix(I)}
    return(I)
  } else {stop("A bipartite network with these degree distributions is not possible")}
}
