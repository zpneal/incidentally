#' Generates an incidence matrix with given row and column marginal sums
#'
#' `incidence.from.vector` generates a random incidence matrix with given row and column sums
#'
#' @param R numeric vector: row marginal sums
#' @param C numeric vector: column marginal sums
#' @param class string: the class of the returned backbone graph, one of c("matrix", "igraph")
#'
#' @return
#' An incidence matrix of class `matrix` or a bipartite graph of class {\link{igraph}}.
#'
#' @export
#'
#' @examples
#' I <- incidence.from.vector(R = c(1,1,2), C = c(1,1,2))
#' I <- incidence.from.vector(R = c(1,1,2), C = c(1,1,2), class = "igraph")
incidence.from.vector <- function(R,C,class="matrix"){
  #Replacement for sample() function so that if length(x)=1, it simply returns x
  newsample <- function(x) {if (length(x) <= 1) {return(x)} else {return(sample(x,1,replace = FALSE, prob = NULL))}}

  #Parameter check
  if (sum(R)!=sum(C)) {stop("sum(R) must equal sum(C)")}
  if (!is.numeric(R) | !is.numeric(C)) {stop("R and C must be numeric")}
  if (any(R%%1!=0) | (any(C%%1!=0)) | any(R<0) | any(C<0)) {stop("R and C must only contain non-negative integers")}

  #Set initial values
  r <- length(R) #number of rows
  row_s <- R #row sum vector to change
  col_s <- C #col sum vector to change
  I <- matrix(0, nrow = length(R), ncol = length(C)) #matrix of all zeros to change

  #Start inserting 1's to matrix
  for (counter in 1:length(C))
  repeat {
    #Loop stopping conditions
    if ((sum(row_s) == 0) & (sum(col_s) == 0)) {break}
    #Choose a random column index that corresponds to a nonzero entry
    col_index <- newsample(which(col_s > 0))
    #Find the column sum of that column
    c_sum <- col_s[col_index]
    #Rank the entries of the row sum sum vector row_s, breaking ties at random
    ranks <- rank(row_s, ties.method = "random")
    #Select c_sum largest entries, find their indices
    indices <- which(ranks > (r-c_sum))
    #Replace entries in matrix with a 1 if row index in indices, col index as chosen
    I[indices, col_index] <- 1
    #Update row and col sums
    row_s <- (R - rowSums(I))
    col_s <- (C - colSums(I))
  } #end while loop

  if ((sum(row_s) == 0) & (sum(col_s) == 0)) {
    I <- curveball(I)
    if (class == "igraph"){I <- igraph::graph_from_incidence_matrix(I)}
    return(I)
  } else {stop("An incidence matrix with these marginal sums does not exist.")}

}

