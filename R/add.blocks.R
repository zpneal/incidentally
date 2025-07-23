#' Adds a block structure to an incidence matrix
#'
#' `add.blocks` shuffles an incidence matrix to have a block structure or planted partition while preserving the row and column sums
#'
#' @param I An incidence matrix or \link[igraph]{igraph} bipartite graph
#' @param rowblock numeric: vector indicating each row node's block membership
#' @param colblock numeric: vector indicating each column node's block membership
#' @param density numeric: desired within-block density
#' @param sorted boolean: if TRUE, return incidence matrix permuted by block
#'
#' @details
#' Stochastic block and planted partition models generate graphs in which the probability that two nodes are connected
#'    depends on whether they are members of the same or different blocks/partitions. Functions such as \link[igraph]{sample_sbm}
#'    can randomly sample from stochastic block models with given probabilities. In contrast `add.blocks` adds a block
#'    structure to an existing incidence matrix while preserving the row and column sums. Row nodes' and column nodes'
#'    block memberships are supplied in separate vectors. If block membership vectors are not provided, then nodes are
#'    randomly assigned to two groups.
#'
#' @return
#' An incidence matrix or \link[igraph]{igraph} bipartite graph with a block structure
#'
#' @references {Neal, Z. P., Domagalski, R., and Sagan, B. 2021. Comparing alternatives to the fixed degree sequence model for extracting the backbone of bipartite projections. *Scientific Reports, 11*, 23929. \doi{10.1038/s41598-021-03238-3}}
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *CRAN* \doi{10.32614/CRAN.package.incidentally}}
#'
#' @export
#'
#' @examples
#' I <- incidence.from.probability(R = 100, C = 100, P = .1)
#' blocked <- add.blocks(I, density = .7)
#' all(rowSums(I)==rowSums(blocked))
#' all(colSums(I)==colSums(blocked))
#'
#' B <- igraph::sample_bipartite(100, 100, p=.1)
#' blocked <- add.blocks(B, density = .7)
#' all(igraph::degree(B)==igraph::degree(blocked))
add.blocks <- function(I,
                       rowblock = sample(1:2,replace=T,nrow(I)),
                       colblock = sample(1:2,replace=T,ncol(I)),
                       density = .5,
                       sorted = FALSE) {

  #Function to sample 2x2 checkerboards from a larger matrix
  sample.cb <- function(m) {
    n <- nrow(m) * ncol(m)
    m <- Matrix::spMatrix(nrow(m), ncol(m),  #Convert m to a sparse matrix in triplet format
                           i = which(m != 0, arr.ind = T)[,1],
                           j = which(m != 0, arr.ind = T)[,2],
                           x = rep(1,sum(m)))
    mIdx <- matrix(sample(length(m@i), 2L*n, TRUE), ncol = 2)
    dt <- data.frame(row1 = m@i[mIdx[,1]], col1 = m@j[mIdx[,2]], row2 = m@i[mIdx[,2]], col2 = m@j[mIdx[,1]]) + 1L
    dt <- dt[which(dt$row1 != dt$row2 & dt$col1 != dt$col2 & !(m[matrix(c(dt$row1, dt$col1), n)] + m[matrix(c(dt$row2, dt$col2), n)])),]
    return(dt)
  }

  # Prep object
  if (!methods::is(I,"matrix") & !methods::is(I,"igraph")) {stop("I must be a matrix or igraph object")}
  class <- "matrix"
  if (methods::is(I,"igraph")) {
    if (!igraph::is.bipartite(I)) {stop("I must be bipartite")}
    I <- igraph::as_incidence_matrix(I)
    class <- "igraph"
  }

  # Parameter checks
  if (!is.numeric(density)) {stop("density must be numeric")}
  if (density<0 | density>1) {stop("density must be between 0 and 1")}
  if (!is.numeric(rowblock) | !is.numeric(colblock)) {stop("rowblock and colblock must be integer vectors")}
  if (length(rowblock)!=nrow(I)) {stop("rowblock must contain nrow(I) elements")}
  if (length(colblock)!=ncol(I)) {stop("colblock must contain ncol(I) elements")}

  # Check starting within-block density
  within <- outer(rowblock, colblock, `==`)  #Find within-group pairs
  within.block <- sum((within*I) / sum(I))  #Compute starting block density
  if (within.block > density) {stop("I already has a within-block density > `density`")}
  pb <- utils::txtProgressBar(min = within.block, max = density, style = 3)  #Initiate progress bar

  while (within.block < density) { #While trying to improve density...

    # Get list a possible swaps
    possible <- sample.cb(I)
    possible <- possible[which(rowblock[possible$row1]!=rowblock[possible$row2] &  #Agents are from different groups
                               colblock[possible$col1]!=colblock[possible$col2] &  #Artifacts are from different groups
                               rowblock[possible$row1]==colblock[possible$col1] &  #First agent and artifact are from same group
                               rowblock[possible$row2]==colblock[possible$col2]),] #Second agent and artifact are from same group
    if (nrow(possible) == 0) {stop("Requested within-block density not achieved")}

    # Try making swaps from the list, until achieving desired density
    for (try in 1:nrow(possible)) {
      if (within.block < density & all(matrix(c(0,1,1,0),nrow=2,ncol=2) == I[c(possible$row1[try],possible$row2[try]),c(possible$col1[try],possible$col2[try])])) {  #If necessary and allowable
        I[c(possible$row1[try],possible$row2[try]),c(possible$col1[try],possible$col2[try])] <- matrix(c(1,0,0,1),nrow=2,ncol=2)
        within.block <- sum((within*I) / sum(I))
        utils::setTxtProgressBar(pb, within.block)
      }
    }
  }

  # Complete & end progress bar, return
  utils::setTxtProgressBar(pb, density)
  close(pb)
  if (sorted & class=="matrix") {I <- I[order(rowblock), order(colblock)]}
  if (class == "igraph") {I <- igraph::graph_from_incidence_matrix(I)}
  return(I)
}

