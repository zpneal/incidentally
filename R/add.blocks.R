#' Adds a block structure to an incidence matrix
#'
#' `add.blocks` shuffles an incidence matrix to have a block structure or planted partition while preserving the row and column sums
#'
#' @param I matrix: An incidence matrix
#' @param blocks integer: number of blocks to add (between 2 and 26)
#' @param density numeric: desired within-block density
#'
#' @details
#' Stochastic block and planted partition models generate graphs in which the probability that two nodes are connected
#'    depends on whether they are members of the same or different blocks/partitions. Functions such as \link[igraph]{sample_sbm}
#'    can randomly sample from stochastic block models with given probabilities. In contrast `add.blocks` adds a block
#'    structure to an existing incidence matrix while preserving the row and column sums. Each row and each column are randomly
#'    assigned to one of `blocks` number of groups, then marginal-perserving checkerboard swaps are performed that increase
#'    the within-block density, until `density` is achieved (if possible).
#'
#' @return
#' matrix: An incidence matrix, row and column names begin with a letter indicating their block membership
#'
#' @references {Neal, Z. P., Domagalski, R., and Sagan, B. 2021. Comparing alternatives to the fixed degree sequence model for extracting the backbone of bipartite projections. *Scientific Reports, 11*, 23929. \doi{10.1038/s41598-021-03238-3}}
#'
#' @export
#'
#' @examples
#' I <- incidence.from.probability(R = 100, C = 100, P = .1)
#' blockedI <- add.blocks(I, blocks = 2, density = .7)
#' all(rowSums(I)==rowSums(blockedI))
#' all(colSums(I)==colSums(blockedI))
add.blocks <- function(I, blocks=2, density=.5) {

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

  # Parameter checks
  if (!is.numeric(blocks) | !is.numeric(density)) {stop("blocks and density must be numeric")}
  if (blocks<2 | blocks>26 | blocks%%1!=0) {stop("blocks must be a positive integer between 2 and 26")}
  if (density<0.5 | density>1) {stop("density must be between 0 and 1")}

  # Begin progress bar, assign nodes to blocks
  block.names <- LETTERS[seq(from = 1, to = blocks)]  #Generate list of group names
  rownames(I) <- paste0(sample(block.names,nrow(I),replace=TRUE),c(1:nrow(I)))  #Assign each row to a group
  colnames(I) <- paste0(sample(block.names,ncol(I),replace=TRUE),c(1:ncol(I)))  #Assign each column to a group
  within <- outer(substr(rownames(I),1,1), substr(colnames(I),1,1), `==`)  #Find within-group pairs
  within.block <- sum((within*I) / sum(I))  #Compute starting block density
  pb <- utils::txtProgressBar(min = .49, max = density, style = 3)  #Initiate progress bar

  while (within.block < density) { #While trying to improve density...

    # Get list a possible swaps
    possible <- sample.cb(I)
    possible$row1 <- rownames(I)[possible$row1]
    possible$row2 <- rownames(I)[possible$row2]
    possible$col1 <- colnames(I)[possible$col1]
    possible$col2 <- colnames(I)[possible$col2]
    possible <- possible[which(substr(possible$row1,1,1)!=substr(possible$row2,1,1) &  #Agents are from different groups
                               substr(possible$row1,1,1)!=substr(possible$row2,1,1) &  #Artifacts are from different groups
                               substr(possible$row1,1,1)==substr(possible$col1,1,1) &  #First agent and artifact are from same group
                               substr(possible$row2,1,1)==substr(possible$col2,1,1)),] #Second agent and artifact are from same group
    if (nrow(possible) == 0) {stop("No remaining swaps available")}

    # Try making swaps from the list, until achieving desired density
    for (try in 1:nrow(possible)) {
      if (within.block < density & all(matrix(c(0,1,1,0),nrow=2,ncol=2) == I[c(possible$row1[try],possible$row2[try]),c(possible$col1[try],possible$col2[try])])) {  #If necessary and allowable
        I[c(possible$row1[try],possible$row2[try]),c(possible$col1[try],possible$col2[try])] <- matrix(c(1,0,0,1),nrow=2,ncol=2)
        within.block <- sum((within*I) / sum(I))
        utils::setTxtProgressBar(pb, within.block)
      }
    }
  }

  # End progress bar & return
  close(pb)
  return(I)
}

