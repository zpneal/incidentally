#' Adds a block structure to an incidence matrix
#'
#' `add.blocks` shuffles an incidence matrix to have a block structure or planted partition while preserving the row and column sums
#'
#' @param I matrix: An incidence matrix
#' @param blocks integer: number of blocks to add (between 2 and 26)
#' @param density numeric: desired within-block density
#' @param max.tries numeric: number of ineligible re-wiring attempts before giving up
#'
#' @details
#' Stochastic block and planted partition models generate graphs in which the probability that two nodes are connected
#'    depends on whether they are members of the same or different blocks/partitions. Functions such as \link[igraph]{sample_sbm}
#'    can randomly sample from stochastic block models with given probabilities. In contrast `add.blocks` attempts to
#'    generate a block model that preserves the degree sequences (i.e., a matrix with preserved row and column sums).
#' 
#' Each row and each column node are randomly assigned to one of `blocks` number of groups. Then
#'    checkerboard swaps are performed that increase the within-block density, until `density` is achieved.
#'    Eligible swaps are identified randomly, so the re-wiring can be slow when `I` is large. The process
#'    can get stuck when no eligible swaps remain but the target `density` has not been achieved; if this
#'    happens, increase `max.tries` to keep looking for eligible swaps or reduce the target `density`.
#'
#' @return
#' matrix: An incidence matrix, row and column names begin with a letter indicating their block membership
#'
#' @export
#'
#' @examples
#' I <- incidence.from.probability(R = 20, C = 20, P = .5)
#' I <- add.blocks(I, blocks = 2, density = .7)
add.blocks <- function(I,blocks=2,density=.5,max.tries=100000) {

  # Parameter checks
  if (!is.numeric(blocks) | !is.numeric(density)) {stop("blocks and density must be numeric")}
  if (blocks<2 | blocks>26 | blocks%%1!=0) {stop("blocks must be a positive integer between 2 and 26")}
  if (density<0.5 | density>1) {stop("density must be between 0 and 1")}

  # Begin progress bar, assign nodes to blocks
  block.names <- LETTERS[seq(from = 1, to = blocks)]  #Generate list of group names
  rownames(I) <- paste0(sample(block.names,nrow(I),replace=TRUE),c(1:nrow(I)))  #Assign each row to a group
  colnames(I) <- paste0(sample(block.names,ncol(I),replace=TRUE),c(1:ncol(I)))  #Assign each column to a group
  within.block <- sum((outer(substr(rownames(I),1,1), substr(colnames(I),1,1), `==`)*1)*I) / sum(I)  #Compute starting block density
  pb <- utils::txtProgressBar(min = .49, max = density, style = 3)  #Initiate progress bar

  failed.swaps <- 0
  while (within.block < density) {
    # Pick agents
    agent1 <- sample(rownames(I),1)  #Pick a random agent
    agent2 <- sample(rownames(I)[which(substr(rownames(I),1,1)!=substr(agent1,1,1))],1)  #Pick a random agent from another group

    # Pick artifacts
    artifact1 <- sample(colnames(I)[which(substr(colnames(I),1,1)==substr(agent1,1,1))],1)  #Pick a random artifact from agent 1's group
    artifact2 <- sample(colnames(I)[which(substr(colnames(I),1,1)==substr(agent2,1,1))],1)  #Pick a random artifact from agent 2's group

    # If a checkboard swap would increase within-block density, make the swap and recompute
    if (all(matrix(c(0,1,1,0),nrow=2,ncol=2) == I[c(agent1,agent2),c(artifact1,artifact2)])) {
      I[c(agent1,agent2),c(artifact1,artifact2)] <- abs(I[c(agent1,agent2),c(artifact1,artifact2)] - 1)
      within.block <- sum((outer(substr(rownames(I),1,1), substr(colnames(I),1,1), `==`)*1)*I) / sum(I)
      utils::setTxtProgressBar(pb, within.block)
      failed.swaps <- 0
    } else {failed.swaps <- failed.swaps + 1}  #If a swap would not increase within-block density, increase counter

  # If within-block density can't be improved further, stop
  if (failed.swaps == max.tries) {stop("No more swaps found; try again with higher `max.tries` or lower target `density`.")}
  }

  # Arrange by groups and close progress bar
  close(pb)
  I <- I[order(rownames(I)), order(colnames(I))]
  return(I)
}
