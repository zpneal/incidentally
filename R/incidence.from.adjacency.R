#' Generates an incidence matrix from an adjacency matrix
#'
#' @description
#' `incidence.from.adjacency` generates an incidence matrix from an adjacency matrix or network using
#'    a given generative model
#'
#' @param G A symmetric, binary adjacency matrix of class `matrix` or `Matrix`,
#'    a `data.frame` containing a symbolic edge list in the first two columns,
#'    or an undirected, unweighted unipartite graph of class {\link{igraph}}.
#' @param k integer: Number of artifacts to generate
#' @param p numeric: Tuning parameter for artifacts, 0 <= p <= 1
#' @param d numeric: Number of dimensions in Blau space, d >= 2
#' @param model string: Generative model, one of c("team", "group", "blau") (see details)
#' @param class string: Return object as `matrix`, `igraph`, or `edgelist`. If `NULL`, object is returned in the same class as `G`.
#'
#' @return An incidence matrix of class `matrix`, or a bipartite graph as an edgelist of {\link{igraph}} object.
#'
#' @details
#' Given a unipartite network composed of *i agents* (i.e. nodes) that can be represented by an *i x i* adjacency
#'    matrix, `incidence.from.adjacency` generates a random *i x k* incidence matrix that indicates whether agent
#'    *i* is associated with *artifact k*. Generative models differ in how they conceptualize artifacts and how
#'    they associate agents with these artifacts.
#'
#' The **Team Model** (`model == "team"`) mirrors a team formation process, where each artifact represents a new team
#'     formed from the incumbants of a prior team (with probability `p`) and newcomers (with probability 1-`p`).
#'
#' The **Group Model** (`model == "group"`) mirrors a social group formation process, where each artifact represents
#'     a social group. Group members attempt to recruit non-member friends, who join the group if it would have a
#'     density of at least `p`.
#'
#' The **Blau Space Model** (`model == "blau"`) mirrors an organization (the artifact) recruiting members from social
#'     space, where those within the organization's niche join with probability `p`, and those outside the niche join
#'     with probability 1-`p`.
#'
#' @export
#'
#' @examples
#' G <- igraph::erdos.renyi.game(10, .4)
#' I <- incidence.from.adjacency(G, k = 1000, p = .95,
#'                               model = "team")
incidence.from.adjacency <- function(G, k = 1, p = 1, d = 2, model = "team", class = NULL) {

  #### Sampling function, to allow sampling from a vector with one entry ####
  sample.vec <- function(x, ...) x[sample(length(x), ...)]

  #### Parameter checks ####
  if (is.null(class) & methods::is(G, "igraph")) {class <- "igraph"}
  if (is.null(class) & methods::is(G, "matrix")) {class <- "matrix"}
  if (is.null(class) & methods::is(G, "Matrix")) {class <- "matrix"}
  if (is.null(class) & methods::is(G, "data.frame")) {class <- "edgelist"}
  if (!is.numeric(k)) {stop("k must be numeric")}
  if (!is.numeric(d)) {stop("d must be numeric")}
  if (!is.numeric(p)) {stop("p must be numeric")}
  if (d%%1!=0 | d < 2) {stop("d must be an integer greater than 1")}
  if (p < 0 | p > 1) {stop("p must be between 0 and 1")}
  if (!(model %in% c("team", "group", "blau"))) {stop("model must be one if c(\"team\", \"group\", \"blau\")")}
  if (!(class %in% c("matrix", "igraph"))) {stop("model must be one if c(\"matrix\", \"igraph\"")}

  #### Check input, get names, convert to igraph ####
  if (methods::is(G, "igraph")) {
    if (igraph::is_directed(G)) {stop("G must be undirected")}
    if (igraph::is_weighted(G)) {stop("G must be unweighted")}
    if (igraph::is_bipartite(G)) {stop("G must be unipartite")}
    if (!is.null(igraph::V(G)$name)) {nodes <- igraph::V(G)$name} else {nodes <- 1:(igraph::gorder(G))}
  }
  if (methods::is(G, "matrix")) {
    if (!isSymmetric(G)) {stop("G must be symmetric")}
    if (!all(G %in% c(0,1))) {stop("G must be binary")}
    if (!is.null(rownames(G))) {nodes <- rownames(G)} else {nodes <- 1:nrow(G)}
    G <- igraph::graph_from_adjacency_matrix(G,mode="undirected")
  }
  if (methods::is(G, "Matrix")) {
    G <- as.matrix(G)
    if (!isSymmetric(G)) {stop("G must be symmetric")}
    if (!all(G %in% c(0,1))) {stop("G must be binary")}
    if (!is.null(rownames(G))) {nodes <- rownames(G)} else {nodes <- 1:nrow(G)}
    G <- igraph::graph_from_adjacency_matrix(G,mode="undirected")
  }
  if (methods::is(G, "data.frame")) {
    if (ncol(G)!=2) {stop("the edgelist must have two columns")}
    G <- igraph::graph_from_data_frame(G, directed = F)
    if (!is.null(igraph::V(G)$name)) {nodes <- igraph::V(G)$name} else {nodes <- 1:(igraph::gorder(G))}
  }

  I <- as.matrix(1:(igraph::gorder(G)))  #Create empty incidence with numeric row labels

  #### Team model (Guimera et al., 2005) ####
  if (model == "team") {

    cliques <- igraph::cliques(G, min=2)  #List of all cliques
    #This step will be slow for large/dense graphs. For these graphs, consider using this approximation
    # - List all maximal cliques
    # - In loop, sample one maximal clique
    # - In loop, sample between 2 and N nodes from the maximal clique

    for (i in 1:k) {                              #For each new team k:
      clique <- sample(1:length(cliques),1)       #Pick a prior team
      incumbent <- as.numeric(cliques[[clique]])  #List its members
      newcomer <- nodes[!nodes %in% incumbent]    #List its nonmembers
      size <- length(incumbent)                   #Find its size
      members <- rep(0, times = size)             #Blank list of new team's members

      for (j in 1:size) {
        if (j == 1) {                                        #For the first position j = 1:
          members[j] <- sample.vec(incumbent,1)              #Fill with a random incumbent
          incumbent <- incumbent[!incumbent %in% members]    #Update the list of remaining incumbents
        } else {if (stats::runif(1) <= p) {                  #For all remaining positions j > 1:
            members[j] <- sample.vec(incumbent,1)            #With probability p, fill position j with a random incumbent
            incumbent <- incumbent[!incumbent %in% members]  #Update the list of remaining incumbents
            } else {
            members[j] <- sample.vec(newcomer,1)             #With probability p-1, fill position j with a random newcomer
            newcomer <- newcomer[!newcomer %in% members]     #Update the list of remaining newcomers
            }
          }
        }

      I <- cbind(I,0)          #Add a blank artifact (team) to I
      I[members, i + 1] <- 1   #Fill the artifact with the new team's members
    }
  }

    #### Group model (Backstrom et al., 2006) ####
    if (model == "group") {

      ## Function to get candidates
      get.candidates <- function(G, members, declined) {
        candidates <- unique(unlist(igraph::adjacent_vertices(G, members)))  #All neighbors of members
        candidates <- candidates[!candidates %in% members]  #Remove members
        candidates <- candidates[!candidates %in% declined]  #Remove recruits who have declined
        return(candidates)
      }

      cliques <- igraph::cliques(G, min=2)  #List of all cliques
      #This step will be slow for large/dense graphs. For these graphs, consider using this approximation
      # - List all maximal cliques
      # - In loop, sample one maximal clique
      # - In loop, sample between 2 and N nodes from the maximal clique

      for (i in 1:k) {                                                  #For each new group k:
        members <- as.numeric(cliques[[sample(1:length(cliques),1)]])   #Sample a clique (initial members)
        declined <- NULL                                                #Recruits who declined membership
        candidates <- get.candidates(G, members, declined)

        while (length(candidates)!= 0) {                                                               #While there are candidates
          recruit <- sample.vec(candidates, 1)                                                         #Pick random recruit from candidates
          density <- igraph::edge_density(igraph::induced_subgraph(G, c(members,recruit)))             #Compute prospective group's density
          if (density >= p) {members <- c(members, recruit)} else {declined <- c(declined, recruit)}   #Join or decline
          candidates <- get.candidates(G, members, declined)                                           #Update candidate list
        }

        I <- cbind(I,0)          #Add a blank artifact (group) to I
        I[members, i + 1] <- 1   #Fill the artifact with the new group's members
      }
    }

  #### BlauSpace model (McPherson, 2004) ####
  if (model == "blau") {

    if (!igraph::is.connected(G)) {stop("the blau space model requires that the network be connected")}
    coords <- igraph::layout_with_mds(G, dim = d)  #Get coordinates in Blau Space
    D <- as.matrix(stats::dist(coords))            #Compute distances between nodes in Blau Space
    diag(D) <- NA

    i <- 1
    while (dim(I)[2] < (k + 1)) {                    #Until `k` organizations are created, for each organization i:
      leader <- sample(1:igraph::gorder(G),1)        #Pick node to serve as organization leader and niche center
      R <- (stats::rbeta(1,2,5) * (max(D,na.rm=T) - min(D,na.rm=T))) + min(D,na.rm=T)  #Pick niche radius
      prob <- ifelse(D[leader,] <= R, p, 1-p)        #Probability of joining, depending on whether inside or outside niche
      prob[is.na(prob)] <- 1                         #Leader always joins
      members <- stats::rbinom(length(prob),1,prob)  #Nodes join organization with given probability
      if (sum(members) > 1) {      #If more than one person joins organization i
        I <- cbind(I, members)     #Add this organization's member list to I
        i <- i + 1}                #Go to the next organization
    }
  }

  # Clean up and return
  I <- I[,-1]  #Remove placeholder ID column
  if (!methods::is(I,"numeric")) {
    rownames(I) <- igraph::V(G)$name  #Insert row names
    colnames(I) <- c(paste0("k", 1:ncol(I)))  #Insert column names
  }
  if (class == "igraph") {I <- igraph::graph_from_incidence_matrix(I)}
  if (class == "edgelist") {I <- data.frame(igraph::as_edgelist(igraph::graph_from_incidence_matrix(I)))}
  return(I)  #Return the bipartite graph with row labels
}

