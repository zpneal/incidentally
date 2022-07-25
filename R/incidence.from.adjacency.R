#' Generates an incidence matrix from an adjacency matrix
#'
#' @description
#' `incidence.from.adjacency` generates an incidence matrix from an adjacency matrix or network using
#'    a given generative model
#'
#' @param G A symmetric, binary adjacency matrix of class `matrix` or `Matrix`,
#'    or an undirected, unweighted unipartite graph of class {\link{igraph}}.
#' @param k integer: Number of artifacts to generate
#' @param p numeric: Tuning parameter for artifacts, 0 <= p <= 1
#' @param maximal boolean: Should teams/clubs models be seeded with *maximal* cliques?
#' @param blau.param vector: Vector of parameters that control blau space in the organizations model (see details)
#' @param model string: Generative model, one of c("team", "club", "org") (see details)
#' @param class string: Return object as `matrix`, `Matrix`, or `igraph`. If `NULL`, object is returned in the same class as `G`.
#' @param narrative boolean: TRUE if suggested text & citations should be displayed.
#'
#' @return An incidence matrix of class `matrix` or `Matrix`, or a bipartite graph of class {\link{igraph}}.
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
#' The **Club Model** (`model == "club"`) mirrors a social club formation process, where each artifact represents
#'     a social club. Club members attempt to recruit non-member friends, who join the club if it would have a
#'     density of at least `p`.
#'
#' The **Organizations Model** (`model == "org"`) mirrors an organization (the artifact) recruiting members from social
#'     space, where those within the organization's niche join with probability `p`, and those outside the niche join
#'     with probability 1-`p`. `blau.param` is a vector containing three values that control the characteristics of the
#'     blau space. The first value is the space's dimensionality. The second two values are shape parameters of a Beta
#'     distribution that describes niche sizes. The default is a two-dimensional blau space, with organization niche
#'     sizes that are strongly positively skewed (i.e., many specialist organizations, few generalists).
#'
#' @references {Neal, Z. P. 2022. The Duality of Networks and Foci: Generative Models of Two-Mode Networks from One-Mode Networks. *arXiv:2204.13670* \[cs.SI\]. \doi{10.48550/arXiv.2204.13670}}
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *OSF Preprints* \doi{10.31219/osf.io/ectms}}
#'
#' @export
#'
#' @examples
#' G <- igraph::erdos.renyi.game(10, .4)
#' I <- incidence.from.adjacency(G, k = 1000, p = .95,
#'                               model = "team", narrative = TRUE)
incidence.from.adjacency <- function(G, k = 1, p = 1, blau.param = c(2,1,10), maximal = TRUE, model = "team", class = NULL, narrative = TRUE) {

  #### Sampling function, to allow sampling from a vector with one entry ####
  sample.vec <- function(x, ...) x[sample(length(x), ...)]

  #### Parameter checks ####
  if (is.null(class) & methods::is(G, "igraph")) {class <- "igraph"}
  if (is.null(class) & methods::is(G, "matrix")) {class <- "matrix"}
  if (is.null(class) & methods::is(G, "Matrix")) {class <- "Matrix"}
  if (!is.numeric(k)) {stop("k must be numeric")}
  if (!is.numeric(p)) {stop("p must be numeric")}
  if (p < 0 | p > 1) {stop("p must be between 0 and 1")}
  if (!(model %in% c("team", "club", "org"))) {stop("model must be one if c(\"team\", \"club\", \"org\")")}
  if (!(class %in% c("matrix", "Matrix", "igraph"))) {stop("class must be one if c(\"matrix\", \"Matrix\", \"igraph\")")}
  if (blau.param[1]%%1!=0 | blau.param[1]<2) {stop("The first blau.param must be an integer greater than 1")}
  if (blau.param[2]<0 | blau.param[3]<0) {stop("The second and third blau.param must be positive")}

  #### Check input, get names, convert to igraph ####
  if (methods::is(G, "igraph")) {
    if (igraph::is_directed(G)) {stop("G must be undirected")}
    if (igraph::is_weighted(G)) {stop("G must be unweighted")}
    if (igraph::is_bipartite(G)) {stop("G must be unipartite")}
    if (!is.null(igraph::V(G)$name)) {names <- igraph::V(G)$name} else {names <- 1:(igraph::gorder(G))}
    igraph::V(G)$name <- 1:(igraph::gorder(G))
  }
  if (methods::is(G, "matrix")) {
    if (!isSymmetric(G)) {stop("G must be symmetric")}
    if (!all(G %in% c(0,1))) {stop("G must be binary")}
    if (!is.null(rownames(G))) {names <- rownames(G)} else (names <- c(1:nrow(G)))
    G <- igraph::graph_from_adjacency_matrix(G,mode="undirected")
  }
  if (methods::is(G, "Matrix")) {
    G <- as.matrix(G)
    if (!isSymmetric(G)) {stop("G must be symmetric")}
    if (!all(G %in% c(0,1))) {stop("G must be binary")}
    if (!is.null(rownames(G))) {names <- rownames(G)} else (names <- c(1:nrow(G)))
    G <- igraph::graph_from_adjacency_matrix(G,mode="undirected")
  }

  nodes <- c(1:igraph::gorder(G))
  I <- as.matrix(1:(igraph::gorder(G)))  #Create empty incidence with numeric row labels

  #### Team model (Guimera et al., 2005) ####
  if (model == "team") {

    if (maximal) {cliques <- igraph::max_cliques(G, min=2)} else {cliques <- igraph::cliques(G, min=2)}  #List all (maximal) cliques

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

    #### Club model (Backstrom et al., 2006) ####
    if (model == "club") {

      ## Function to get candidates
      get.candidates <- function(G, members, declined) {
        candidates <- unique(unlist(igraph::adjacent_vertices(G, members)))  #All neighbors of members
        candidates <- candidates[!candidates %in% members]  #Remove members
        candidates <- candidates[!candidates %in% declined]  #Remove recruits who have declined
        return(candidates)
      }

      if (maximal) {cliques <- igraph::max_cliques(G, min=2)} else {cliques <- igraph::cliques(G, min=2)}  #List all (maximal) cliques

      for (i in 1:k) {                                                  #For each new club k:
        members <- as.numeric(cliques[[sample(1:length(cliques),1)]])   #Sample a clique (initial members)
        declined <- NULL                                                #Recruits who declined membership
        candidates <- get.candidates(G, members, declined)

        while (length(candidates)!= 0) {                                                               #While there are candidates
          recruit <- sample.vec(candidates, 1)                                                         #Pick random recruit from candidates
          density <- igraph::edge_density(igraph::induced_subgraph(G, c(members,recruit)))             #Compute prospective club's density
          if (density >= p) {members <- c(members, recruit)} else {declined <- c(declined, recruit)}   #Join or decline
          candidates <- get.candidates(G, members, declined)                                           #Update candidate list
        }

        I <- cbind(I,0)          #Add a blank artifact (club) to I
        I[members, i + 1] <- 1   #Fill the artifact with the new club's members
      }
    }

  #### Organizations model (McPherson, 2004) ####
  if (model == "org") {

    if (!igraph::is.connected(G)) {stop("the organizations model requires that the network be connected")}
    coords <- igraph::layout_with_mds(G, dim = blau.param[1])  #Get coordinates in Blau Space
    D <- as.matrix(stats::dist(coords))            #Compute distances between nodes in Blau Space

    i <- 1
    while (dim(I)[2] <= k) {                         #Until `k` organizations are created, for each organization i:
      R <- (stats::rbeta(1,blau.param[2],blau.param[3]) * (max(D,na.rm=T) - min(D,na.rm=T))) + min(D,na.rm=T)  #Pick niche radius
      center <- sample(1:igraph::gorder(G),1)        #Pick a node to serve as niche center
      dat <- data.frame(dist = D[center,])           #Start dataframe, add nodes' distance from niche center
      dat$inside <- dat$dist <= R                    #...add nodes' location inside or outside niche
      dat$member <- NA                               #...add initial memberships in organization
      dat$member[which(dat$inside)] <- stats::rbinom(sum(dat$inside),1,p)            #Each node inside niche joins with probability p
      while (sum(dat$member,na.rm=T)<sum(dat$inside) & sum(is.na(dat$member))!=0) {  #While spaces & recruits are available...
        recruit <- min(dat$dist[which(is.na(dat$member))])                           #...find recruit nearest to niche
        dat$member[which(dat$dist==recruit)] <- stats::rbinom(1,1,1-p)               #...attempt to recruit
      }
      dat$member[which(is.na(dat$member))] <- 0  #Non-recruits are non-members
      if (sum(dat$member) > 1) {   #If more than one person joins organization i...
        I <- cbind(I, dat$member)  #...add this organization's member list to I
        i <- i + 1}                #...go to the next organization
    }
  }

  # Clean up and return
  I <- I[,-1]  #Remove placeholder ID column
  if (!methods::is(I,"numeric")) {
    rownames(I) <- names  #Insert row names
    colnames(I) <- c(paste0("k", 1:ncol(I)))  #Insert column names
  }
  if (class == "igraph") {I <- igraph::graph_from_incidence_matrix(I)}
  if (class == "Matrix"){I <- Matrix::Matrix(I)}

  #Display narrative if requested
  if (narrative) {
    version <- utils::packageVersion("incidentally")
    type <- " newly-formed teams"
    if (model == "club") {type <- " newly-formed club"}
    if (model == "org") {type <- " newly-formed organizations"}
    if (class == "igraph") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022a) to generate a random bipartite graph from a unipartite graph. The bipartite graph represents the memberships of the ", igraph::gorder(G), " nodes from the unipartite network in ", k, type, " (Neal, 2022b).")}
    if (class != "igraph") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022a) to generate a random incidence matrix from a unipartite graph. The incidence matrix represents the memberships of the ", igraph::gorder(G), " nodes from the unipartite network (rows) in ", k, type, " (columns; Neal, 2022b).")}
    message("")
    message("=== Suggested manuscript text and citations ===")
    message(text)
    message("")
    message("Neal, Z. P. (2022a). incidentally: An R package to generate incidence matrices and bipartite graphs. OSF Preprints. https://doi.org/10.31219/osf.io/ectms")
    message("Neal, Z. P. (2022b). The Duality of Networks and Foci: Generative Models of Two-Mode Networks from One-Mode Networks. arXiv:2204.13670 [cs.SI]. https://doi.org/10.48550/arXiv.2204.13670")
  }

  return(I)  #Return the bipartite graph with row labels
}

