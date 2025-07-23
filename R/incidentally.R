#' incidentally: Generates incidence matrices and bipartite graphs
#'
#' @description Functions to generate incidence matrices and bipartite graphs that have
#'    (1) a fixed fill rate, (2) given marginal sums, (3) marginal sums that follow given
#'    distributions, or (4) represent bill sponsorships in the US Congress. It can also
#'    generate an incidence matrix from an adjacency matrix, or bipartite graph from a
#'    unipartite graph, via a social process mirroring team, group, or organization formation.
#'
#'    Incidence matrices can be generated:
#'    \itemize{
#'    \item ...with a fixed fill rate: [incidence.from.probability()].
#'    \item ...with given marginals: [incidence.from.vector()].
#'    \item ...with marginals that follow given distributions: [incidence.from.distribution()].
#'    \item ...from a network, by a social process mirroring team, group, or organization formation [incidence.from.adjacency()].
#'    \item ...with a block structure or planted partition: [add.blocks()].
#'    \item ...from US Congress bill sponsorships: [incidence.from.congress()].
#'    }
#'
#' @aliases incidentally-package
#' @name incidentally
"_PACKAGE"
NULL
