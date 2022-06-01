.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("incidentally")
  packageStartupMessage("O  O  O  incidentally v",local_version)
  packageStartupMessage("|\\ | /|  Cite: Neal, Z. P. (2022). incidentally: An R package for generating incidence matrices")
  packageStartupMessage("|  |  |        and bipartite graphs. OSF Preprints. https://doi.org/10.31219/osf.io/ectms")
  packageStartupMessage("|/ | \\|  Help: type vignette(\"incidentally\"); email zpneal@msu.edu; github zpneal/incidentally")
  packageStartupMessage("X  X  X  Beta: type devtools::install_github(\"zpneal/incidentally\", ref = \"devel\")")
}
