#' Generate bill sponsorship incidence matrices and bipartite graphs
#'
#' `incidence.from.congress()` uses data from \href{https://www.congress.gov/}{https://www.congress.gov/} to construct an incidence
#'    matrix or bipartite graph recording legislators' bill (co-)sponsorships.
#'
#' @param session numeric: the session of congress
#' @param types vector: types of bills to include. May be any combination of c(\"s\", \"sres\", \"sjres\", \"sconres\") OR any combination of c(\"hr\", \"hres\", \"hjres\", \"hconres\").
#' @param areas string: policy areas of bills to include (see details)
#' @param weighted boolean: should sponsor-bill edges have a weight of 2, but cosponsor-bill edges have a weight of 1
#' @param format string: format of output, one of c(\"data\", \"igraph\")
#' @param narrative boolean: TRUE if suggested text & citations should be displayed.
#'
#' @details
#' The `incidence.from.congress()` function uses data from \href{https://www.congress.gov/}{https://www.congress.gov/} to
#'    construct an incidence matrix or bipartite graph recording legislators' bill (co-)sponsorships. In an incidence matrix
#'    **I**, entry *Iik = 1* if legislator *i* sponsored or co-sponsored bill *k*, and otherwise is 0. In a bipartite graph
#'    **G**, a legislator *i* is connected to a bill *k* if *i* sponsored or co-sponsored *k*.
#'
#' In the US Congress, the law making process begins when a *sponsor* legislator introduces a bill in their chamber (House of
#'    Representatives or Senate). Additional legislators in the same chamber can support the bill by joining as a *co-sponsor*.
#'    The bill is discussed, revised, and possibly voted on in the chamber. If it passes in one chamber, it is sent to the other
#'    chamber for further discussion, revision, and possibly a vote. If it passed both chambers, it is sent to the President. If
#'    the President signs the bill, it becomes law.
#'
#' In the House of Representatives, legislators can introduce four types of bills: a House Bill (hb), a House Joint Resolution (hjres),
#'    a House Concurrent Resolution (hconres), and a House Simple Resolution (hres). Similarly, in the Senate, legislators can introduce
#'    four types of bills: a Senate Bill (s), a Senate Joint Resolution (sjres), a Senate Concurrent Resolution (sconres), and a Senate
#'    Simple Resolution (sres). In both chambers, concurrent and simple resolutions are used for minor procedural matters and do not
#'    have the force of law. Only bills and joint resolutions require the President's signature and have the force of law if signed.
#'
#' Each bill is assigned a policy area by the Congressional Research Service. By default, bills from all policy areas are included,
#'    however the `areas` parameter can be used to include only bills addressing certain policy areas. The `areas` takes a vector of
#'    strings listing the desired policy areas (e.g., `areas = c("Congress", "Animals")`). Policy area names are **case-sensitive**. A
#'    complete list of policy areas and brief descriptions is available at \href{https://www.congress.gov/help/field-values/policy-area}{https://www.congress.gov/help/field-values/policy-area}.
#'
#' @return
#' If `format = "data"`, a list containing an incidence matrix, a dataframe of legislator characteristics, and a dataframe of bill characteristics.
#'
#' If `format = "igraph"`, a bipartite igraph object composed of legislator vertices and bill vertices, each with vertex attributes.
#'
#' @references
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *OSF Preprints* \doi{10.31219/osf.io/ectms}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' D <- incidence.from.congress(session = 116, types = "s", format = "data")
#' D <- incidence.from.congress(session = 116, types = "s", format = "data", areas = "Animals")
#' G <- incidence.from.congress(session = 115, types = c("hr", "hres"), format = "igraph")
#' }
incidence.from.congress <- function(session = NULL, types = NULL, areas = "All", weighted = FALSE, format = "data", narrative = FALSE){

  #Parameter check
  if (!is.numeric(session)) {stop("session must be an integer")}
  if (session%%1!=0) {stop("session must be an integer")}
  if (!(all(types %in% c("s", "sres", "sjres", "sconres"))) & !(all(types %in% c("hr", "hres", "hjres", "hconres")))) {stop("types must be a combination of c(\"s\", \"sres\", \"sjres\", \"sconres\") OR a combination of c(\"hr\", \"hres\", \"hjres\", \"hconres\")")}
  if (!(format %in% c("data", "igraph"))) {stop("format must be one of c(\"data\", \"igraph\")")}

  #Initialize data
  dat <- data.frame(id = NULL, name = NULL, last = NULL, party = NULL, state = NULL, bill = NULL, introduced = NULL, title = NULL, area = NULL, status = NULL, weight = NULL)

  #Begin bill type loop
  for (type in types) {

    #Download zip of bill-type as tempfile, determine contents
    temp <- tempfile()
    message(paste0("Retriving bills from session ", session))
    utils::download.file(paste0("https://www.govinfo.gov/bulkdata/BILLSTATUS/",session,"/",type,"/BILLSTATUS-",session,"-",type,".zip"),temp)  #Download file
    files <- utils::unzip(temp, list = TRUE)$Name  #Unzip and get list of XML contents
    number.of.bills <- length(files)
    message(paste0("Examining ", number.of.bills, " bills"))
    pb <- utils::txtProgressBar(min = 1, max = number.of.bills, style = 3)  #Initiate progress bar

    #Parse each bill in zip
    for (file in 1:number.of.bills) {
      bill <- xml2::read_xml(unz(temp, files[file]))

      #Check area, add bill if relevant
      area <- xml2::xml_text(xml2::xml_find_first(bill, ".//policyArea"))
      if (areas=="All" | area %in% areas) {

      #Bill characteristics
      number <- paste0(xml2::xml_text(xml2::xml_find_first(bill, ".//billType")),xml2::xml_text(xml2::xml_find_first(bill, ".//billNumber")))
      introduced <- xml2::xml_text(xml2::xml_find_first(bill, ".//introducedDate"))
      title <- xml2::xml_text(xml2::xml_find_first(bill, ".//title"))
      status <- "Introduced"
      if (grepl("Became Public Law", xml2::xml_text(bill))) {status <- "Became law"}
      if (grepl("Passed/agreed to in Senate", xml2::xml_text(bill)) & grepl("Passed/agreed to in House", xml2::xml_text(bill)) & !(grepl("Became Public Law", xml2::xml_text(bill)))) {status <- "Sent to president"}
      if (grepl("Passed/agreed to in Senate", xml2::xml_text(bill)) & !(grepl("Passed/agreed to in House", xml2::xml_text(bill)))) {status <- "Passed senate"}
      if (grepl("Passed/agreed to in House", xml2::xml_text(bill)) & !(grepl("Passed/agreed to in Senate", xml2::xml_text(bill)))) {status <- "Passed house"}

      #Sponsor
      sponsor <- xml2::xml_find_first(bill, ".//sponsors")
      sponsor <- xml2::xml_find_all(sponsor, ".//item")
      s.id <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//bioguideId"))
      s.name <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//fullName"))
      s.last <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//lastName"))
      s.last <- tools::toTitleCase(tolower(s.last))  #Correcting capitalization
      s.party <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//party"))
      s.state <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//state"))

      #Co-sponsors
      cosponsor <- xml2::xml_find_first(bill, ".//cosponsors")
      cosponsor <- xml2::xml_find_all(cosponsor, ".//item")
      cs.id <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//bioguideId"))
      cs.name <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//fullName"))
      cs.last <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//lastName"))
      cs.last <- tools::toTitleCase(tolower(cs.last))  #Correcting capitalization
      cs.party <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//party"))
      cs.state <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//state"))

      #Add to data
      dat <- rbind(dat, data.frame(id = s.id, name = s.name, last = s.last, party = s.party, state = s.state, bill = number, introduced = introduced, title = title, area = area, status = status, weight = 2))
      if (length(cs.id)>0) dat <- rbind(dat, data.frame(id = cs.id, name = cs.name, last = cs.last, party = cs.party, state = cs.state, bill = number, introduced = introduced, title = title, area = area, status = status, weight = 1))
      }

      utils::setTxtProgressBar(pb, file)
    } #End file loop
    close(pb)
  } #End type loop

  #Prep sponsorship data and codebooks
  if (weighted) {sponsorship <- dat[c("name", "bill", "weight")]} else {sponsorship <- dat[c("name", "bill")]}
  legislator <- unique(dat[c("id", "name", "last", "party", "state")])
  bills <- unique(dat[c("bill", "introduced", "title", "area", "status")])

  #Display narrative if requested
  if (narrative) {
    version <- utils::packageVersion("incidentally")
    if (all(types %in% c("s", "sres", "sjres", "sconres"))) {who <- "Senators'"}
    if (all(types %in% c("hr", "hres", "hjres", "hconres"))) {who <- "Representatives'"}
    if (format == "igraph" & areas == "All") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a bipartite graph recording ", who, " bill sponsorships during the ", session, " session of the US Congress.")}
    if (format == "igraph" & areas != "All") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a bipartite graph recording ", who, " bill sponsorships during the ", session, " session of the US Congress. We restricted our focus to bills in the following policy areas: ", paste(areas, collapse=', '), ".")}
    if (format == "data" & areas == "All") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate an incidence matrix recording ", who, " bill sponsorships during the ", session, " session of the US Congress.")}
    if (format == "data" & areas != "All") {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate an incidence matrix recording ", who, " bill sponsorships during the ", session, " session of the US Congress. We restricted our focus to bills in the following policy areas: ", paste(areas, collapse=', '), ".")}
    message("")
    message("=== Suggested manuscript text and citations ===")
    message(text)
    message("")
    message("Neal, Z. P. (2022). incidentally: An R package to generate incidence matrices and bipartite graphs. OSF Preprints. https://doi.org/10.31219/osf.io/ectms")
  }

  #Construct incidence matrix and codebooks
  if (format == "data") {
    G <- igraph::graph_from_data_frame(sponsorship, directed = F)
    igraph::V(G)$type <- igraph::V(G)$name %in% sponsorship[,2] #second column of edges is TRUE type
    if (weighted) {I <- igraph::as_incidence_matrix(G, attr = "weight", sparse = FALSE)} else {I <- igraph::as_incidence_matrix(G, sparse = FALSE)}
    return(list(matrix = I, legislator = legislator, bills = bills))
  }

  if (format == "igraph") {
    G <- igraph::graph_from_data_frame(sponsorship, directed = F)
    igraph::V(G)$type <- igraph::V(G)$name %in% sponsorship[,2] #second column of edges is TRUE type
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F)]$party <- legislator$party)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F)]$state <- legislator$state)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F)]$last <- legislator$last)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F)]$id <- legislator$id)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$introduced <- bills$introduced)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$title <- bills$title)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$area <- bills$area)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$status <- bills$status)
    return(G)
  }

}
