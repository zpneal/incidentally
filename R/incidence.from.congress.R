#' Generate bill sponsorship incidence matrices and bipartite graphs
#'
#' `incidence.from.congress()` uses data from \href{https://www.congress.gov/}{https://www.congress.gov/} to construct an incidence
#'    matrix or bipartite graph recording legislators' bill (co-)sponsorships.
#'
#' @param session numeric: the session of congress (currently 108-117)
#' @param types vector: types of bills to include. May be any combination of c(\"s\", \"sres\", \"sjres\", \"sconres\") OR any combination of c(\"hr\", \"hres\", \"hjres\", \"hconres\").
#' @param sponsors boolean: should sponsors be distinguished from co-sponsors using edge weights
#' @param names boolean: should legislators be identified by name (TRUE), or by bioguideId (FALSE)
#' @param format string: format of output, one of c(\"data\", \"igraph\")
#'
#' @return
#' If `format = "data"`, a list containing an incidence matrix, a dataframe of legislator characteristics, and a dataframe of bill characteristics.
#'
#' If `format = "igraph"`, a bipartite igraph object composed of legislator vertices and bill vertices, each with vertex attributes.
#'
#' @export
incidence.from.congress <- function(session = NULL, types = NULL, sponsors = FALSE, names = TRUE, format = "data"){

  #Parameter check
  if (!is.numeric(session)) {stop("session must be an integer between 108 and 117")}
  if (session%%1!=0) {stop("session must be an integer between 108 and 117")}
  if (session<108 | session>117) {stop("session must be an integer between 108 and 117")}
  if (!(all(types %in% c("s", "sres", "sjres", "sconres"))) & !(all(types %in% c("hr", "hres", "hjres", "hconres")))) {stop("types must be a combination of c(\"s\", \"sres\", \"sjres\", \"sconres\") OR a combination of c(\"hr\", \"hres\", \"hjres\", \"hconres\")")}
  if (!(format %in% c("data", "igraph"))) {stop("format must be one of c(\"data\", \"igraph\")")}

  #Initialize data
  dat <- data.frame(id = NULL, name = NULL, last = NULL, party = NULL, state = NULL, bill = NULL, title = NULL, area = NULL, status = NULL, weight = NULL)

  #Begin bill type loop
  for (type in types) {

    #Download zip of bill-type as tempfile, determine contents
    temp <- tempfile()
    message(paste0("Retriving ", type, " bills from session ", session))
    utils::download.file(paste0("https://www.govinfo.gov/bulkdata/BILLSTATUS/",session,"/",type,"/BILLSTATUS-",session,"-",type,".zip"),temp)  #Download file
    files <- utils::unzip(temp, list = TRUE)$Name  #Unzip and get list of XML contents
    number.of.bills <- length(files)
    message(paste0("Examining ", number.of.bills, " bills"))
    pb <- utils::txtProgressBar(min = 1, max = number.of.bills, style = 3)  #Initiate progress bar

    #Parse each bill in zip
    for (file in 1:number.of.bills) {
      bill <- xml2::read_xml(unz(temp, files[file]))

      #Bill characteristics
      number <- paste0(xml2::xml_text(xml2::xml_find_first(bill, ".//billType")),xml2::xml_text(xml2::xml_find_first(bill, ".//billNumber")))
      title <- xml2::xml_text(xml2::xml_find_first(bill, ".//title"))
      area <- xml2::xml_text(xml2::xml_find_first(bill, ".//policyArea"))
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
      s.party <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//party"))
      s.state <- xml2::xml_text(xml2::xml_find_first(sponsor, ".//state"))

      #Co-sponsors
      cosponsor <- xml2::xml_find_first(bill, ".//cosponsors")
      cosponsor <- xml2::xml_find_all(cosponsor, ".//item")
      cs.id <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//bioguideId"))
      cs.name <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//fullName"))
      cs.last <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//lastName"))
      cs.party <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//party"))
      cs.state <- xml2::xml_text(xml2::xml_find_first(cosponsor, ".//state"))

      #Add to data
      dat <- rbind(dat, data.frame(id = s.id, name = s.name, last = s.last, party = s.party, state = s.state, bill = number, title = title, area = area, status = status, weight = 2))
      if (length(cs.id)>0) dat <- rbind(dat, data.frame(id = cs.id, name = cs.name, last = cs.last, party = cs.party, state = cs.state, bill = number, title = title, area = area, status = status, weight = 1))

      utils::setTxtProgressBar(pb, file)
    } #End file loop
    close(pb)
  } #End type loop

  #Prep sponsorship data and codebooks
  if (sponsors & names) {sponsorship <- dat[c("name", "bill", "weight")]}
  if (sponsors & !names) {sponsorship <- dat[c("id", "bill", "weight")]}
  if (!sponsors & names) {sponsorship <- dat[c("name", "bill")]}
  if (!sponsors & !names) {sponsorship <- dat[c("id", "bill")]}
  legislator <- unique(dat[c("id", "name", "last", "party", "state")])
  bills <- unique(dat[c("bill", "title", "area", "status")])

  #Construct incidence matrix and codebooks
  if (format == "data") {
    G <- igraph::graph_from_data_frame(sponsorship, directed = F)
    igraph::V(G)$type <- igraph::V(G)$name %in% sponsorship[,2] #second column of edges is TRUE type
    if (sponsors) {I <- igraph::as_incidence_matrix(G, attr = "weight", sparse = FALSE)} else {I <- igraph::as_incidence_matrix(G, sparse = FALSE)}
    return(list(matrix = I, legislator = legislator, bills = bills))
  }

  if (format == "igraph") {
    G <- igraph::graph_from_data_frame(sponsorship, directed = F)
    igraph::V(G)$type <- igraph::V(G)$name %in% sponsorship[,2] #second column of edges is TRUE type
    igraph::V(G)[which(igraph::V(G)$type==F)]$party <- legislator$party
    igraph::V(G)[which(igraph::V(G)$type==F)]$state <- legislator$state
    igraph::V(G)[which(igraph::V(G)$type==F)]$last <- legislator$last
    if (names) {igraph::V(G)[which(igraph::V(G)$type==F)]$id <- legislator$id}
    if (!names) {igraph::V(G)[which(igraph::V(G)$type==F)]$name <- legislator$name}
    igraph::V(G)[which(igraph::V(G)$type==T)]$title <- bills$title
    igraph::V(G)[which(igraph::V(G)$type==T)]$area <- bills$area
    igraph::V(G)[which(igraph::V(G)$type==T)]$status <- bills$status
    return(G)
  }

}
