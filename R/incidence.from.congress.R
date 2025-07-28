#' Generate bill sponsorship incidence matrices and bipartite graphs
#'
#' `incidence.from.congress()` uses data from \href{https://www.congress.gov/}{https://www.congress.gov/} to construct an incidence
#'    matrix or bipartite graph recording legislators' bill (co-)sponsorships.
#'
#' @param session numeric: the session of congress
#' @param types vector: types of bills to include. May be any combination of c("s", "sres", "sjres", "sconres") OR any combination of c("hr", "hres", "hjres", "hconres").
#' @param areas vector: policy areas of bills to include (see details)
#' @param nonvoting boolean: should non-voting members be included
#' @param weighted boolean: should sponsor-bill edges have a weight of 2, but cosponsor-bill edges have a weight of 1
#' @param format string: format of output, one of c("data", "igraph")
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
#' In the House of Representatives, legislators can introduce four types of bills: a House Bill (hr), a House Joint Resolution (hjres),
#'    a House Concurrent Resolution (hconres), and a House Simple Resolution (hres). Similarly, in the Senate, legislators can introduce
#'    four types of bills: a Senate Bill (s), a Senate Joint Resolution (sjres), a Senate Concurrent Resolution (sconres), and a Senate
#'    Simple Resolution (sres). In both chambers, concurrent and simple resolutions are used for minor procedural matters and do not
#'    have the force of law. Only bills and joint resolutions require the President's signature and have the force of law if signed.
#'
#' Each bill is assigned a policy area by the Congressional Research Service. By default, bills from all policy areas are included,
#'    however the `areas` parameter can be used to include only bills addressing certain policy areas. The `areas` takes a vector of
#'    strings listing the desired policy areas (e.g., `areas = c("Congress", "Animals")`). A complete list of policy areas and brief
#'    descriptions is available at \href{https://www.congress.gov/help/field-values/policy-area}{https://www.congress.gov/help/field-values/policy-area}.
#'
#' @return
#' If `format = "data"`, a list containing an incidence matrix, a dataframe of legislator characteristics, and a dataframe of bill characteristics.
#'
#' If `format = "igraph"`, a bipartite igraph object composed of legislator vertices and bill vertices, each with vertex attributes.
#'
#' For both formats, legislator characteristics include: BioGuide ID, full name, last name, party affiliation, and state. Bill characteristics
#'     include: bill ID, introduction date, title, policy area, status, sponsor's party, and number of co-sponsors from each party.
#'
#' @references {Neal, Z. P. 2022. Constructing legislative networks in R using incidentally and backbone. *Connections, 42*. \doi{10.2478/connections-2019.026}}
#' @references {Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. *CRAN* \doi{10.32614/CRAN.package.incidentally}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' D <- incidence.from.congress(session = 116, types = "s", format = "data")
#' D <- incidence.from.congress(session = 116, types = "s", format = "data",
#'      areas = c("animals", "health"))
#' G <- incidence.from.congress(session = 115, types = c("hr", "hres"), format = "igraph")
#' }
incidence.from.congress <- function(session = NULL, types = NULL, areas = "all", nonvoting = FALSE, weighted = FALSE, format = "data", narrative = FALSE){

  #Parameter check
  if (!is.numeric(session)) {stop("session must be an integer")}
  if (session%%1!=0) {stop("session must be an integer")}
  if (!(all(types %in% c("s", "sres", "sjres", "sconres"))) & !(all(types %in% c("hr", "hres", "hjres", "hconres")))) {stop("types must be a combination of c(\"s\", \"sres\", \"sjres\", \"sconres\") OR a combination of c(\"hr\", \"hres\", \"hjres\", \"hconres\")")}
  if (!(format %in% c("data", "igraph"))) {stop("format must be one of c(\"data\", \"igraph\")")}
  if (length(areas) > 1 & "all" %in% areas) {stop("areas may be *either* \"all\" or a vector of specific areas")}
  areas <- tolower(areas)

  #Initialize data as an empty list, to which rows will be appended
  dat <- list()

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
      area <- tolower(xml2::xml_text(xml2::xml_find_all(bill, "bill/policyArea/name")))
      if (length(area) == 0) {area <- NA}  #Some bills don't have an area (e.g. private legislation)
      if (areas[1]=="all" | area %in% areas) {

      #Bill characteristics

      #Old XML tags (https://github.com/usgpo/bill-status/issues/200)
      number <- paste0(xml2::xml_text(xml2::xml_find_all(bill, "bill/billType")), xml2::xml_text(xml2::xml_find_all(bill, "bill/billNumber")))
      if (length(number) == 0) {number <- paste0(xml2::xml_text(xml2::xml_find_all(bill, "bill/type")), xml2::xml_text(xml2::xml_find_all(bill, "bill/number")))}

      introduced <- tolower(xml2::xml_text(xml2::xml_find_all(bill, "bill/introducedDate")))
      title <- xml2::xml_text(xml2::xml_find_all(bill, "bill/title"))
      status <- xml2::xml_text(xml2::xml_find_all(bill, "bill/latestAction/text"))

      #Sponsors
      s.id <- xml2::xml_text(xml2::xml_find_all(bill, "bill/sponsors/item/bioguideId"))
      s.name <- xml2::xml_text(xml2::xml_find_all(bill, "bill/sponsors/item/fullName"))
      s.last <- xml2::xml_text(xml2::xml_find_all(bill, "bill/sponsors/item/lastName"))
      s.last <- paste(toupper(substr(s.last, 1, 1)), substr(s.last, 2, nchar(s.last)), sep="")
      s.party <- xml2::xml_text(xml2::xml_find_all(bill, "bill/sponsors/item/party"))
      s.state <- xml2::xml_text(xml2::xml_find_all(bill, "bill/sponsors/item/state"))

      #Coponsors
      c.id <- xml2::xml_text(xml2::xml_find_all(bill, "bill/cosponsors/item/bioguideId"))
      c.name <- xml2::xml_text(xml2::xml_find_all(bill, "bill/cosponsors/item/fullName"))
      c.last <- xml2::xml_text(xml2::xml_find_all(bill, "bill/cosponsors/item/lastName"))
      c.last <- paste(toupper(substr(c.last, 1, 1)), substr(c.last, 2, nchar(c.last)), sep="")
      c.party <- xml2::xml_text(xml2::xml_find_all(bill, "bill/cosponsors/item/party"))
      c.state <- xml2::xml_text(xml2::xml_find_all(bill, "bill/cosponsors/item/state"))

      #Compute partisanship
      partisan <- NA
      if (length(c.party)>0) {partisan <- sum(c.party == s.party[1]) / length(c.party)}

      #Add to data, each bill sponsor and each bill co-sponsor set becomes a new row in a growing list
      if (length(s.id)>0) {dat[[length(dat)+1]] <- data.frame(id = s.id, name = s.name, last = s.last, party = s.party, state = s.state, bill = number, introduced = introduced, title = title, area = area, sponsor.party = s.party[1], partisan = partisan, status = status, weight = 2)}
      if (length(c.id)>0) {dat[[length(dat)+1]] <- data.frame(id = c.id, name = c.name, last = c.last, party = c.party, state = c.state, bill = number, introduced = introduced, title = title, area = area, sponsor.party = s.party[1], partisan = partisan, status = status, weight = 1)}
      }

      utils::setTxtProgressBar(pb, file)
    } #End file loop
    close(pb)
  } #End type loop

  #Clean up data
  dat <- do.call(rbind,dat)  #Convert data stored as list into data frame
  dat <- unique(dat)  #Remove duplicate rows, in rare cases when a sponsor or co-sponsor was listed twice
  if (!nonvoting) {dat <- dat[which(dat$state!="AS" & dat$state!="DC" & dat$state!="GU" & dat$state!="MP" & dat$state!="PR" & dat$state!="VI"),]}

  legislator <- dat[c("id", "name", "last", "party", "state")]  #Get preliminary legislator data
  legislator <- legislator[!duplicated(legislator$id), ]  #Keep only one record per Bioguide ID (necessary if legislator changed parties or name spelling)
  dat <- dat[, -match(c("name", "last", "party", "state"), names(dat))]  #Remove old legislator data from raw dat
  dat <- merge(dat, legislator, by = "id")  #Insert new legislator data

  #Prep sponsorship data and codebooks
  if (weighted) {sponsorship <- dat[c("name", "bill", "weight")]} else {sponsorship <- dat[c("name", "bill")]}
  legislator <- unique(dat[c("id", "name", "last", "party", "state")])
  bills <- unique(dat[c("bill", "introduced", "title", "area", "sponsor.party", "partisan", "status")])

  #Display narrative if requested
  if (narrative) {
    version <- utils::packageVersion("incidentally")
    if (all(types %in% c("s", "sres", "sjres", "sconres"))) {who <- "Senators'"}
    if (all(types %in% c("hr", "hres", "hjres", "hconres"))) {who <- "Representatives'"}
    if (format == "igraph" & "all" %in% areas) {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a bipartite graph recording ", who, " bill sponsorships during the ", session, " session of the US Congress.")}
    if (format == "igraph" & !("all" %in% areas)) {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate a bipartite graph recording ", who, " bill sponsorships during the ", session, " session of the US Congress. We restricted our focus to bills in the following policy areas: ", paste(areas, collapse=', '), ".")}
    if (format == "data" & "all" %in% areas) {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate an incidence matrix recording ", who, " bill sponsorships during the ", session, " session of the US Congress.")}
    if (format == "data" & !("all" %in% areas)) {text <- paste0("We used the incidentally package for R (v", version, "; Neal, 2022) to generate an incidence matrix recording ", who, " bill sponsorships during the ", session, " session of the US Congress. We restricted our focus to bills in the following policy areas: ", paste(areas, collapse=', '), ".")}
    message("")
    message("=== Suggested manuscript text and citations ===")
    message(text)
    message("")
    message("Neal, Z. P. 2022. Constructing legislative networks in R using incidentally and backbone. Connections, 42. https://doi.org/10.2478/connections-2019.026")
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
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F)]$color <- grDevices::rgb(0,0,1))
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F & igraph::V(G)$party=="R")]$color <- grDevices::rgb(1,0,0))
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==F & igraph::V(G)$party=="I")]$color <- grDevices::rgb(0,1,0))
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$introduced <- bills$introduced)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$title <- bills$title)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$area <- bills$area)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$sponsor.party <- bills$sponsor.party)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$color <- grDevices::rgb(0,0,1))
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T & igraph::V(G)$sponsor.party=="R")]$color <- grDevices::rgb(1,0,0))
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T & igraph::V(G)$sponsor.party=="I")]$color <- grDevices::rgb(0,1,0))
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$partisan <- bills$partisan)
    suppressWarnings(igraph::V(G)[which(igraph::V(G)$type==T)]$status <- bills$status)
    return(G)
  }

}
