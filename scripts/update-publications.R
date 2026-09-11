#!/usr/bin/env Rscript

# Update publications.yml from PubMed.
#
# Usage:  Rscript scripts/update-publications.R
#
# Searches PubMed with the query in _publications-config.yml, fetches the
# matching records and merges them into publications.yml.
#
# Fields owned by the script, refreshed on every run:
#   pmid, title, author, journal, journal_full, year, date, volume, issue,
#   pages, doi
#
# Fields owned by you, never touched:
#   featured, links, description, categories, and anything else you add
#
# Entries you write by hand (preprints, book chapters, software) survive as
# long as no PubMed record matches them by PMID or DOI.

suppressPackageStartupMessages({
  library(httr2)
  library(xml2)
  library(yaml)
})

CONFIG_FILE <- "_publications-config.yml"
OUT_FILE    <- "publications.yml"
EUTILS      <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

# Fields regenerated from PubMed; anything else on an existing entry is kept.
PUBMED_FIELDS <- c("pmid", "title", "author", "journal", "journal_full",
                   "year", "date", "volume", "issue", "pages", "doi")

msg <- function(...) cat(..., "\n", sep = "")

# ---------------------------------------------------------------- config ----

if (!file.exists(CONFIG_FILE)) {
  stop("Missing ", CONFIG_FILE, " (run this from the project root)", call. = FALSE)
}
cfg <- yaml::read_yaml(CONFIG_FILE)

api_key <- Sys.getenv("NCBI_API_KEY", unset = "")
if (!nzchar(api_key)) api_key <- cfg$api_key %||% ""

exclude <- as.character(unlist(cfg$exclude_pmids %||% character()))
extra   <- as.character(unlist(cfg$extra_pmids %||% character()))

# ------------------------------------------------------------ e-utilities ----

eutils_req <- function(endpoint, params) {
  params <- c(params, list(tool = "dhammarstrom-website", email = cfg$email %||% ""))
  if (nzchar(api_key)) params$api_key <- api_key
  req <- request(paste0(EUTILS, "/", endpoint))
  req <- do.call(req_url_query, c(list(req), params))
  req |>
    req_user_agent("dhammarstrom.github.io publication updater") |>
    req_retry(max_tries = 3) |>
    req_throttle(capacity = if (nzchar(api_key)) 8 else 2, fill_time_s = 1)
}

esearch <- function(query, retmax) {
  resp <- eutils_req("esearch.fcgi", list(
    db = "pubmed", term = query, retmax = retmax, retmode = "json"
  )) |> req_perform() |> resp_body_json()
  as.character(unlist(resp$esearchresult$idlist))
}

efetch <- function(pmids) {
  if (!length(pmids)) return(list())
  out <- list()
  for (chunk in split(pmids, ceiling(seq_along(pmids) / 150))) {
    xml <- eutils_req("efetch.fcgi", list(
      db = "pubmed", id = paste(chunk, collapse = ","), retmode = "xml"
    )) |> req_perform() |> resp_body_xml()
    out <- c(out, as.list(xml_find_all(xml, "//PubmedArticle")))
  }
  out
}

# ---------------------------------------------------------------- parsing ----

txt <- function(node, xpath) {
  hit <- xml_find_first(node, xpath)
  if (inherits(hit, "xml_missing")) return(NULL)
  value <- trimws(xml_text(hit))
  if (nzchar(value)) value else NULL
}

parse_authors <- function(article) {
  nodes <- xml_find_all(article, ".//AuthorList/Author")
  if (!length(nodes)) return(character())
  names <- vapply(nodes, function(a) {
    collective <- txt(a, "./CollectiveName")
    if (!is.null(collective)) return(collective)
    last <- txt(a, "./LastName") %||% ""
    init <- txt(a, "./Initials") %||% ""
    trimws(paste(last, init))
  }, character(1))
  names[nzchar(names)]
}

# PubMed dates arrive in several shapes. Prefer the electronic ArticleDate,
# fall back to the journal issue date, and pad a missing month or day so the
# result always sorts correctly.
parse_date <- function(article) {
  months <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
              Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
  pick <- function(xpath) {
    y <- txt(article, paste0(xpath, "/Year"))
    if (is.null(y)) return(NULL)
    m <- txt(article, paste0(xpath, "/Month")) %||% "1"
    d <- txt(article, paste0(xpath, "/Day")) %||% "1"
    if (m %in% names(months)) m <- months[[m]]
    m <- suppressWarnings(as.integer(m)); if (is.na(m)) m <- 1L
    d <- suppressWarnings(as.integer(d)); if (is.na(d)) d <- 1L
    sprintf("%s-%02d-%02d", y, m, d)
  }
  date <- pick(".//ArticleDate") %||% pick(".//Journal/JournalIssue/PubDate")
  if (is.null(date)) {
    # MedlineDate, e.g. "2023 Nov-Dec" or "2023 Winter"
    medline <- txt(article, ".//Journal/JournalIssue/PubDate/MedlineDate")
    if (!is.null(medline)) {
      year <- regmatches(medline, regexpr("[0-9]{4}", medline))
      if (length(year)) date <- paste0(year, "-01-01")
    }
  }
  date
}

parse_article <- function(node) {
  entry <- list()
  entry$pmid   <- txt(node, ".//MedlineCitation/PMID")
  entry$title  <- sub("\\.$", "", txt(node, ".//Article/ArticleTitle") %||% "Untitled")
  entry$author <- parse_authors(node)

  entry$journal      <- txt(node, ".//Journal/ISOAbbreviation")
  entry$journal_full <- txt(node, ".//Journal/Title")
  if (is.null(entry$journal)) entry$journal <- entry$journal_full

  date <- parse_date(node)
  if (!is.null(date)) {
    entry$date <- date
    entry$year <- as.integer(substr(date, 1, 4))
  }

  entry$volume <- txt(node, ".//Journal/JournalIssue/Volume")
  entry$issue  <- txt(node, ".//Journal/JournalIssue/Issue")
  entry$pages  <- txt(node, ".//Pagination/MedlinePgn") %||%
                  txt(node, ".//Pagination/StartPage")
  entry$doi    <- txt(node, ".//ELocationID[@EIdType='doi']") %||%
                  txt(node, ".//ArticleIdList/ArticleId[@IdType='doi']")

  entry[!vapply(entry, is.null, logical(1))]
}

# ----------------------------------------------------------------- merge ----

key_of <- function(entry) {
  pmid <- entry$pmid %||% ""
  doi  <- entry$doi %||% ""
  if (nzchar(pmid)) paste0("pmid:", pmid)
  else if (nzchar(doi)) paste0("doi:", tolower(doi))
  else paste0("title:", tolower(entry$title %||% ""))
}

existing <- if (file.exists(OUT_FILE)) yaml::read_yaml(OUT_FILE) else list()
if (is.null(existing)) existing <- list()
if (length(existing)) names(existing) <- vapply(existing, key_of, character(1))

msg("Searching PubMed: ", cfg$query)
pmids <- esearch(cfg$query, cfg$retmax %||% 300)
pmids <- setdiff(unique(c(pmids, extra)), exclude)
msg("  ", length(pmids), " record(s) to fetch")

fetched <- lapply(efetch(pmids), parse_article)
if (length(fetched)) names(fetched) <- vapply(fetched, key_of, character(1))

merged   <- list()
n_new    <- 0L
n_update <- 0L

for (key in names(fetched)) {
  new <- fetched[[key]]
  old <- existing[[key]]
  if (is.null(old)) {
    new$featured <- FALSE
    new$links    <- list()
    merged[[key]] <- new
    n_new <- n_new + 1L
    msg("  + new: ", substr(new$title, 1, 70))
  } else {
    # Manual fields win; PubMed-derived fields are refreshed.
    merged[[key]] <- c(new, old[setdiff(names(old), PUBMED_FIELDS)])
    n_update <- n_update + 1L
  }
}

# Hand-written entries PubMed does not know about, minus anything since
# excluded by PMID.
n_manual <- 0L
for (key in setdiff(names(existing), names(fetched))) {
  old <- existing[[key]]
  if (!is.null(old$pmid) && old$pmid %in% exclude) {
    msg("  - excluded: ", substr(old$title %||% key, 1, 70))
    next
  }
  merged[[key]] <- old
  n_manual <- n_manual + 1L
}

# ------------------------------------------------------------------ write ----

sort_key <- vapply(merged, function(e) {
  e$date %||% paste0(e$year %||% "0000", "-01-01")
}, character(1))
merged <- merged[order(sort_key, decreasing = TRUE)]
names(merged) <- NULL

# Keep a stable key order so the file stays readable when hand-edited.
field_order <- c("title", "author", "journal", "journal_full", "year", "date",
                 "volume", "issue", "pages", "doi", "pmid", "featured", "links")
merged <- lapply(merged, function(e) {
  if (is.null(e$featured)) e$featured <- FALSE
  if (is.null(e$links))    e$links    <- list()
  e[c(intersect(field_order, names(e)), setdiff(names(e), field_order))]
})

header <- c(
  "# Publications listed on publications.qmd -- partly generated, safe to edit.",
  "#",
  "# Regenerate with:  Rscript scripts/update-publications.R",
  "#",
  "# The script refreshes the PubMed fields and preserves everything else, so",
  "# these are yours and they survive every run:",
  "#",
  "#   featured: true          moves the entry into the Featured section",
  "#   links:                  extra links shown under the entry",
  "#     - text: Blog post",
  "#       href: posts/good-science/index.html",
  "#       icon: journal-text  # any Bootstrap icon name",
  "#     - text: Data and code",
  "#       href: https://github.com/dhammarstrom/some-repo",
  "#       icon: github",
  "#",
  "# To drop a wrong hit, add its PMID to exclude_pmids in",
  "# _publications-config.yml -- deleting it here only brings it back.",
  ""
)

# Quarto parses YAML 1.2, where `yes`/`no` are strings rather than booleans,
# so emit true/false instead of the yaml package's default.
verbatim_logical <- function(x) {
  out <- ifelse(x, "true", "false")
  class(out) <- "verbatim"
  out
}

body <- yaml::as.yaml(merged, indent = 2,
                      handlers = list(logical = verbatim_logical))

con <- file(OUT_FILE, open = "wb")
writeLines(enc2utf8(c(header, body)), con, useBytes = TRUE)
close(con)

msg("")
msg("Wrote ", OUT_FILE, ": ", length(merged), " publication(s)")
msg("  ", n_new, " new, ", n_update, " refreshed, ", n_manual, " manual/unmatched")
