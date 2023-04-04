#' Read API key for Science Direct
#'
#' @description
#' Read API key for Science Direct
#'
#' @param page the resulting webpage object returned by rvest
#' @param n the number of records to keep
#' @param limit_per_search the number of maximum records to keep
#' @param type ?
#'
#' @noRd
readkey <- function() {
  key <- readline(prompt = "Please enter your API key for Science Direct (if you don't have one, get it from here: https://dev.elsevier.com/user/login): ")
  while (nchar(stringr::str_trim(key)) < 25) {
    print("The length of key seems wrong")
    key <- readline(prompt = "Please enter your API key for Science Direct (if you don't have one, get it from here: https://dev.elsevier.com/user/login): ")
  }
  return(key)
}

l_field <- list(
  "sage_journal" = list(
    "abstract" = "abstract",
    "all" = "allfield"
  ),
  "science_direct" = list(
    "abstract" = "tak",
    "all" = "all"
  ),
  "pubmed" = list(
    "abstract" = "[title/abstract]",
    "all" = ""
  ),
  "proquest" = list(
    "abstract" = "abstract%3D",
    "all" = ""
  )
)


#' Encode keywords. For keywords that have special character such as space or hyphen.
#' @param keyword A string of one keyword, or a vector of keywords. When a vector
#'   of multiple keywords are provided, this is a relationship="and" case.
encode_keyword <- function(keyword, db, field) {
  keyword <- lapply(keyword, stringr::str_trim) %>% unlist() # trim spaces
  # if (db %in% c('pubmed')){
  #   keyword <- paste0(keyword,l_field[[db]][[field]])
  # }

  if (db %in% c("sage_journal", "pubmed")) {
    res <- lapply(keyword, function(x) {
      stringr::str_replace_all(x, " ", "+")
    }) %>% unlist() # %>%
    # paste(collapse = '+AND+')
  } else {
    res <- lapply(keyword, URLencode) %>% unlist() # %>%
    # paste(collapse = '%20AND%20')
  }

  return(res)
}

#' @param keyword A string of one keyword, or a vector of keywords. When a vector
#'   of multiple keywords are provided, this is a relationship="and" case, and
#'   the request asks for the result where ALL keywords appear in the same article.
#' @param api_key A parameter only used by science direct.
#' @param subdb A parameter only used by proquest.
get_url <- function(keyword, field, db, api_key = NULL, subdb = NULL, start_record = NULL,
                    start_year = NULL, end_year = NULL, additional_args = list(), verbose = 0) {
  field_name <- l_field[[db]][[field]]

  additional_args <- paste(names(additional_args), unname(unlist(additional_args)), sep = "=") %>%
    paste(collapse = "&")

  if (db == "sage_journal") {
    res <- glue::glue("http://journals.sagepub.com/action/doSearch?pageSize=100&{additional_args}")

    if (!is.null(start_year)) {
      res <- glue::glue("{res}&AfterYear={start_year}")
    }

    if (!is.null(end_year)) {
      res <- glue::glue("{res}&BeforeYear={end_year}")
    }

    for (i in 1:length(keyword)) {
      res <- glue::glue("{res}&field{i}={field_name}&text{i}={keyword[i]}")
    }

    res <- glue::glue("{res}&startPage=0") # the first page is startPage=0
  } else if (db == "science_direct") {
    query <- glue::glue("{field_name}({keyword})")
    res <- glue::glue("http://api.elsevier.com/content/search/scidir?apiKey={api_key}&{additional_args}&query={paste(query,collapse='%20AND%20')}&subscribed=true&oa=true&content=journals&count={maxsize_db}&httpaccept=application/xml&view=complete")
  } else if (db == "pubmed") {
    # pubmed ESearch API can only return the first 10k results
    # if more are needed, will need to switch to EDirect API: https://www.ncbi.nlm.nih.gov/books/NBK25499/
    # retmode can be either xml or json - json is easier for parsing

    start_year <- ifelse(is.null(start_year), 1000, start_year)
    end_year <- ifelse(is.null(end_year), lubridate::year(Sys.Date()), end_year)

    query <- glue::glue("{keyword}{field_name}")
    res <- glue::glue("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&{additional_args}&term={paste(query,collapse='+AND+')}&mindate={start_year}&maxdate={end_year}&retmax=10000&sort=relevance&retmode=json")
  } else if (db == "proquest") {
    query <- paste(keyword, collapse = "%20AND%20")
    res <- glue::glue('http://fedsearch.proquest.com/search/sru/{subdb}?operation=searchRetrieve&version=1.2&maximumRecords=10000&{additional_args}&query={field_name}"{query}"')
    res <- glue::glue('{res}&startRecord=1')
  }

  if (verbose > 0) {
    print(res)
  }

  return(res)
}


#' Get Proquest Sub-Daatabases
#' @description
#' This function retrieves all the subscribed Proquest Sub-Databases by sending a query request to
#' a non-existing sub-db and parsing the returned message where all subscriptions are mentioned.
#'
#' @export

get_proquest_subdb <- function() {
  url_pq <- "https://fedsearch.proquest.com/search/sru/x?operation=searchRetrieve&version=1.2&maximumRecords=10000&&query=abstract%3D%22x%22"
  page_pq <- xml2::read_html(url_pq)
  res <- page_pq %>%
    rvest::html_element("message") %>%
    rvest::html_text() %>%
    sub("^[^,]+-\\s+([^,]+).*; ", "\\1", .) %>%
    stringr::str_split_1(" ")
  print(glue::glue("{length(res)} subdatabases found"))
  return(res)
}
