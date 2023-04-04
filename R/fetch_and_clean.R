#' Fetch and clean search results
#'
#' @description
#' Fetches and cleans search results from PubMed as a dataframe. This is used by
#' the main web_scrapper() function.
#' If 0 search result, return an empty dataframe.
#'
#' @param keyword Required. A string value.
#' @param field The field to search for the keywords. Currently support:
#'   `abstract`, `all`
#'   Some databases may not support some field to be used in the search.
#' @param db A string of one database name. Default to `sage_journal`.
#'   Currently support: `sage_journal`, `science_direct`, `pubmed`, `proquest`
#'   If both correct and wrong database names are specified, only the correct ones
#'   will be queried.
#' @param subdb_proquest A string or a vector of sub databases in proquest
#'   to query.
#' @param sdkey API key for science direct.
#' @param no_duplicate Whether to remove duplicated results (TRUE) or not (FALSE).
#'   One article could be returned by multiple databases, if you specified multiple
#'   databases to search in `database_name`.
#' @param limit_per_search The maximum number of results collected from each
#'   database.
#'
#' @export

fetch_and_clean <- function(keyword, field, db, subdb_proquest, sdkey, no_duplicate, limit_per_search = NULL,
                            start_year = NULL, end_year = NULL, additional_args = list()) {
  closeAllConnections()

  keyword_original <- paste(keyword, collapse = " AND ") # concatenate keywords if multiple
  keyword_encoded <- encode_keyword(keyword, db, field)


  if (db == "sage_journal") {
    maxsize_db <- 100

    url_sj <- get_url(keyword_encoded, field, db, start_year = start_year, end_year = end_year, additional_args = additional_args)
    print(url_sj)
    download.file(url = url_sj, destfile = "scrapedpage.html", quiet = TRUE)
    page <- xml2::read_html("scrapedpage.html")
    closeAllConnections()

    n <- page %>%
      rvest::html_element("span.result__count") %>%
      rvest::html_text() %>%
      # str_sub(2, -2) %>% # delete parenthese
      as.numeric()

    if (n > 0) {
      df_db <- parse_res_sj(url_sj, page, n, limit_per_search) %>%
        dplyr::mutate(search_term = keyword_original, database = db)
      if (no_duplicate) {
        title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp), ]
      }
    } else {
      print(paste("Total results: 0"))
      df_db <- data.frame()
    }
  } else if (db == "science_direct") {
    maxsize_db <- 200

    if (sdkey == "") {
      sdkey <- readkey()
    }

    url_sd <- get_url(keyword_encoded, field, db, api_key = sdkey, start_year = start_year, end_year = end_year, additional_args = additional_args)

    page <- xml2::read_html(url_sd)
    n <- page %>%
      rvest::html_nodes("totalresults") %>%
      rvest::html_text() %>%
      as.numeric()

    if (n > 0) {
      df_db <- parse_res_sd(page, n, limit_per_search) %>%
        dplyr::mutate(search_term = keyword_original, database = db)
      if (no_duplicate) {
        title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp), ]
      }
    } else {
      print(paste("Total results: 0"))
      df_db <- data.frame()
    }
  } else if (db == "pubmed") {
    url_pm <- get_url(keyword_encoded, field, db, start_year = start_year, end_year = end_year, additional_args = additional_args)

    res <- httr::GET(url_pm) %>% httr::content()
    n <- res$esearchresult$count %>% as.numeric()

    if (n > 0) {
      df_db <- parse_res_pm(res, n, limit_per_search) %>%
        dplyr::mutate(search_term = keyword_original, database = db)
      if (no_duplicate) {
        title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp), ]
      }
    } else {
      print(paste("Total results: 0"))
      df_db <- data.frame()
    }
  } else if (db == "proquest") {
    df_db <- data.frame(stringsAsFactors = F)
    count_subdb <- 0
    for (subdb in subdb_proquest) {
      count_subdb <- count_subdb + 1
      print(paste("Current subdatabase: ", subdb, " ", count_subdb, "/", length(subdb_proquest), sep = ""))

      url_pq <- get_url(keyword_encoded, field, db, subdb = subdb, start_year = start_year, end_year = end_year, additional_args = additional_args)
      print(url_pq)
      page_pq <- xml2::read_html(url_pq)
      n <- page_pq %>%
        rvest::html_nodes("numberofrecords") %>%
        rvest::html_text() %>%
        as.numeric()

      if (n > 0) {
        df_subdb <- parse_res_pq(url_pq, page_pq, n, limit_per_search) %>%
          dplyr::mutate(search_term = keyword_original, database = db)
      } else {
        print("Total results: 0")
        df_subdb <- data.frame()
      }

      df_db <- dplyr::bind_rows(df_db, df_subdb)
    }

    if (no_duplicate) {
      title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
      df_db <- df_db[!duplicated(title_tmp), ]
    }
  }

  return(df_db)
}
