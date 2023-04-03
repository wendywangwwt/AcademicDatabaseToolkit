#' Fetch and clean search results
#'
#' @description
#' Fetchs and cleans search results from PubMed as a dataframe. This is used by
#' the main web_scrapper() function.
#'
#' @param keyword Required. A string value.
#' @param field The field to search for the keywords. Currently support:
#'   `abstract`, `all`
#'   Some databases may not support some field to be used in the search.
#' @param db A string of one database name. Default to `sage_journal`.
#'   Currently support: `sage_journal`, `science_direct`, `pubmed`, `proquest`
#'   If both correct and wrong database names are specified, only the correct ones
#'   will be queried.
#' @param sdkey API key for science direct.
#' @param no_duplicate Whether to remove duplicated results (TRUE) or not (FALSE).
#'   One article could be returned by multiple databases, if you specified multiple
#'   databases to search in `database_name`.
#' @param limit_per_search The maximum number of results collected from each
#'   database.
#'
#' @export

fetch_and_clean <- function(keyword, field, db, sdkey, no_duplicate, limit_per_search=NULL,
                            start_year=NULL, end_year=NULL, additional_args=list()) {
  closeAllConnections()

  keyword_original <- paste(keyword, collapse = " AND ") # concatenate keywords if multiple
  keyword_encoded <- encode_keyword(keyword, db, field)


  if (db == "sage_journal") {
    maxsize_db <- 100

    url_sj <- get_url(keyword_encoded, field, db, start_year=start_year, end_year=end_year, additional_args=additional_args)
    print(url_sj)
    download.file(url = url_sj, destfile = "scrapedpage.html", quiet = TRUE)
    page <- read_html("scrapedpage.html")
    closeAllConnections()

    n <- page %>%
      html_element('span.result__count') %>%
      html_text() %>%
      # str_sub(2, -2) %>% # delete parenthese
      as.numeric()

    if (length(n) == 0) {
      print(paste("Total results: 0"))
      next
    } else {
      df_db <- parse_res_sj(url_sj, page, n, limit_per_search) %>%
        mutate(search_term = keyword_original, database = db)
      if (no_duplicate) {
        title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp), ]
      }
    }
  } else if (db == "science_direct") {
    maxsize_db <- 200

    if (sdkey == "") {
      sdkey <- readkey()
    }

    url_sd <- get_url(keyword_encoded, field, db, api_key = sdkey, start_year=start_year, end_year=end_year, additional_args=additional_args)

    page <- read_html(url_sd)
    n <- page %>%
      html_nodes("totalresults") %>%
      html_text() %>%
      as.numeric()

    if (n > 0) {
      df_db <- parse_res_sd(page, n, limit_per_search) %>%
        mutate(search_term = keyword_original, database = db)
      if (no_duplicate) {
        title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp), ]
      }
    } else {
      print(paste("Total results: 0"))
      next
    }
  } else if (db == "pubmed") {
    url_pm <- get_url(keyword_encoded, field, db, start_year=start_year, end_year=end_year, additional_args=additional_args)

    res <- httr::GET(url_pm) %>% httr::content()
    n <- res$esearchresult$count %>% as.numeric()

    if (n > 0) {
      df_db <- parse_res_pm(res, n, limit_per_search) %>%
        mutate(search_term = keyword_original, database = db)
      if (no_duplicate) {
        title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
        df_db <- df_db[!duplicated(title_tmp), ]
      }
    } else {
      print(paste("Total results: 0"))
      next
    }
  } else if (db == "proquest") {
    df_db <- data.frame(stringsAsFactors = F)
    count_subdb <- 0
    for (subdb in subdb_pq) {
      count_subdb <- count_subdb + 1
      print(paste("Current subdatabase: ", subdb, " ", count_subdb, "/", length(subdb_pq), sep = ""))

      url_pq <- get_url(keyword, field, db, start_year=start_year, end_year=end_year, additional_args=additional_args)
      print(url_pq)
      page_pq <- read_html(url_pq)
      n <- page_pq %>%
        html_nodes("numberofrecords") %>%
        html_text() %>%
        as.numeric()

      type <- page_pq %>%
        html_nodes('[tag="513"]') %>%
        html_text() %>%
        tolower()

      count_done <- 0
      if (n > 0 & n > length(type)) { # in some examples the server only returns 1000 records per request
        num_page <- ceiling(n / length(type))
        print(paste0("Total pages: ", num_page))
        df_subdb <- data.frame()
        for (i in 1:num_page) {
          print(paste0("Current page: ", i, "/", num_page))

          url_pq_cur <- get_url(keyword, field, db, start_record = length(type) * (i - 1) + 1, start_year=start_year, end_year=end_year, additional_args=additional_args)

          page_pq_cur <- read_html(url_pq_cur)
          type_cur <- page_pq_cur %>%
            html_nodes('[tag="513"]') %>%
            html_text() %>%
            tolower()
          type_cur <- str_detect(type_cur, "journal|feature|article|case|study|periodical|literature|statistics")

          if (count_done + sum(type_cur) <= limit_per_search) {
            count_done <- count_done + sum(type_cur)
            df_subdb_cur <- parse_res_pq(page_pq_cur, sum(type_cur), limit_per_search, type_cur) %>%
              mutate(search_term = keyword_original, database = db)
            df_subdb <- rbind(df_subdb, df_subdb_cur)
          } else {
            df_subdb_cur <- parse_res_pq(page_pq_cur, sum(type_cur), limit_per_search - count_done, type_cur) %>%
              mutate(search_term = keyword_original, database = db)
            df_subdb <- rbind(df_subdb, df_subdb_cur)
            print("Finish collecting required amount of records.")
            break
          }
        }
      } else {
        type <- str_detect(type, "journal|feature|article|case|study|periodical|literature|statistics")

        n <- length(which(type))

        if (n > 0) {
          df_subdb <- parse_res_pq(page_pq, n, limit_per_search, type) %>%
            mutate(search_term = keyword_original, database = db)
        } else {
          print("Total results: 0")
          next
        }
      }

      df_db <- rbind(df_db, df_subdb)
    }

    if (no_duplicate) {
      title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_db$title))
      df_db <- df_db[!duplicated(title_tmp), ]
    }
  }

  return(df_db)
}
