

#' Parse search results from PubMed
#'
#' @description
#' Parses search results from PubMed as a dataframe. This is used by the main
#' web_scrapper() function.
#'
#' @param res the resulting json content returned by httr::get
#' @param n the number of records to keep
#' @param limit_per_search the number of maximum records to keep
#'
#' @noRd

parse_res_pm <- function(res, n, limit_per_search) {
  maxsize_db <- 200
  limit_per_search <- min(n,limit_per_search) # make sure the number is valid, i.e., no larger than total

  n_total <- n # the total number of search results
  n_expected_to_return <- ifelse(!is.null(limit_per_search) & (limit_per_search < n), limit_per_search, n) # the possibly truncated number of results to return

  # PubMed provides all ids as the first search result
  # then we need to fetch the details using ids
  # this means we can easily customize the exact number of results returned
  num_page_total <- ifelse(n_total %% maxsize_db == 0, n_expected_to_return %/% maxsize_db, n_expected_to_return %/% maxsize_db + 1)
  if (is.null(limit_per_search)) {
    num_page <- num_page_total
  } else {
    num_page <- ifelse(limit_per_search %% maxsize_db == 0, limit_per_search %/% maxsize_db, limit_per_search %/% maxsize_db + 1)
  }


  print(glue::glue("Total pages: {num_page}"))

  info_extracted <- list(
    title = c(),
    link = c(),
    author = c(),
    abstract = c(),
    year = c(),
    availability = rep(NA,n_expected_to_return) # PubMed response does not have article availability info
  )

  count_done <- 0
  count_remain <- n_expected_to_return

  # the link for each paper is very easy to construct with the id
  # no need to look for this info in each batch of detailed results
  ids_total <- res$esearchresult$idlist %>% unlist()
  ids <- ids_total[1:n_expected_to_return]
  # info_extracted[["link"]] <- paste("https://www.ncbi.nlm.nih.gov/pubmed/", ids, sep = "")

  time_query <- Sys.time()
  for (k in 1:num_page) {
    # rate limit control; for non-apikey access, rate limit is 3 queries per sec
    while (Sys.time() - time_query < 0.5) {
      Sys.sleep(0.1)
    }
    time_query <- Sys.time()

    # initialize the info list for the current page
    info_extracted_cur <- list(
      title_cur = c(),
      author_cur = c(),
      abstract_cur = c(),
      year_cur = c()
    )

    idx_invalid <- c(1) # initialize with a random value, will be overwritten later if needed
    # check and update ids_total & ids to exclude invalid entries, such as retracted articles
    while (length(idx_invalid) > 0) {
      # fetch the page with details about a set of papers using a constructed url
      ids_cur <- if (k < num_page) ids[seq((1 + maxsize_db * (k - 1)), maxsize_db * k)] else ids[seq((1 + maxsize_db * (k - 1)), length(ids))]
      ids_cur_string <- paste(ids_cur, collapse = ",")

      # the EFetch API only supports xml output :(
      url_cur <- glue::glue("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={ids_cur_string}")
      page_cur <- xml2::read_html(url_cur)

      nodes <- page_cur %>%
        rvest::html_elements("pubmedarticleset") %>%
        rvest::html_children()

      # PubMed has this "pubmedbookarticle" type which is not a journal article
      # we need to basically identify such entry, remove it, and re-construct ids list
      article_type <- nodes %>%
        rvest::html_name()
      idx_invalid <- which(article_type != "pubmedarticle")

      # authorlist - there can be retraction article where the authorlist is empty
      count <- 0
      for (node in nodes){
        count <- count + 1
        author_list <- node %>% rvest::html_element('authorlist')
        if (length(author_list) == 0){
          idx_invalid <- c(idx_invalid, count)
        }
      }

      if (length(idx_invalid) > 0) {
        idx_invalid <- unique(idx_invalid) # to prevent both checks return the same id
        print(glue::glue("Encountered {length(idx_invalid)} non-journal articles. Removing and re-querying..."))

        for (idx_invalid_cur in idx_invalid){
          print(nodes[idx_invalid_cur] %>% rvest::html_element('pmid') %>% rvest::html_text())
          print(nodes[idx_invalid_cur] %>% rvest::html_element('articletitle') %>% rvest::html_text())
        }

        ids_total <- ids_total[-((k-1)*maxsize_db + idx_invalid)]
        ids <- ids_total[1:n_expected_to_return]


        while (Sys.time() - time_query < 0.5) {
          Sys.sleep(0.1)
        }
        time_query <- Sys.time()
      }
    }

    # link
    info_extracted[["link"]] <- paste("https://www.ncbi.nlm.nih.gov/pubmed/", ids, sep = "")

    # title
    info_extracted_cur[["title_cur"]] <- page_cur %>%
      rvest::html_nodes("articletitle") %>%
      rvest::html_text()

    # published year
    info_extracted_cur[["year_cur"]] <- page_cur %>%
      rvest::html_nodes('pubmedpubdate[pubstatus="pubmed"] year') %>%
      rvest::html_text() %>%
      as.numeric()

    nodes <- page_cur %>% rvest::html_elements('pubmedarticle')
    for (node in nodes){
      # author
      author_node <- node %>% rvest::html_elements('authorlist')
      if (length(author_node) > 0){
        author_names <- paste(author_node %>% rvest::html_nodes("forename") %>% rvest::html_text(), author_node %>% rvest::html_nodes("lastname") %>% rvest::html_text(), collapse = ", ")
      }else{
        author_names <- NA
      }
      info_extracted_cur[["author_cur"]] <- c(info_extracted_cur[["author_cur"]],author_names)

      # abstract
      abstract_node <- node %>% rvest::html_elements('abstract')
      if (length(abstract_node) > 0){
        abstract <- abstract_node %>% rvest::html_text()
      }else{
        abstract <- NA
      }
      info_extracted_cur[["abstract_cur"]] <- c(info_extracted_cur[["abstract_cur"]],abstract)
    }


    # assert 1: different types of info should have the same length
    n_unique_length <- lapply(info_extracted_cur, length) %>%
      unlist() %>%
      unique() %>%
      length()
    assertthat::assert_that(n_unique_length == 1)

    # assert 2: check if the length is of expected value
    n_expected_from_webpage <- length(ids_cur)
    if (count_remain >= n_expected_from_webpage) {
      assertthat::assert_that(length(info_extracted_cur$title_cur) == n_expected_from_webpage)
      n_expected_to_add <- n_expected_from_webpage # the number of records to add within this batch
    } else { # in this situation, we expect a truncation
      for (k in names(info_extracted_cur)) {
        info_extracted_cur[[k]] <- info_extracted_cur[[k]][1:count_remain]
      }
      n_expected_to_add <- count_remain
    }

    # save this batch of information
    for (k in names(info_extracted)) {
      info_extracted[[k]] <- c(info_extracted[[k]], info_extracted_cur[[glue::glue("{k}_cur")]])
    }

    count_done <- count_done + n_expected_to_add
    count_remain <- count_remain - n_expected_to_add
    print(glue::glue("Done: {count_done}/{n_expected_to_return}"))
  }

  return(data.frame(info_extracted, stringsAsFactors = F))







  if (n <= maxsize_db) {
    print(paste("Total results: ", n, sep = ""))
    url_summary <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
      paste(ids, collapse = ","),
      "&rettype=text",
      sep = ""
    )
    page_summary <- xml2::read_html(url_summary)
    title <- page_summary %>%
      rvest::html_nodes("articletitle") %>%
      rvest::html_text()
    year <- page_summary %>%
      rvest::html_nodes('pubmedpubdate[pubstatus="pubmed"] year') %>%
      rvest::html_text() %>%
      as.numeric()
    article_nodes <- page_summary %>% rvest::html_nodes("article")
    author <- c()
    abstract <- c()
    for (article_node in article_nodes) {
      author <- c(author, paste(article_node %>% rvest::html_nodes("forename") %>% rvest::html_text(), article_node %>% rvest::html_nodes("lastname") %>% rvest::html_text(), collapse = ", "))
      abstract_now <- article_node %>%
        rvest::html_nodes("abstract") %>%
        rvest::html_text()
      abstract <- if (length(abstract_now) > 0) c(abstract, abstract_now) else c(abstract, "No abstract")
    }

    print(paste("Done: ", n, "/", n, sep = ""))
  } else {
    title <- c()
    author <- c()
    abstract <- c()
    year <- c()
    count_done <- 0
    time_query <- Sys.time()

    for (k in 1:num_page) {
      ids_cur <- if (k < num_page) ids[seq((1 + maxsize_db * (k - 1)), maxsize_db * k)] else ids[seq((1 + maxsize_db * (k - 1)), length(ids))]
      url_summary_cur <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
        paste(ids_cur, collapse = ","),
        "&rettype=text",
        sep = ""
      )
      page_cur <- xml2::read_html(url_summary_cur)
      title_cur <- page_cur %>%
        rvest::html_nodes("articletitle") %>%
        rvest::html_text()
      year_cur <- page_cur %>%
        rvest::html_nodes('pubmedpubdate[pubstatus="pubmed"] year') %>%
        rvest::html_text() %>%
        as.numeric()
      article_nodes_cur <- page_cur %>% rvest::html_nodes("pubmedarticle")
      author_cur <- c()
      abstract_cur <- c()
      for (article_node_cur in article_nodes_cur) {
        count_done <- count_done + 1
        author_cur <- c(author_cur, paste(article_node_cur %>% rvest::html_nodes("forename") %>% rvest::html_text(), article_node_cur %>% rvest::html_nodes("lastname") %>% rvest::html_text(), collapse = ", "))
        abstract_cur_now <- article_node_cur %>%
          rvest::html_nodes("abstract") %>%
          rvest::html_text()
        abstract_cur <- if (length(abstract_cur_now) > 0) c(abstract_cur, abstract_cur_now) else c(abstract_cur, "No abstract")
      }

      title <- c(title, title_cur)
      author <- c(author, author_cur)
      abstract <- c(abstract, abstract_cur)
      year <- c(year, year_cur)
      print(paste("Done: ", count_done, "/", n, sep = ""))
    }
  }
  closeAllConnections()
  return(data.frame(title = title, author = author, year = year, abstract = abstract, link = link, availability = rep(NA, length(title)), stringsAsFactors = F))
}
