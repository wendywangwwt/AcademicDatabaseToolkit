library(assertthat)

#' Parse search results from Sage Journal
#'
#' @description
#' Parses search results from Sage Journal as a dataframe. This is used by the main
#' web_scrapper() function.
#'
#' @param page the resulting webpage object returned by rvest
#' @param n the number of records to keep
#' @param limit_per_search the number of maximum records to keep
#'
#' @noRd
#'
parse_res_sj <- function(url, page, n, limit_per_search) {
  maxsize_db <- 100
  limit_per_search <- min(n,limit_per_search) # make sure the number is valid, i.e., no larger than total

  n_total <- n # the total number of search results
  n_expected_to_return <- ifelse(!is.null(limit_per_search) & (limit_per_search < n), limit_per_search, n) # the possibly truncated number of results to return

  # number of pages for ALL search results
  num_page_total <- ifelse(n_total %% maxsize_db == 0, n_total %/% maxsize_db, n_total %/% maxsize_db + 1)

  # effective number of pages for results to be retrieved
  if (is.null(limit_per_search)){
    num_page <- num_page_total
  }else{
    num_page <- ifelse(limit_per_search %% maxsize_db == 0, limit_per_search %/% maxsize_db, limit_per_search %/% maxsize_db + 1)
  }

  print(paste("Total pages: ", num_page, sep = ""))

  info_extracted <- list(
    title = c(),
    link = c(),
    author = c(),
    abstract = c(),
    year = c(),
    journal = c(),
    availability = c()
  )

  count_done <- 0
  count_remain <- n_expected_to_return

  page_cur <- page
  for (k in 1:num_page) {
    print(paste("Current page: ", k, "/", num_page, sep = ""))

    # if more than one page, fetch the next (now current) page using a modified url
    if (k > 1) {
      url_cur <- paste(url, k - 1, sep = "")
      page_cur <- read_html(url_cur)
    }

    n_expected_from_webpage <- ifelse(k < num_page_total, maxsize_db, n_total %% maxsize_db)

    # initialize the info list for the current page
    info_extracted_cur <- list(
      title_cur = c(),
      link_cur = c(),
      author_cur = c(),
      abstract_cur = c(),
      year_cur = c(),
      journal_cur = c(),
      availability_cur = c()
    )

    # title
    info_extracted_cur[["title_cur"]] <- page_cur %>%
      html_elements("h3.issue-item__heading") %>%
      html_text()

    # link
    info_extracted_cur[["link_cur"]] <- paste("http://journals.sagepub.com", page_cur %>% html_elements("a.sage-search-title") %>% html_attr("href"), sep = "")

    # published year
    year_cur <- page_cur %>%
      html_elements("div.issue-item__header") %>%
      html_element("span:nth-child(3)") %>%
      html_text()
    info_extracted_cur[["year_cur"]] <- stringr::str_sub(year_cur, -4, -1)

    # journal
    info_extracted_cur[['journal_cur']] <- page_cur %>%
      html_elements("div.issue-item__row.issue-item__journal") %>%
      html_text()

    # availability
    availability_cur <- page_cur %>%
      html_elements("span.issue-item-access") %>%
      html_text()
    info_extracted_cur[["availability_cur"]] <- ifelse(availability_cur %in% c("Available access", "Open Access"), "Y", "N")

    # author
    author_nodes_cur <- page_cur %>%
      html_elements("div.issue-item__authors > ul")
    for (author_node in author_nodes_cur) {
      info_extracted_cur[["author_cur"]] <- c(info_extracted_cur[["author_cur"]], author_node %>% html_elements("li") %>% html_text() %>% paste(collapse = ", "))
    }

    # abstract - there can be articles with NO abstract :(
    abstract_cur <- c()
    nodes <- page_cur %>%
      html_elements("div.rlist.search-result__body.items-results") %>%
      html_children()

    for (node in nodes) {
      if (length(node %>% html_elements("div.issue-item__abstract__content")) == 0){
        abstract_cur <- c(abstract_cur,NA)
      }else{
        abstract_cur <- c(abstract_cur,node %>% html_elements("div.issue-item__abstract__content") %>% html_text())
      }
    }
    info_extracted_cur[["abstract_cur"]] <- stringr::str_sub(abstract_cur, 9, -1) # remove the first word (Abstract)


    # assert 1: different types of info should have the same length
    n_unique_length <- lapply(info_extracted_cur, length) %>%
      unlist() %>%
      unique() %>%
      length()
    assert_that(n_unique_length == 1)


    # assert 2: check if the length is of expected value
    # note that Sage Journals could return more search results than it claims on
    # the LAST page, so cannot do this number check if it is the last page
    n_effective_from_webpage <- length(info_extracted_cur[['abstract_cur']])
    if (count_remain >= n_effective_from_webpage) {
        assert_that(length(info_extracted_cur$title_cur) == n_expected_from_webpage)
        n_expected_to_add <- n_expected_from_webpage # the number of records to add within this batch
    } else { # in this situation, we expect a truncation
      # if (k == num_page_total){
      #   idx_keep_until <- length(info_extracted_cur[["abstract_cur"]]) # if it's the last page, Sage Journals could return more results than it claimed
      # }else{
      #   idx_keep_until <- count_remain
      # }

      idx_keep_until <- count_remain

      for (k in names(info_extracted_cur)) {
        info_extracted_cur[[k]] <- info_extracted_cur[[k]][1:idx_keep_until]
      }
      n_expected_to_add <- idx_keep_until
    }

    # save this batch of information
    for (k in names(info_extracted)) {
      info_extracted[[k]] <- c(info_extracted[[k]], info_extracted_cur[[glue("{k}_cur")]])
    }

    count_done <- count_done + n_expected_to_add
    count_remain <- count_remain - n_expected_to_add
    print(glue("Done: {count_done}/{n_expected_to_return}"))
  }

  return(data.frame(info_extracted, stringsAsFactors = F))
}
