#' Parse search results from ProQuest
#'
#' @description
#' Parses search results from ProQuest as a dataframe. This is used by the main
#' web_scrapper() function.
#'
#' @param page the resulting webpage object returned by rvest
#' @param n the number of records to keep
#' @param limit_per_search the number of maximum records to keep
#'
#' @noRd
parse_res_pq <- function(url, page, n, limit_per_search) {
  maxsize_db <- 500
  limit_per_search <- min(n, limit_per_search) # make sure the number is valid, i.e., no larger than total

  n_total <- n # the total number of search results
  n_expected_to_return <- ifelse(!is.null(limit_per_search) & (limit_per_search < n), limit_per_search, n) # the possibly truncated number of results to return

  # number of pages for ALL search results
  num_page_total <- ifelse(n_total %% maxsize_db == 0, n_total %/% maxsize_db, n_total %/% maxsize_db + 1)

  # effective number of pages for results to be retrieved
  if (is.null(limit_per_search)) {
    num_page <- num_page_total
  } else {
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
      url_cur <- paste(stringr::str_sub(url, 1, -2), (k - 1) * maxsize_db + 1, sep = "")
      page_cur <- xml2::read_html(url_cur)
    }

    types <- page_cur %>%
      rvest::html_nodes('[tag="513"]') %>%
      rvest::html_text() %>%
      tolower()
    types_valid <- stringr::str_detect(types, "journal|feature|article|case|study|periodical|literature|conference")
    idx_valid <- which(types_valid)

    # adjust the expected total results according to publication type
    #.  remove the old estimated amount, add back the new estimated amount
    n_expected_from_webpage <- ifelse(k < num_page_total, maxsize_db, n_total %% maxsize_db)

    if (n_expected_to_return > n_expected_from_webpage){
      n_expected_to_return <- n_expected_to_return - n_expected_from_webpage

      n_expected_from_webpage <- min(sum(types_valid), n_expected_from_webpage)
      n_expected_to_return <- n_expected_to_return + n_expected_from_webpage
      count_remain <- n_expected_to_return - count_done
    }


    # filter only the records with a valid publication type
    nodes <- page_cur %>%
      rvest::html_elements("recorddata") %>%
      .[idx_valid]

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
    info_extracted_cur[["title_cur"]] <- nodes %>%
      rvest::html_elements('[tag="245"]') %>%
      rvest::html_text()

    # link
    info_extracted_cur[["link_cur"]] <- nodes %>%
      rvest::html_elements('[ind2="1"] [code="u"]') %>%
      rvest::html_text()

    # published year
    year_cur <- nodes %>%
      rvest::html_elements('[tag="260"] [code="c"]') %>%
      rvest::html_text()
    info_extracted_cur[["year_cur"]] <- stringr::str_sub(year_cur, -4, -1) %>% as.numeric()

    # journal
    info_extracted_cur[["journal_cur"]] <- nodes %>%
      rvest::html_elements('[tag="773"] [code="t"]') %>%
      rvest::html_text()

    # abstract - there can be articles with NO abstract :(
    # availability - one article can have multiple tag 856 that indicates availability
    # .   some with more details
    # author - main author and other authors are stored under different tags
    abstract_cur <- c()
    availability_cur <- c()
    author_cur <- c()
    for (node in nodes) {
      if (length(node %>% rvest::html_elements('[tag="520"]')) == 0) {
        abstract_cur <- c(abstract_cur, NA)
      } else {
        abstract_cur <- c(abstract_cur, node %>% rvest::html_elements('[tag="520"]') %>% rvest::html_text())
      }

      if (length(node %>% rvest::html_elements('[tag="856"]')) == 0) {
        availability_cur <- c(availability_cur, NA)
      } else {
        info <- node %>%
          rvest::html_elements('[tag="856"] [code="3"]') %>%
          rvest::html_text() %>%
          paste(collapse = " ") %>%
          tolower()
        availability_cur <- c(availability_cur, ifelse(stringr::str_detect(info, "full text"), "Y", "N"))
      }

      if (length(node %>% rvest::html_elements('[tag="100"]')) == 0) {
        author_cur <- c(author_cur, NA)
      } else {
        author_main <- node %>%
          rvest::html_elements('[tag="100"]') %>%
          rvest::html_text() %>%
          stringr::str_split_fixed(",", 2) %>%
          stringr::str_trim()
          author_main <- paste(author_main[2], author_main[1])

        author_other <- node %>%
          rvest::html_elements('[tag="700"] [code="a"]') %>%
          rvest::html_text() %>%
          stringr::str_split_fixed(",", 2)
        if (length(author_other) > 0) {
          author_other <- paste(author_other[,2],author_other[,1]) %>% stringr::str_trim()
        }else{
          author_other <- ''
        }
        author_cur <- c(author_cur, paste(author_main, author_other, collapse = ', '))
      }
    }
    info_extracted_cur[["abstract_cur"]] <- abstract_cur
    info_extracted_cur[["availability_cur"]] <- availability_cur
    info_extracted_cur[["author_cur"]] <- author_cur


    # assert 1: different types of info should have the same length
    n_unique_length <- lapply(info_extracted_cur, length) %>%
      unlist() %>%
      unique() %>%
      length()
    assertthat::assert_that(n_unique_length == 1)


    # assert 2: check if the length is of expected value
    # note that Sage Journals could return more search results than it claims on
    # the LAST page, so cannot do this number check if it is the last page
    n_effective_from_webpage <- length(info_extracted_cur[["abstract_cur"]])
    if (count_remain >= n_effective_from_webpage) {
      assertthat::assert_that(length(info_extracted_cur$title_cur) == n_expected_from_webpage)
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
      info_extracted[[k]] <- c(info_extracted[[k]], info_extracted_cur[[glue::glue("{k}_cur")]])
    }

    count_done <- count_done + n_expected_to_add
    count_remain <- count_remain - n_expected_to_add
    print(glue::glue("Done: {count_done}/{n_expected_to_return}"))
  }

  return(data.frame(info_extracted, stringsAsFactors = F))
}
