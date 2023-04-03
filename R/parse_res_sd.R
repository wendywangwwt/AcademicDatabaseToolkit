#' Parse search results from Science Direct
#'
#' @description
#' Parses search results from Science Direct as a dataframe. This is used by the main
#' web_scrapper() function.
#'
#' @param page the resulting webpage object returned by rvest
#' @param n the number of records to keep
#' @param limit_per_search the number of maximum records to keep
#'
#' @noRd
get_info_sd <- function(page,n,limit_per_search){
  maxsize_db <- 200

  n <- ifelse(!is.null(limit_per_search) & (limit_per_search<n),limit_per_search,n)

  print(paste('Total results: ', n, sep=''))


  if (n <= maxsize_db){
    title <- page %>% html_nodes('title') %>% html_text()
    link <- page %>% html_nodes('entry link[ref="scidir"]') %>% html_attr('href')
    year <- page %>% html_nodes('coverdate') %>% html_text() %>% str_sub(1,4) %>% as.numeric()
    article_nodes <- page %>% html_nodes('entry')
    author <- c()
    abstract <- c()

    for (article_node in article_nodes){
      author <- c(author, paste(article_node %>% html_nodes('given-name') %>% html_text(),article_node %>% html_nodes('surname') %>% html_text(),collapse = ", "))
      abstract_now <- article_node %>% html_nodes('description') %>% html_text()
      abstract <- if(length(abstract_now)>0) c(abstract,abstract_now) else c(abstract,'No abstract')
    }
    print(paste("Done: ", n,'/',n,sep=''))

  }else{
    num_page = ifelse(n %% maxsize_db == 0, n %/% maxsize_db,n %/% maxsize_db + 1)

    print(paste('Total pages: ', num_page, sep=''))

    title <- c()
    author <- c()
    abstract <- c()
    link <- c()
    year <- c()
    count_done <- 0

    page_cur <- page
    for (k in 1:num_page){
      print(paste('Current page: ',k,'/', num_page, sep=''))

      n_left <- n - (k-1)*maxsize_db

      if(k>1){
        url_cur <- page_cur %>% html_nodes('link[ref="next"]') %>% html_attr('href')
        page_cur <- read_html(url_cur)
      }

      title_cur <- page_cur %>% html_nodes('title') %>% html_text()
      link_cur <- page_cur %>% html_nodes('entry link[ref="scidir"]') %>% html_attr('href')
      year_cur <- page_cur %>% html_nodes('coverdate') %>% html_text() %>% str_sub(1,4) %>% as.numeric()
      article_nodes_cur <- page_cur %>% html_nodes('entry')

      if ((n_left < maxsize_db) & !is.null(limit_per_search) & (n == limit_per_search)){
        title <- c(title, title_cur[1:n_left])
        link <- c(link, link_cur[1:n_left])
        year <- c(year, year_cur[1:n_left])
        article_nodes_cur <- article_nodes_cur[1:n_left]
      }else{
        title <- c(title, title_cur)
        link <- c(link, link_cur)
        year <- c(year, year_cur)
      }


      author_cur <- c()
      abstract_cur <- c()
      for (article_node_cur in article_nodes_cur){
        author_cur <- c(author_cur, paste(article_node_cur %>% html_nodes('given-name') %>% html_text(),article_node_cur %>% html_nodes('surname') %>% html_text(),collapse = ", "))
        abstract_cur_now <- article_node_cur %>% html_nodes('description') %>% html_text()
        abstract_cur <- if(length(abstract_cur_now)>0) c(abstract_cur,abstract_cur_now) else c(abstract_cur,'No abstract')
        count_done <- count_done+1
      }
      author <- c(author,author_cur)
      abstract <- c(abstract,abstract_cur)
      print(paste("Done: ", count_done,'/',n,sep=''))
    }
  }

  return(data.frame(title = title, author = author,year=year, abstract = abstract, link = link, availability = rep("Y",length(title)),stringsAsFactors = F))
}
