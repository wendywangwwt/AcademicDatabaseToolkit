#' Parse search results from ProQuest
#'
#' @description
#' Parses search results from ProQuest as a dataframe. This is used by the main
#' web_scrapper() function.
#'
#' @param page the resulting webpage object returned by rvest
#' @param n the number of records to keep
#' @param limit_per_search the number of maximum records to keep
#' @param type ?
#'
#' @noRd
parse_res_pq <- function(page,n,limit_per_search,type){
  n <- ifelse(!is.null(limit_per_search) & (limit_per_search<n),limit_per_search,n)

  print(paste('Total results: ', n, sep=''))

  title <- page %>% html_nodes('[tag="245"]') %>% html_text()
  title <- title[type][1:n]
  year <- page %>% html_nodes('[tag="260"] [code="c"]') %>% html_text()
  year <- str_sub(year[type][1:n],-4,-1)
  abstract <- page %>% html_nodes('[tag="520"]') %>% html_text() %>% str_trim()
  abstract <- str_replace(abstract[type][1:n],"\n","")
  link <- page %>% html_nodes('[ind2="1"] [code="u"]') %>% html_text()
  link <- link[type][1:n]

  author <- c()
  availability <- c()
  count_done <- 0
  page_cur <- page %>% html_nodes('recorddata')

  for (i in 1:length(type)){
    if (count_done < limit_per_search){
      if (type[i]){
        author_fullname <- page_cur[i] %>% html_nodes('[tag="100"]') %>% html_text() %>% str_split_fixed(",",2)
        author_cur <- paste(author_fullname[2],author_fullname[1])

        author_other <- page_cur[i] %>% html_nodes('[tag="700"]')
        if (length(author_other)>0){
          for (j in 1:length(author_other)){
            author_fullname <- author_other[j]  %>% html_text() %>% str_split_fixed(",",2)
            author_cur <- paste(author_cur,paste(author_fullname[2],author_fullname[1]),sep=",")
          }
        }
        author <- c(author, str_trim(author_cur))

        availability_cur <- page_cur[i] %>% html_nodes('[tag="856"]') %>% html_text()
        availability <- c(availability,ifelse(str_detect(paste(availability_cur,collapse = " "),"Full Text"),"Y","N"))
        count_done <- count_done + 1
        print(paste("Done: ", count_done,'/',n,sep=''))
      }
    }

  }

  closeAllConnections()
  return(data.frame(title = title, author = author, year = year, abstract = abstract, link = link, availability = availability,stringsAsFactors = F))
}
