test_that("limit > maxsize_db", {
  keywords <- c('natural language processing','machine learning')
  database_name <- 'pubmed'
  limit_per_search <- 300
  df_data <- search_database(keywords,relationship='or',field='abstract',
                             database_name=database_name,limit_per_search=limit_per_search,
                             drop_duplicate=F)
  expect_equal(nrow(df_data),limit_per_search*length(keywords))
})

test_that("limit < maxsize_db", {
  keywords <- c('natural language processing','machine learning')
  database_name <- 'pubmed'
  limit_per_search <- 10
  df_data <- search_database(keywords,relationship='or',field='abstract',
                             database_name=database_name,limit_per_search=limit_per_search,
                             drop_duplicate=F)
  expect_equal(nrow(df_data),limit_per_search*length(keywords))
})



test_that("remove invalid search result & shrink size of n accordingly", {
  keywords <- list(c("social environment"),
                   c( "mental health"),
                   c( "young people"))
  database_name <- 'pubmed'
  df_data <- search_database(keywords,field='abstract',
                             database_name=database_name,start_year=2000,end_year=2023,
                             drop_duplicate=F)
})



