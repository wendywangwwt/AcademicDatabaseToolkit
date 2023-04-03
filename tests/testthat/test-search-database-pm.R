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
