
test_that("limit > maxsize_db", {
  keywords <- c('natural language processing','machine learning')
  database_name <- 'sage_journal'
  limit_per_search <- 300
  df_data <- search_database(keywords,relationship='or',field='abstract',
                              database_name=database_name,limit_per_search=limit_per_search,
                             drop_duplicate=F)
  expect_equal(nrow(df_data),limit_per_search*length(keywords))
})

# TODO: for OR, put all keywords in ONE query?
test_that("limit < maxsize_db", {
  keywords <- c('natural language processing','machine learning')
  database_name <- 'sage_journal'
  limit_per_search <- 10
  df_data <- search_database(keywords,relationship='or',field='abstract',
                             database_name=database_name,limit_per_search=limit_per_search,
                             drop_duplicate=F)
  expect_equal(nrow(df_data),limit_per_search*length(keywords))
})


test_that("limit > available size, i.e., get all results", {
  keywords <- c('natural language processing','machine learning')
  database_name <- 'sage_journal'
  limit_per_search <- 30000
  df_data <- search_database(keywords,relationship='or',field='abstract',
                             database_name=database_name,limit_per_search=limit_per_search,
                             drop_duplicate=F)
  # expect equal should target at the sum of n from all queries, which unfortunately is not returned atm
})

test_that("multiple sets of keywords", {
  keywords <- list(c("built environment", "neighborhood environment", "social environment"),
                   c( "mental health", "depression"),
                   c( "young adults"))
  database_name <- 'sage_journal'
  df_data <- search_database(keywords,field='abstract',
                             database_name=database_name,start_year=2000,end_year=2023,
                             drop_duplicate=F)
})
