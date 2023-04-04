test_that("limit > maxsize_db", {
  subdb_proquest <- c('politicalscience')
  keywords <- c('text analysis')
  database_name <- c('proquest')
  df_data <- search_database(keywords,field='abstract',
                             database_name=database_name,
                             subdb_proquest=subdb_proquest,
                             drop_duplicate=F)
})

test_that("limit < maxsize_db", {
  subdb_proquest <- c('politicalscience')
  keywords <- c('text analysis')
  database_name <- c('proquest')
  df_data <- search_database(keywords,field='abstract',
                             database_name=database_name,
                             subdb_proquest=subdb_proquest,
                             limit_per_search = 10,
                             drop_duplicate=F)
})
