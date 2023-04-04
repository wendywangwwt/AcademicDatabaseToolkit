l_field <- list(
  "sage_journal" = list(
    "abstract" = "abstract",
    "all" = "allfield"
  ),
  "science_direct" = list(
    "abstract" = "tak",
    "all" = "all"
  ),
  "pubmed" = list(
    "abstract" = "[title/abstract]",
    "all" = ""
  ),
  "proquest" = list(
    "abstract" = "abstract%3D",
    "all" = ""
  )
)


SUBDB_PQ <- c(
  "politicalscience",
  "publichealth",
  "psychology",
  "sociology",
  "socscijournals",
  "marketresearch",
  "medline"
)



#' Search Specified Databases
#'
#' This is the main function to be used. This function searches the specified
#' databases using the keywords provided.
#'
#' @param keywords Required.
#'   If you only have one set of keywords, this will be a string of one keyword or
#'   a vector of multiple keywords:
#'       "machine learning"
#'       c("neural networks", "deep learning")
#'   If you have multiple sets of keywords, for example each set describing one concept
#'   using different wording, and you need to search all the combinations between these
#'   keyword sets, then provide a list of keywords:
#'       list(c("neural networks", "deep learning"),
#'            c("text","textual","document"),
#'            c("psychology","psychological"))
#'   Note that in the second case, argument "relationship" is ignored as it is useless.
#' @param relationship Relationship between the keywords. Can be either `or` or
#'   `and`. Default to `or`. When using `or`, the function runs a search for each
#'   and every keyword independently. When using `and`, the function runs one search
#'   that combines all the keywords.
#' @param database_name A string of one database name or a vector of multiple
#'   database names. Default to `sage_journal`. Currently support:
#'   `sage_journal`, `science_direct`, `pubmed`, `proquest`
#'   If both correct and wrong database names are specified, only the correct ones
#'   will be queried.
#' @param subdb_proquest A string or a vector of sub databases in proquest
#'   to query. You can use `get_proquest_subdb()` to retrieve a vector of all subscribed
#'   sub-databases, then pass the ones needed to this argument.
#' @param field The field to search for the keywords. Currently support:
#'   `abstract`, `all`
#'   Some databases may not support some field to be used in the search.
#' @param sdkey API key for science direct.
#' @param drop_duplicate Whether to remove duplicated results (TRUE) or not (FALSE).
#'   One article could be returned by multiple databases, if you specified multiple
#'   databases to search in `database_name`.
#' @param limit_per_search The maximum number of results collected from each
#'   database.
#' @param availability_pubmed Whether query to see if the full text of an article
#'   is available to you (TRUE) or not (FALSE). Default to FALSE. Because it has
#'   to check each article one by one, it can be time consuming and can hit the
#'   3 queries / sec limit.
#'
#' @export
search_database <- function(keywords,
                            relationship = "or",
                            field = "abstract",
                            database_name = c("sage_journal", "science_direct", "pubmed", "proquest"),
                            subdb_proquest = SUBDB_PQ, sdkey = "",
                            start_year = NULL, end_year = NULL, additional_args = list(),
                            limit_per_search = NULL,
                            drop_duplicate = T, availability_pubmed = F) {
  vals_relationship <- c("or", "and")
  vals_field <- c("abstract", "all")
  vals_database_name <- c("sage_journal", "science_direct", "pubmed", "proquest")

  # assert via match.arg
  relationship <- match.arg(relationship, vals_relationship)
  field <- match.arg(field, vals_field)
  database_name <- match.arg(database_name, vals_database_name, several.ok = T)

  # assert via assertthat
  assertthat::assert_that(is.list(keywords) | is.vector(keywords,mode='character'),msg="keywords should either be a list of keyword sets, or a vector of keywords as string")

  num_database <- length(database_name)
  if (is.list(keywords)){
    # num_keyword reflects the number of keyword combinations in this case
    num_keyword <- lapply(keywords,length) %>% unlist() %>% cumprod() %>% tail(1)
  }else{
    num_keyword <- length(keywords)
  }

  if (relationship == "and") {
    num_stage <- 1 * num_database
  } else {
    num_stage <- num_keyword * num_database
  }

  num_stage_cur <- 0
  df_total <- data.frame(stringsAsFactors = F)

  for (db in database_name) {
    if (!is.list(keywords) & (relationship == "and")) {
      num_stage_cur <- num_stage_cur + 1

      print(glue::glue("\nStart current stage ({num_stage_cur}/{num_stage}):"))
      print(glue::glue('Keyword: {paste(keywords,collapse=", ")}; Database: {db}; Field: {field}'))

      df_db <- fetch_and_clean(keywords, field, db, subdb_proquest, sdkey, drop_duplicate, limit_per_search)
    } else {
      df_db <- data.frame(stringsAsFactors = F)

      # reformat keyword vector into df to standardize the processing
      # for a vector of keywords, this creates a 1-column df
      # for a list of keyword sets, this creates a df where each row is a combination
      df_keywords <- expand.grid(keywords,stringsAsFactors=F)

      for (i in 1:nrow(df_keywords)) {
        num_stage_cur <- num_stage_cur + 1

        print(glue::glue("Start current stage ({num_stage_cur}/{num_stage}):"))
        print(glue::glue("Keyword: {df_keywords[i,] %>% unlist() %>% paste(collapse=' AND ')}; Database: {db}; Field: {field}"))

        keyword <- df_keywords[i,] %>% unlist() %>% unname()
        df_db_kw <- fetch_and_clean(
          keyword, field, db, subdb_proquest, sdkey, drop_duplicate, limit_per_search,
          start_year, end_year, additional_args
        )
        df_db <- dplyr::bind_rows(df_db, df_db_kw)
      }
    }

    df_total <- dplyr::bind_rows(df_total, df_db)
  }


  if (drop_duplicate) {
    df_total <- df_total[order(df_total$availability, decreasing = T), ]
    title_tmp <- tolower(gsub("[^[:alnum:]]", "", df_total$title))
    df_total <- df_total[!duplicated(title_tmp), ]
  }

  if ("pubmed" %in% database_name) {
    if (availability_pubmed) {
      if (length(is.na(df_total$availability)) > 0) {
        pubmed_idx <- which(df_total$database == "pubmed")
        count_done <- 0
        print("Collect availability info for the remaining PubMed records")
        for (idx in pubmed_idx) {
          page_cur <- xml2::read_html(as.character(df_total$link[idx]))
          availability_cur <- page_cur %>% rvest::html_nodes('[class="icons portlet"]')
          df_total$availability[idx] <- ifelse(length(availability_cur) > 0, "Y", "N")
          count_done <- count_done + 1
          print(glue::glue("Done: {count_done}/{length(pubmed_idx)}"))
        }
        closeAllConnections()
      }
    }
  }

  return(df_total)
}
