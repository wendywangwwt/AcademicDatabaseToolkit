# Academic Database Toolkit
This is a refactored & extended package of the scripts in [WebScrapper-for-Academic-Databases](https://github.com/wendywangwwt/WebScrapper-for-Academic-Databases), a side project started years ago.

This toolkit is intended to include
- A web scrapper for academic databases in R/Python in the form of a (set of) function. Initially, this was built for accelerating meta-data analysis where one intends to try various combinations of different keywords in a handful of databases. Manually typing the keywords in each databases and eye-balling the resulting articles can be extremely inefficient and painful, especially when an article already seen from a previous database came up again in a new database, and
- A set of useful functions to summarize, analyze, and visualize the retrieved search results.

The web scrapper automatically
- generates query based on the provided keywords
- parses the result page (xml in PubMed for example)
- extracts the a group of information, including
  - title
  - author(s)
  - published year
  - abstract
  - link to the article
  - availability (may not be available to all databases)
  - search term
  - database
- returns the results as a data frame

Supported databases as of 2023-04-03, for ONE or MULTIPLE sets of keywords:
- PubMed (`pubmed`)
- Sage Journals (`sage_journal`): note that you may want to use filter **Article Type** to include only the research articles for your work (there are other types such as review article) by `search_database(...,additional_args=list(ContentItemType='research-article'))`


Databases that need code update as of 2023-04-03:
- Science Direct (`science_direct`)
- ProQuest (`proquest`)

Current development work focuses on implementing the automated search of MULTIPLE sets of keywords. This is the scenario where each concept can be described with slightly or largely different words/phrases, and the need is to search for articles that involves at least one keyword from each concept (relationship=`or`).


## How to install
```
devtools::install_github('wendywangwwt/AcademicDatabaseToolkit')
library(AcademicDatabaseToolkit)
```

## How to use

#### param: keywords
For ONE set of keywords, use a string of keyword, or a vector of multiple keywords:
```
keywords <- c("decisions","decision-making")
```
For MULTIPLE set of keywords, use a list where each sublist is a vector of one or multiple keywords that describes the same concept:
```
keywords <- list(c("decisions","decision-making"),
                 c( "consumer behavior"))
```

#### param: database_name
A string of database name:
```
database_name <- 'pubmed'
```
or a vector of multiple database names:
```
database_name <- c('pubmed','sage_journal')
```

#### param: limit_per_search
Put a limit to avoid collecting tens of thousands of results, unless it is intended.
```
df_data <- search_databases(keywords,database_name=database_name,limit_per_search=300)
```

#### param: relationship
The relationship between keywords, if multiple are provided. Default to `or`. So the above example is equivalent to:
```
df_data <- search_databases(keywords,relationship='or',database_name=database_name,limit_per_search=300)
```

If you want to concatenate your keywords with an AND relationship for the search, change the value to `and`.
```
df_data <- search_databases(keywords,relationship='and',database_name=database_name,limit_per_search=300)
```

#### param: field
Which field to search. Default to `abstract` (depending on database, this usually includes article title & keywords as well). Optionally, you can switch to `all`, to search full article.
```
df_data <- search_databases(keywords,relationship='and',field='all',database_name=database_name,limit_per_search=300)
```

#### param: no_duplicate
Whether to drop duplicated results or not. Default to TRUE. Duplicated results come from searches across keywords (relationship='or'), and/or searches across databases. You may want to turn it off if you'd like to better understand which database + search term combinations bring a duplicate. Higher number of duplicates could indicate higher relevancy of the article to the topic you intend to look into.
```
df_data <- search_databases(keywords,relationship='or',database_name=database_name,no_duplicate=F,limit_per_search=300)
```


    
## Change log
2023-04-03:
- re-factored the code into an R package that can be installed from GitHub, for pubmed & sage journals
- added tests for pubmed and sage journals
- updated readme

2022-02-20:
- re-factored the code (not completely) and updated the readme file
- only pubmed passed the tests, need to check and update the interaction with other databases later

