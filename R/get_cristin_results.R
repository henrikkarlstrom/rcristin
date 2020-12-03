#' Get results from the Cristin API
#'
#' @description `get_cristin_results()` takes in search parameters for the Cristin API and returns the results as a tibble.
#'
#' @details # API parameters
#' The Cristin API recognises many search parameters. You can find the latest specification at the Cristin API \href{https://api.cristin.no/v2/}{documentation page}.
#'
#' Here are examples of the formatting of these parameters:
#'
#' | parameter | description |
#' | --- | --- |
#' | \strong{doi} | DOI name (e.g. doi=10.1000/123456) |
#' | \strong{title} | The title of the result |
#' | \strong{contributor} | Author's name or Cristin person id |
#' | \strong{issn} | The issn of the result |
#' | \strong{unit} | Id (e.g. unit=185.53.18.10) |
#' | \strong{institution} | Id (one number, e.g. institution=185), name or acronym of the institution the contributors belong to |
#' | \strong{user} | A person's username in Cristin together with the institution id separated by ':' askeladd:185' means Cristin user 'askeladd' from the institution '185' |
#' | \strong{category} | Result category code, see \href{https://api.cristin.no/v2/results/categories?lang=en}{Cristin category codes} for specification |
#' | \strong{published_since} | Results published since and inclusive the given year, (yyyy), e.g: 2005 |
#' | \strong{published_before} | Results published before and inclusive the given year, (yyyy), e.g: 2017 |
#' | \strong{created_since} | Results created since and inclusive the given date, (yyyy-mm-dd), e.g: 2005-03-17 |
#' | \strong{created_before} | Results created before and inclusive the given date, (yyyy-mm-dd), e.g: 2005-03-17 |
#' | \strong{modified_since} | Results modified since and inclusive the given date, (yyyy-mm-dd), e.g: 2005-03-17 |
#' | \strong{modified_before} | Results modified before and inclusive the given date, (yyyy-mm-dd), e.g: 2005-03-17 |
#' | \strong{year_reported} | The year a result was reported |
#' | \strong{project_code} | Project code is the internal reference number used by funding source |
#' | \strong{funding_source} | Funding source code e.g: NFR |
#' | \strong{funding} | Funding source code e.g: NFR, and project_code together separated by ':' 'NFR:1234' means 'funding_source': NFR' with 'project_code': '1234' |
#' | \strong{lang} | Two letter language code that determines value of fields such as institution name, one of 'en' (default), 'nb', or 'nn'. |
#'
#' @return A tibble with results from the API call, with one row per result.
#' @export
#' @md
#'
#' @param ... Arguments to pass on to Cristin API. See 'API parameters' below
#'   for more information.
#' @param page Page number for pagination purposes, set to 1 by default
#' @param per_page Number of items per page, set to 999 by default
#' @param fields 'all' by default, if omitted will return a minimal set of
#'   fields
#' @param NVI Logical determining whether to return only peer-reviewed
#'   publications or all results, set to 'FALSE' by default

#' @examples
#' get_cristin_results(contributor = "25062")

get_cristin_results <- function(
  ...,
  page = 1,
  per_page = 999,
  fields = "all",
  NVI = FALSE
){

  if(NVI){
    articles <- get_cristin_results(..., category = "ARTICLE")
    chapters <- get_cristin_results(..., category = "CHAPTERACADEMIC")
    monographies <- get_cristin_results(..., category = "MONOGRAPHACA")
    anthologies <- get_cristin_results(..., category = "ANTHOLOGYACA")
    commentary <- get_cristin_results(..., category = "COMMENTARYACA")
    review <- get_cristin_results(..., category = "ACADEMICREVIEW")
    results <- list(articles, chapters, monographies, anthologies, commentary, review)
    results <- dplyr::bind_rows(results)
    return(results)
  }

  args <- list(..., page = page, per_page = per_page, fields = fields)

  if(length(args) == 3){
    if(all(names(args) == c("page", "per_page", "fields"))){
      stop("No search parameters specified. This will download the entire Cristin database.
         If you are certain this is what you want, please provide the parameter
         'published_since = 1900' to the function.")
    }
  }

  base_data <- httr::GET(
    url = "https://api.cristin.no/v2/results?",
    query = args
  )

  if(httr::http_error(base_data)){
    stop(
      print(args),
      paste0(
        "Query failed with error status '",
        httr::http_status(base_data)[["message"]],
        "' and message '",
        jsonlite::fromJSON(httr::content(base_data, "text"))[["errors"]],
        "'"
      )
    )
  } else if(base_data[["headers"]][["x-total-count"]] == 0){
    return(data.frame())
  } else {
    paging <- base_data[["headers"]][["link"]]

    base_data <- tibble::as_tibble(jsonlite::fromJSON(
      txt = httr::content(base_data, "text"),
      flatten = TRUE
    ))


    base_data <- dplyr::mutate(
      .data = base_data,
      title = dplyr::coalesce(
        !!!dplyr::select(base_data, dplyr::starts_with("title"))
      )
    )

    if(any(startsWith(names(base_data), "summary"))){
      base_data <- dplyr::mutate(
        .data = base_data,
        summary = dplyr::coalesce(
          !!!dplyr::select(base_data, dplyr::starts_with("summary"))
          )
        )
      }

    if(!is.null(paging)) {
      while(
        grepl("next", paging)
      ){
        url <- regmatches(paging, regexec("<(.+?)>", paging))[[1]][2]

        base_data2 <- httr::GET(url)

        paging <- base_data2[["headers"]][["link"]]

        base_data2 <- jsonlite::fromJSON(
          txt = httr::content(base_data, "text"),
          flatten = TRUE
        )

        base_data2 <- dplyr::mutate(
          .data = base_data,
          title = dplyr::coalesce(
            dplyr::select(base_data, dplyr::starts_with("title"))
          ),
          summary = dplyr::coalesce(
            dplyr::select(base_data, dplyr::starts_with("summary"))
          )
        )

        base_data <- dplyr::bind_rows(
          base_data,
          base_data2
        )

      }
    }

    names(base_data) <- gsub("\\.", "_", names(base_data))

    base_data <- dplyr::select(
      .data = base_data,
      -c(dplyr::starts_with("title_"),
         dplyr::starts_with("summary_"))
    )

    base_data <- dplyr::select(
      .data = base_data,
      "cristin_result_id",
      "result_url" = "url",
      "category_code",
      "title",
      "year_published",
      dplyr::everything()
    )

    return(base_data)
  }
}
