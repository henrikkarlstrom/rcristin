#' Get Cristin results
#'
#' @param doi string
#' @param title string
#' @param contributor string
#' @param issn string
#' @param unit string
#' @param institution string
#' @param user string
#' @param category string
#' @param published_since string
#' @param published_before string
#' @param created_since string
#' @param created_before string
#' @param modified_since string
#' @param modified_before string
#' @param year_reported string
#' @param project_code string
#' @param funding string
#' @param funding_source string
#' @param page integer
#' @param per_page integer
#' @param fields string
#' @param NVI logical
#'
#' @return data frame
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' get_cristin_results(contributor = "25062")

get_cristin_results <- function(
  doi = NULL,
  title = NULL,
  contributor = NULL,
  issn = NULL,
  unit = NULL,
  institution = NULL,
  user = NULL,
  category = NULL,
  published_since = NULL,
  published_before = NULL,
  created_since = NULL,
  created_before = NULL,
  modified_since = NULL,
  modified_before = NULL,
  year_reported = NULL,
  project_code = NULL,
  funding = NULL,
  funding_source = NULL,
  page = 1,
  per_page = 999,
  fields = "all",
  NVI = FALSE
  ) {

  if(NVI) {

    articles <- get_cristin_results(
      doi = doi,
      title = title,
      contributor = contributor,
      issn = issn,
      unit = unit,
      institution = institution,
      user = user,
      category = "ARTICLE",
      published_since = published_since,
      published_before = published_before,
      created_since = created_since,
      created_before = created_before,
      modified_since = modified_since,
      modified_before = modified_before,
      year_reported = year_reported,
      project_code = project_code,
      funding_source = funding_source,
      page = 1,
      per_page = 999,
      fields = "all"
      )

    monographies <- get_cristin_results(
      doi = doi,
      title = title,
      contributor = contributor,
      issn = issn,
      unit = unit,
      institution = institution,
      user = user,
      category = "MONOGRAPHACA",
      published_since = published_since,
      published_before = published_before,
      created_since = created_since,
      created_before = created_before,
      modified_since = modified_since,
      modified_before = modified_before,
      year_reported = year_reported,
      project_code = project_code,
      funding_source = funding_source,
      page = 1,
      per_page = 999,
      fields = "all"
    )

    chapters <- get_cristin_results(
      doi = doi,
      title = title,
      contributor = contributor,
      issn = issn,
      unit = unit,
      institution = institution,
      user = user,
      category = "CHAPTERACADEMIC",
      published_since = published_since,
      published_before = published_before,
      created_since = created_since,
      created_before = created_before,
      modified_since = modified_since,
      modified_before = modified_before,
      year_reported = year_reported,
      project_code = project_code,
      funding_source = funding_source,
      page = 1,
      per_page = 999,
      fields = "all"
    )

    return(
      dplyr::bind_rows(
        articles,
        monographies,
        chapters
        )
      )
  }

  # API call to Cristin
  base_data <- httr::GET(
    url = "https://api.cristin.no/v2/results?",
    query = list(
      doi = doi,
      title = title,
      contributor = contributor,
      issn = issn,
      unit = unit,
      institution = institution,
      user = user,
      category = category,
      published_since = published_since,
      published_before = published_before,
      created_since = created_since,
      created_before = created_before,
      modified_since = modified_since,
      modified_before = modified_before,
      year_reported = year_reported,
      project_code = project_code,
      funding = funding,
      funding_source = funding_source,
      page = page,
      per_page = per_page,
      fields = fields
      )
    )

  # checking if the response returns a valid status and aborts if the call has been wrongly specified or there are no results
  if(httr::http_error(base_data)){
    stop(
      paste(
        "Query failed with status",
        httr::http_status(post_query)[["message"]]
      )
    )
  } else if(base_data[["headers"]][["x-total-count"]] == 0){
    return(data.frame())

  } else {

    # fetches next page URL, for pagination purposes
    paging <- base_data[["headers"]][["link"]]

    # create the base data tibble, containing the variables connected to a Cristin result that only occur once
    base_data <- tibble::as_tibble(
      jsonlite::fromJSON(
        txt = httr::content(base_data, "text"),
        flatten = TRUE
        )
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("title"),
        names_to = "title_language",
        values_to = "title",
        values_drop_na = TRUE
        ) %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("summary"),
        names_to = "summary_language",
        values_to = "summary"
        ) %>%
      dplyr::distinct(cristin_result_id, .keep_all = TRUE)

    # fetch the rest of the results from the API call
    if(!is.null(paging)) {
      while(
        grepl("next", paging)
        ){
        url <- regmatches(paging, regexec("<(.+?)>", paging))[[1]][2]

        base_data2 <- httr::GET(url)

        paging <- base_data2[["headers"]][["link"]]

        base_data2 <- tibble::as_tibble(
          jsonlite::fromJSON(
            httr::content(
              base_data2,
              "text"
              ),
            flatten = TRUE
            )
          ) %>%
          tidyr::pivot_longer(
            cols = dplyr::starts_with("title"),
            names_to = "title_language",
            values_to = "title",
            values_drop_na = TRUE
          ) %>%
          tidyr::pivot_longer(
            cols = dplyr::starts_with("summary"),
            names_to = "summary_language",
            values_to = "summary"
          ) %>%
          dplyr::distinct(cristin_result_id, .keep_all = TRUE)

        base_data <- dplyr::bind_rows(
          base_data,
          base_data2
          )

      }
    }

    # clean up title and summary language columns
    base_data[["title_language"]] <- gsub(
      "title.",
      "",
      base_data[["title_language"]]
      )

    base_data[["summary_language"]] <- gsub(
      "summary.",
      "",
      base_data[["summary_language"]]
    )

    # clean up column names
    names(base_data) <- gsub("\\.", "_", names(base_data))

    return(base_data)

  }

}
