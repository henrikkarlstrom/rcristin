#' Get Cristin results
#'
#' @param unit string
#' @param institution string
#' @param contributor string
#' @param project_code string
#' @param user string
#' @param category string
#' @param funding_source string
#' @param created_since string
#' @param created_before string
#' @param published_since string
#' @param published_before string
#' @param per_page string
#' @param page string
#' @param fields string
#' @param simplify string
#'
#' @return data frame
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' get_cristin_results(contributor = "25062")

get_cristin_results <- function(unit = NULL,
                                institution = NULL,
                                contributor = NULL,
                                project_code = NULL,
                                user = NULL,
                                category = NULL,
                                funding_source = NULL,
                                created_since = NULL,
                                created_before = NULL,
                                published_since = NULL,
                                published_before = NULL,
                                per_page = 999,
                                page = 1,
                                fields = "all",
                                simplify = FALSE) {

  # API call to Cristin
  base_data <- httr::GET(url = "https://api.cristin.no/v2/results?",
                         query = list(unit = unit,
                                      institution = institution,
                                      contributor = contributor,
                                      project_code = project_code,
                                      user = user,
                                      category = category,
                                      funding_source = funding_source,
                                      published_since = published_since,
                                      published_before = published_before,
                                      created_since = created_since,
                                      created_before = created_before,
                                      per_page = per_page,
                                      page = page,
                                      fields = fields))

  # checking if the response returns a valid status and aborts if the call has been wrongly specified or there are no results
  if(base_data[["status_code"]] != 200
     ||
     base_data[["headers"]][["x-total-count"]] == 0) {
    return(NA)

  } else {

    # fetches next page URL
    paging <- base_data[["headers"]][["link"]]

    # creates the base data tibble, containing the variables connected to a Cristin result that only occur once
    base_data <- tibble::as_tibble(jsonlite::fromJSON(
      httr::content(base_data, "text"), flatten = TRUE)) %>%
      tidyr::gather(dplyr::starts_with("title"),
                    key = "title_language",
                    value = "title",
                    na.rm = TRUE) %>%
      dplyr::select(-dplyr::starts_with("summary"),
                    -import_sources)

    # fetches the rest of the results from the API call
    while(grepl("next", paging)){
      url <- regmatches(paging, regexec("<(.+?)>", paging))[[1]][2]

      base_data2 <- httr::GET(url)
      paging <- base_data2[["headers"]][["link"]]
      base_data2 <- tibble::as_tibble(jsonlite::fromJSON(
        httr::content(base_data2, "text"), flatten = TRUE)) %>%
        tidyr::gather(dplyr::starts_with("title"),
                      key = "title_language",
                      value = "title",
                      na.rm = TRUE)%>%
        dplyr::select(-dplyr::starts_with("summary"))

      base_data <- dplyr::bind_rows(base_data, base_data2)

    }


    base_data[["title_language"]] <- gsub("title.", "", base_data[["title_language"]])

    data <- list()


    # creates a new tibble for funding source data, which can contain multiple entries per Cristin result
      if("funding_sources" %in% colnames(base_data)){

        funding_sources <- as.list(base_data[["funding_sources"]])
        names(funding_sources) <- base_data[["cristin_result_id"]]
        funding_sources <- purrr::compact(funding_sources) %>%
          dplyr::bind_rows(.id = "cristin_result_id") %>%
          dplyr::rename("funding_source_project_code" = project_code,
                        "funding_source_name" = funding_source_name.en)
        data <- append(data, list(funding_sources))

      }

    # creates a new tibble for links data, which can contain multiple entries per Cristin result
      if("links" %in% colnames(base_data)){

        links <- as.list(base_data[["links"]])
        names(links) <- base_data[["cristin_result_id"]]
        links <- purrr::compact(links) %>%
          dplyr::bind_rows(.id = "cristin_result_id") %>%
          dplyr::rename("link_type" = url_type,
                        "link_url" = url)

        data <- append(data, list(links))

      }

    # creates a new tibble for project data, which can contain multiple entries per Cristin result
      if("projects" %in% colnames(base_data)){

        projects <- as.list(base_data[["projects"]])
        names(projects) <- base_data[["cristin_result_id"]]
        projects <- purrr::compact(projects) %>%
          dplyr::bind_rows(.id = "cristin_result_id") %>%
          dplyr::select(-dplyr::starts_with("title")) %>%
          dplyr::rename("project_url" = url)

        data <- append(data, list(projects))

      }

    # creates a new tibble with journal identifiers, which can be both printed and electronic
    if("journal.international_standard_numbers" %in% colnames(base_data)){

      journal_id <- as.list(base_data[["journal.international_standard_numbers"]])
      names(journal_id) <- base_data[["cristin_result_id"]]
      journal_id <- purrr::compact(journal_id) %>%
        dplyr::bind_rows(.id = "cristin_result_id") #%>%
        dplyr::rename("journal_id_type" = type,
                      "journal_id" = value)

      data <- append(data, list(journal_id))

    }

    #selects the columns containing unique observations per publication
    base_data <- base_data %>%
      dplyr::select("cristin_result_id",
                    "open_access",
                    "original_language",
                    "url",
                    dplyr::contains("date"),
                    dplyr::contains("title"),
                    dplyr::contains("name"),
                    dplyr::contains("year"),
                    "title",
                    "contributors.url",
                    dplyr::contains("journal.nvi_level")
      )

    data <- append(data, list(base_data), after = 0)
  }


  # combine the base data with the specified analysis
  if(simplify) {

    data <- Reduce(function(x, y) merge(x, y, by = "cristin_result_id", all = TRUE), data)
    return(data)

  } else {

    return(data)

  }

}
