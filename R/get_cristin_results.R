#' Get Cristin results
#'
#' @param NVI logical
#' @param unit string
#' @param contributor string
#' @param project_code string
#' @param user string
#' @param category string
#' @param funding_source string
#' @param published_since string
#' @param published_before string
#' @param per_page string
#' @param page string
#' @param fields string
#' @param analyse string
#'
#' @return data frame
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' get_cristin_results(contributor = "25062")

get_cristin_results <- function(NVI = TRUE,
                                unit = NULL,
                                contributor = NULL,
                                project_code = NULL,
                                user = NULL,
                                category = NULL,
                                funding_source = NULL,
                                published_since = NULL,
                                published_before = NULL,
                                per_page = 999,
                                page = 1,
                                fields = "all",
                                analyse = "base_data") {

  # a counter for returning multiple pages of results
  counter <- 0
  total <- 1

  # checks whether the results returned should be limited to scientific publications
  if(NVI == TRUE) {
    category = "ARTICLE"
  } else {
    category = category
  }

  # pagination
  while(counter < total) {

    # API call to Cristin
    base_data <- httr::GET(url = "https://api.cristin.no/v2/results?",
                           query = list(unit = unit,
                                        contributor = contributor,
                                        project_code = project_code,
                                        user = user,
                                        category = category,
                                        funding_source = funding_source,
                                        published_since = published_since,
                                        published_before = published_before,
                                        per_page = per_page,
                                        page = page,
                                        fields = fields))

    # checking if the response returns a valid status and aborts if the call has been
    # wrongly specified or there are no results
    if(httr::status_code(base_data) != 200 || base_data[["headers"]][["x-total-count"]] == 0) {

      counter <- total + 1
      return(NA)

    } else {
      # updates the pagination
      total <- as.numeric(base_data[["headers"]][["x-total-count"]])
      counter <- counter + 1000
      page <- page + 1

      # creates the base data tibble, containing the variables connected to a Cristin result
      # that only occur once
      base_data <- tibble::as_tibble(jsonlite::fromJSON(
        httr::content(base_data, "text"), flatten = TRUE)) %>%
        tidyr::gather(dplyr::starts_with("title"),
                      key = "title_language",
                      value = "title",
                      na.rm = TRUE)

      base_data[["title_language"]] <- gsub("title.", "", base_data[["title_language"]])

      # fetches contributor data from the Cristin API and stores it in a data.frame
      contributors <- lapply(unique(base_data[["contributors.url"]]),
                             httr::GET)

      names(contributors) <- unique(base_data[["cristin_result_id"]])

      contributors <- lapply(contributors, function(x)
        tibble::as_tibble(jsonlite::fromJSON(
          httr::content(x, "text"), flatten = TRUE))) %>%
        dplyr::bind_rows(.id = "cristin_result_id") %>%
        dplyr::filter(!purrr::map_lgl(affiliations, is.null)) %>%
        tidyr::unnest(cols = c(affiliations)) %>%
        dplyr::rename("contributor_url" = url)

      # creates a new tibble for funding source data, which can contain multiple entries
      # per Cristin result
      if("funding_sources" %in% analyse){
        if("funding_sources" %in% colnames(base_data)){
          funding_sources <- base_data %>%
            dplyr::select(cristin_result_id, funding_sources) %>%
            dplyr::filter(!purrr::map_lgl(funding_sources, is.null)) %>%
            tidyr::unnest(cols = funding_sources) %>%
            dplyr::rename("funding_source_project_code" = project_code,
                          "funding_source_name" = funding_source_name.en)
        } else {
          funding_sources <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                            funding_source_code = NA,
                                            funding_source_project_code = NA,
                                            funding_source_name = NA)
        }
      } else {
        funding_sources <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                          funding_source_code = NA,
                                          funding_source_project_code = NA,
                                          funding_source_name = NA)
      }

      # creates a new tibble for links data, which can contain multiple entries
      # per Cristin result
      if("links" %in% analyse){
        if("links" %in% colnames(base_data)){
          links <- base_data %>%
            dplyr::select(cristin_result_id, links) %>%
            dplyr::filter(!purrr::map_lgl(links, is.null)) %>%
            tidyr::unnest(cols = links) %>%
            dplyr::rename("link_type" = url_type,
                          "link_url" = url)
        } else {
          links <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                  link_type = NA,
                                  link_url = NA)
        }
      } else {
        links <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                link_type = NA,
                                link_url = NA)
      }

      # creates a new tibble for project data, which can contain multiple entries
      # per Cristin result
      if("projects" %in% analyse){
        if("projects" %in% colnames(base_data)){
          projects <- base_data %>%
            dplyr::select(cristin_result_id, projects) %>%
            dplyr::filter(!purrr::map_lgl(projects, is.null)) %>%
            tidyr::unnest(cols = projects) %>%
            tidyr::gather(dplyr::starts_with("title"),
                          key = "language",
                          value = "project_code") %>%
            dplyr::select(-language) %>%
            dplyr::rename("project_url" = url)
        } else {
          projects <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                     cristin_project_id = NA,
                                     project_url = NA,
                                     project_code = NA)
        }
      } else {
        projects <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                   cristin_project_id = NA,
                                   project_url = NA,
                                   project_code = NA)
      }

      # creates a new tibble with journal identifiers,
      # which can be both printed and electronic
      if("journal.international_standard_numbers" %in% colnames(base_data)){
        journal_id <- base_data %>%
          dplyr::select(cristin_result_id, journal.international_standard_numbers) %>%
          dplyr::filter(!purrr::map_lgl(journal.international_standard_numbers, is.null)) %>%
          tidyr::unnest(cols = journal.international_standard_numbers) %>%
          dplyr::rename("journal_id_type" = type,
                        "journal_id" = value)
      } else {
        journal_id <- tibble::tibble(cristin_result_id = base_data[["cristin_result_id"]],
                                     journal_id_type = NA,
                                     journal_id = NA)
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
                      "journal.nvi_level"
        )

    }
    # save the whole dataset in the form of a list of tibbles
    data <- list(base_data = base_data,
                 funding_sources = funding_sources,
                 links = links,
                 projects = projects,
                 contributors = contributors)

    # combine the base data with the specified analysis

    if(all(analyse %in% c("base_data", "links", "projects",
                          "contributors", "funding_sources"))){
      output <- dplyr::left_join(data[["base_data"]],
                                 data[["links"]],
                                 by = c("cristin_result_id"))
      output <- dplyr::left_join(output,
                                 data[["projects"]],
                                 by = c("cristin_result_id"))
      output <- dplyr::left_join(output,
                                 data[["contributors"]],
                                 by = c("cristin_result_id"))
      output <- dplyr::left_join(output,
                                 data[["funding_sources"]],
                                 by = c("cristin_result_id"))
      output <- Filter(function(x) !all(is.na(x)), output)
      return(output)
    } else {
      stop('Error: Analysis parameters must be one of
           c("links", "projects", "contributors", "funding_sources").')
    }
  }

}
