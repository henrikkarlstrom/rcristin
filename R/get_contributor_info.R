#' Get data on publication authors
#'
#' @param base_data data frame
#'
#' @return data frame
#' @export
#'
#' @examples
#' get_contributor_info(data.frame(
#' cristin_result_id = 1094009,
#' contributors_url = "https://api.cristin.no/v2/results/1094009/contributors")
#' )
get_contributor_info <- function(base_data){

  if(!"contributors_url" %in% names(base_data)){
    stop("There is no column called contributors_url here")
  }

  base_data <- dplyr::filter(
    base_data,
    !is.na(base_data[["contributors_url"]])
    )

  contributors <- lapply(
    unique(base_data[["contributors_url"]]),
    httr::GET
    )

  contributors <- lapply(
    contributors,
    function(x){
      tibble::as_tibble(
        jsonlite::fromJSON(
          httr::content(x, "text"),
          flatten = TRUE)
        )
      }
    )
  contributors <- dplyr::bind_rows(contributors)

  contributors <- dplyr::rename(
    contributors,
    "contributor_url" = "url",
    "cristin_result_id" = "result_id"
    )

  contributors <- tidyr::unnest(
    contributors,
    cols = c("affiliations"),
    keep_empty = TRUE
    )

  contributors <- dplyr::mutate(
    contributors,
    unit_name = dplyr::coalesce(
      !!!dplyr::select(contributors, dplyr::contains("unit_name")))
    )

  contributors <- dplyr::select(
    contributors,
    "cristin_result_id",
    "cristin_person_id",
    "first_name",
    "surname",
    "author_order" = "order",
    "role_code",
    "role_name" = "role.name.en",
    "cristin_institution_id" = "institution.cristin_institution_id",
    "institution_url" = "institution.url",
    "cristin_unit_id" = "unit.cristin_unit_id",
    "unit_url" = "unit.url",
    "unit_name",
    "contributor_url"
    )

  return(contributors)
}
