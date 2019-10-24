#' Get data on publication authors
#'
#' @param base_data data frame
#'
#' @return data frame
#' @export
#'
#' @examples
#' base_data <- data.frame(cristin_result_id = 1094009, contributors.url = "https://api.cristin.no/v2/results/1094009/contributors")
#' get_contributor_info(base_data)
get_contributor_info <- function(base_data){
  contributors <- lapply(unique(base_data[["contributors.url"]]),
                         httr::GET)

  names(contributors) <- unique(base_data[["cristin_result_id"]])

  contributors <- lapply(contributors, function(x)
    tibble::as_tibble(jsonlite::fromJSON(
      httr::content(x, "text"), flatten = TRUE))) %>%
    dplyr::bind_rows(.id = "cristin_result_id") %>%
    dplyr::rename("contributor_url" = url) %>%
    tidyr::unnest(cols = c(affiliations))

  return(contributors)
}
