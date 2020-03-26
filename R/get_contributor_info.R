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
#' contributors.url = "https://api.cristin.no/v2/results/1094009/contributors",
#' stringsAsFactors = FALSE)
#' )
get_contributor_info <- function(base_data){

  base_data <- dplyr::filter(
    base_data,
    !is.na(base_data[["contributors.url"]])
    )

  contributors <- lapply(
    unique(base_data[["contributors.url"]]),
    httr::GET
    )

  names(contributors) <- unique(base_data[["cristin_result_id"]])

  contributors <- lapply(
    contributors,
    function(x){
      tibble::as_tibble(
        jsonlite::fromJSON(
          httr::content(x, "text"),
          flatten = TRUE)
          ) %>%
      dplyr::bind_rows(.id = "cristin_result_id") %>%
      dplyr::rename("contributor_url" = url) %>%
      tidyr::unnest(
        cols = c(base_data[["affiliations"]]),
        keep_empty = TRUE
        )
  })


  return(contributors)
}
