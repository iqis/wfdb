.pb_index_url = "http://physionet.org/physiobank/database/"

#' Get PhysioBank data catalog
#'
#' @return tibble
#' @export
#'
#' @examples
#' pb_catalog()
pb_catalog <- function(){suppressWarnings({
  httr::GET(file.path(.pb_index_url, "DBS")) %>%
    httr::content("text", encoding = "UTF-8") %>%
    stringr::str_replace_all("\t{3,}", "\t") %>%
    readr::read_tsv(col_names = FALSE) %>%
    magrittr::set_colnames(c("name", "description"))
})}