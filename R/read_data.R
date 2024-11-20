#' Read Volumetric Data
#'
#' Reads the volumetric data into a dataframe of a single variable, where each
#' row is the data at a single voxel. The positional data associated with each
#' row depends on how the file was generated, but the convention is to write
#' values from an inner z loop, to an outer x loop.
#'
#' @param result A named list where result$inp is the connection to the
#' file to read from, and result$out is an (initially) empty list to
#' write parsed data to.
#'
#' @returns A named list the same as its argument, with newly parsed
#' data.
read_data <- function(result) {
  vals                           <-
    result$inp                   |>
    read.table(fill = TRUE)      |>
    t()                          |>
    as.vector()                  |>
    (function(x) x[!is.na(x)])()

  result$out$data <- data.frame(vals)

  result
}
