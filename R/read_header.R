#' Read the header of a cubefile
#'
#' Parses the cubefile header
read_header <- function(result) {
  result                     |>
    read_comment()           |>
    read_n_atom_and_origin() |>
    read_voxels()            |>
    read_atoms()
}

#' Read Cubefile header comment
#'
#' Read the first two lines from a cubefile, which are the comment.
#' This should be called before any other parsing function.
#'
#' @param result A named list where result$inp is the connection to the
#' file to read from, and result$out is an (initially) empty list to
#' write parsed data to.
#'
#' @returns A named list the same as its argument, with newly parsed
#' data.
read_comment <- function(result) {
  result$out$comment <- readLines(result$inp, n = 2)
  result
}

#' Read line 3 of A Cubefile
#'
#' Reads the third line of a cubefile, which contains the number of
#' atoms and the position of the origin
#'
#' @param result A named list where result$inp is the connection to the
#' file to read from, and result$out is an (initially) empty list to
#' write parsed data to.
#'
#' @returns A named list the same as its argument, with newly parsed
#' data.
read_n_atom_and_origin <- function(result) {
  line <- readLines(result$inp, n = 1) |>
    trimws()                    |>
    strsplit("\\s+")            |>
    unlist()                    |>
    as.numeric()

  result$out$n_atoms <- line[1]
  result$out$origin <- line[2:4]
  result
}

#' Read Voxels
#'
#' Reads the axis directions and the number of voxels along each one
#'
#' @param result A named list where result$inp is the connection to the
#' file to read from, and result$out is an (initially) empty list to
#' write parsed data to.
#'
#' @returns A named list the same as its argument, with newly parsed
#' data.
read_voxels <- function(result) {
  grid <-
    readLines(result$inp, n = 3) |>
    trimws()                     |>
    strsplit("\\s+")             |>
    lapply(as.numeric)           |>
    lapply(function(x) {
      list(
        n_voxel = head(x, 1),
        axis = tail(x, -1)
      )
    })

  names(grid) <- c("x", "y", "z")
  result$out$grid <- grid
  result
}

#' Read Atom Geometry
#'
#' Reads the atom data, i.e. atomic number, charge and position for every
#' atom listed
#'
#' @param result A named list where result$inp is the connection to the
#' file to read from, and result$out is an (initially) empty list to
#' write parsed data to.
#'
#' @returns A named list the same as its argument, with newly parsed
#' data.
read_atoms <- function(result) {
  result$out$atoms <-
    readLines(result$inp, n = result$out$n_atoms) |>
    trimws() |>
    strsplit("\\s+") |>
    lapply(as.numeric) |>
    lapply(function(x) {
      list(
        atomic_number = x[1],
        charge = x[2],
        position = x[3:5]
      )
    })

  result
}
