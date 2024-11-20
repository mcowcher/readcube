# readcube - A Gaussian Cubefile reader for R
# Copyright (C) 2024  Moss Cowcher
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' Read a Gaussian Cubefile
#' @export
read_cube <- function(path) {
  # Read file line by line (big files)
  list(
    inp = file(path, "r"),
    out = list()
  )                         |>
    read_header()           |>
    read_data()             |>
    (function(x) {
      close(x$inp)
      x$out
    })()
}

calc_coords <- function(cube) {
  x_max <- cube$grid$x$n_voxel
  y_max <- cube$grid$y$n_voxel
  z_max <- cube$grid$z$n_voxel

  cube$data$x <- 1:x_max |> rep(each = y_max * z_max)
  cube$data$y <- 1:y_max |> rep(each = z_max, times = x_max)
  cube$data$z <- 1:z_max |> rep(times = y_max * x_max)

  cube
}

test <- function() {
  fp <- file("dens.0.cube")
  open(fp, "r")
  out <- read_header(list(inp = fp, out = list()))
  close(fp)
  out
}
