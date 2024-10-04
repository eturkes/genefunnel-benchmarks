#    This file is part of genefunnel-benchmarks.
#    Copyright (C) 2024  Emir Turkes, UK DRI at UCL
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Emir Turkes can be contacted at emir.turkes@eturkes.com

# This file holds common functions and methods.

genefunnel <- function(mat, gene_sets) {

  if (!inherits(mat, "sparseMatrix")) {
    mat <- Matrix(mat, sparse = TRUE)
  }

  scores <- calculateScores(mat, rownames(mat), gene_sets)
  rownames(scores) <- names(gene_sets)
  colnames(scores) <- colnames(mat)

  return(scores)
}
