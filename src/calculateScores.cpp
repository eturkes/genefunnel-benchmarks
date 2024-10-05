/*
 *    This file is part of genefunnel-benchmarks.
 *    Copyright (C) 2024  Emir Turkes, UK DRI at UCL
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *    Emir Turkes can be contacted at emir.turkes@eturkes.com
 */

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericMatrix calculateScores(
  const arma::sp_mat& orig_mat, CharacterVector row_names, List gene_ids
) {
  int ncol_mat = orig_mat.n_cols;
  int nrow_list = gene_ids.size();

  NumericMatrix mat(nrow_list, ncol_mat);

  std::unordered_map<std::string, uword> row_map;
  for (uword i = 0; i < row_names.size(); ++i) {
    row_map[as<std::string>(row_names[i])] = i;
  }

  for (int j = 0; j < ncol_mat; ++j) {
    for (int i = 0; i < nrow_list; ++i) {
      CharacterVector gene_set = gene_ids[i];
      std::vector<uword> indices;

      for (int m = 0; m < gene_set.size(); ++m) {
        std::string gene = as<std::string>(gene_set[m]);
        if (row_map.find(gene) != row_map.end()) {
          indices.push_back(row_map[gene]);
        }
      }

      vec idx_values(indices.size());
      for (size_t k = 0; k < indices.size(); ++k) {
        idx_values[k] = orig_mat(indices[k], j);
      }

      double total_sum = sum(idx_values);
      double var_value = 0.0;
      int n = idx_values.size();
      for (int i = 0; i < n; i++) {
        double leave_one_out_mean = (total_sum - idx_values[i]) / (n - 1);
        var_value += fabs(idx_values[i] - leave_one_out_mean);
      }

      double score = log(total_sum + 1) - (log(var_value + 1) / 2);
      double epsilon = 1e-9;
      if (fabs(score) < epsilon) {
        score = 0.0;
      }

      mat(i, j) = score;
    }
  }

  return mat;
}
