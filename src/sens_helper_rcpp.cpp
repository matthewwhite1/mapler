#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int mkScore(NumericVector x, int n) {
  int S = {0};
  for (int j = 0; j < n; ++j) {
    for (int k = 0; k <= j; ++k) {
      double diff = x[j] - x[k];
      S += (diff > 0) - (diff < 0);
    }
  }
  return(S);
}

// [[Rcpp::export]]
NumericVector sens_slope_rcpp(NumericVector x, NumericVector t, double conf_level = 0.95) {
  // Get size of x
  int n = x.length();

  // Get Mann-Kendall variance
  double tadjs = sum(t * (t - 1) * (2 * t + 5));
  double varS = (n * (n - 1) * (2 * n + 5) - tadjs) / 18;

  // Calculate slope between each pair
  size_t n_pairs = static_cast<size_t>(n) * (n - 1) / 2;
  NumericVector d (n_pairs, 0.0);
  int k = 0;
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      d[k] = (x[j] - x[i]) / (j - i);
      k++;
    }
  }

  // Find median slope
  double b_sen;
  int mid = k / 2;
  std::nth_element(d.begin(), d.begin() + mid, d.end());
  if (k % 2 == 0) {
    std::nth_element(d.begin(), d.begin() + mid - 1, d.end());
    b_sen = (d[mid - 1] + d[mid]) / 2.0;
  } else{
    b_sen = d[mid];
  }

  // Find confidence interval bounds
  double C = R::qnorm(1 - (1 - conf_level) / 2, 0, 1, TRUE, FALSE) * sqrt(varS);
  int rank_up = round((k + C) / 2 + 1);
  std::nth_element(d.begin(), d.begin() + rank_up, d.end());
  int rank_lo = round((k - C) / 2);
  std::nth_element(d.begin(), d.begin() + rank_lo, d.end());
  double lo = d[rank_lo];
  double up = d[rank_up];
  NumericVector cint = {lo, up};

  // Calculate z score
  int S = mkScore(x, n);
  int sg = (S > 0) - (S < 0);
  double z = sg * (abs(S) - 1) / sqrt(varS);

  // Calculate p-value
  double pval = 2 * min(NumericVector::create(0.5, R::pnorm(abs(z), 0, 1, FALSE, FALSE)));

  // Return stuff
  double n_double = n;
  NumericVector ans = {b_sen, z, pval, n_double, cint[0], cint[1]};

  return(ans);
}
