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
List sens_slope_rcpp(NumericVector x, NumericVector t, double conf_level = 0.95) {
  int n = x.length();

  // varmk
  double tadjs = sum(t * (t - 1) * (2 * t + 5));
  double varS = (n * (n - 1) * (2 * n + 5) - tadjs) / 18;

  size_t n_pairs = static_cast<size_t>(n) * (n - 1) / 2;
  NumericVector d (n_pairs, 0.0);
  int k = 0;
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      d[k] = (x[j] - x[i]) / (j - i);
      k++;
    }
  }

  // Find median
  double b_sen;
  int mid = k / 2;
  std::nth_element(d.begin(), d.begin() + mid, d.end());
  if (k % 2 == 0) {
    std::nth_element(d.begin(), d.begin() + mid - 1, d.end());
    b_sen = (d[mid - 1] + d[mid]) / 2.0;
  } else{
    b_sen = d[mid];
  }

  double C = R::qnorm(1 - (1 - conf_level) / 2, 0, 1, TRUE, FALSE) * sqrt(varS);
  int rank_up = round((k + C) / 2 + 1);
  std::nth_element(d.begin(), d.begin() + rank_up, d.end());
  int rank_lo = round((k - C) / 2);
  std::nth_element(d.begin(), d.begin() + rank_lo, d.end());
  double lo = d[rank_lo];
  double up = d[rank_up];

  int S = mkScore(x, n);

  int sg = (S > 0) - (S < 0);
  double z = sg * (abs(S) - 1) / sqrt(varS);

  double pval = 2 * min(NumericVector::create(0.5, R::pnorm(abs(z), 0, 1, FALSE, FALSE)));

  NumericVector cint = {lo, up};

  List ans = List::create(
    Named("estimates") = NumericVector::create(Named("Sen's slope") = b_sen),
    Named("statistic") = NumericVector::create(Named("z") = z),
    Named("p.value") = pval,
    Named("parameter") = NumericVector::create(Named("n") = n),
    Named("conf.int") = cint
  );
  return(ans);
}
