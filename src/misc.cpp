#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat rotate(
  const arma::mat & mat,
  const arma::vec & origin,
  double alpha
) {

  // Rotation matrix
  arma::mat R(2, 2);
  R(0,0) = cos(alpha);
  R(1,0) = -sin(alpha);
  R(0,1) = -R(1,0);
  R(1,1) = R(0,0);

  arma::mat centered_mat = mat;
  centered_mat.col(0) -= origin[0];
  centered_mat.col(1) -= origin[1];

  // centered_mat = (R * centered_mat.t()).t();
  centered_mat = centered_mat * R;

  centered_mat.col(0) += origin[0];
  centered_mat.col(1) += origin[1];

  return centered_mat;
}


/***R

mat <- cbind(runif(100), runif(100))
a   <- pi/1.52
origin <- c(.5,-.5)

ans0 <- polygons::rotate(mat, origin, a)
ans1 <- rotate(mat, origin, a)

identical(ans0, ans1)

plot(ans0, xlim = c(-2,2), ylim = c(-2, 2))
points(rbind(origin), pch=3, cex=20)
points(mat, col="red")

library(microbenchmark)
microbenchmark(
  polygons::rotate(mat, origin, a),
  rotate(mat, origin, a),
  times = 1e4
)
*/
