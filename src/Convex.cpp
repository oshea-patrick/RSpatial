#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export('.isInsideConvexHull')]]
bool isInsideConvexHull(Rcpp::DataFrame hulls, double checkx, double checky, int size) {
  if (size < 3)
    return false;
  Rcpp::NumericVector hullx= hulls["lon"];
  Rcpp::NumericVector hully= hulls["lat"];
  
  double r_x;
  double r_y;
  double q_x;
  double q_y;
  double p_x;
  double p_y;
  double calc;
  
  for (int i = 0; i < size-1; i++) {
    
    r_x = checkx;
    r_y = checky;
    q_x = hullx[i+1];
    q_y = hully[i+1];
    p_x = hullx[i];
    p_y = hully[i];
    calc = (r_y-q_y)*(q_x-p_x)-(q_y-p_y)*(r_x-q_x);
    //std::cout << calc << "\n";
    if (calc == 0)
      return true;
    if (calc > 0)
      return false;
  }
  
  q_x = hullx[0];
  q_y = hully[0];
  p_x = hullx[size-1];
  p_y = hully[size-1];
  calc = (r_y-q_y)*(q_x-p_x)-(q_y-p_y)*(r_x-q_x);
  //std::cout << calc << "\n";
  if (calc == 0)
    return true;
  if (calc > 0)
    return false;
  
  return true;
}
