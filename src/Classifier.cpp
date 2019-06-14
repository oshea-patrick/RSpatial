#include <Rcpp.h>
#include <string>

// [[Rcpp::export]]
Rcpp::NumericVector classifySpatiallyByBlocks(Rcpp::DataFrame df, std::string lon, std::string lat, int size) {
  //takes in Dataframe and splits it into latitude and longtude cols
  Rcpp::NumericVector out(size);
  // lon goes from 0 to -180 and 0 to 180, at 180 they converge
  // 37 across or 0 through 36 inclusive
  Rcpp::NumericVector lon_vector= df[lon];
  // lat goes from -90 to 90
  // 19 across or 0 through 18 inclusive
  Rcpp::NumericVector lat_vector= df[lat];
  
  // assign in linear time
  int current_index = 0;
  
  for (int i =0; i < size; i++) {
    // first classfiy by longitude
    current_index += std::abs(lon_vector[i]/10);
    // then classify by latitude
    current_index += std::abs(19*(lat_vector[i]/10));
    //store value
    out[i] = current_index;
    // reset current_index
    current_index = 0;
  }
  return out;
}

