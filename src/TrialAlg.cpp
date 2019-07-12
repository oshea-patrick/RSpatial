#include <Rcpp.h>
#include <iostream>	
#include <ctime>	
#include <cstdlib>	


// returns a lon and lat pair (haven't decided yet which order)
// the indices of lon lat and the values must all be corresponding
// [[Rcpp::export('.getAVN')]]
Rcpp::NumericVector getAVN(double value, Rcpp::NumericVector lon, Rcpp::NumericVector lat, Rcpp::NumericVector values, int size) {
  Rcpp::NumericVector out(2);
  for (int i = 0; i < size; i++) {
    if (values[i] >= value) {
      out[0] = lat[i];
      out[1] = lon[i];
      break;
    }
  }
  return out;
}

// The passed in arrays go lat as the [0] index and lon as the [1] index
// [[Rcpp::export('.mutateNumber')]]
Rcpp::NumericVector mutateNumber(Rcpp::NumericVector one, Rcpp::NumericVector two) {
  Rcpp::NumericVector out(2);
  switch (rand() % 8) {
  // normal cases
  case 0:
    out[0] = one[0];
    out[1] = one[1];
    break;
  case 1:
    out[0] = two[0];
    out[1] = two[1];
    break;
  case 2:
    out[0] = one[0];
    out[1] = two[1];
    break;
  case 3:
    out[0] = two[0];
    out[1] = one[1];
    break;
    
    
  case 4:
    out[0] = one[0];
    out[1] = (one[1] + two[1]) /2.0;
    break;
  case 5:
    out[0] = two[0];
    out[1] = (one[1] + two[1]) /2.0;
    break;
  case 6:
    out[0] = (one[0] + two[0]) /2.0;
    out[1] = one[1];
    break;
  case 7:
    out[0] = (one[0] + two[0]) /2.0;
    out[1] = two[1];
    break;  
  }
  return out;
}
// Expects an array of pointers containing latitude and longitude as 
// [[Rcpp::export('.createNextGen')]]
Rcpp::DataFrame createNextGen(Rcpp::DataFrame df, Rcpp::NumericVector avnValues, double mutationRate, int explore, int size, Rcpp::NumericVector randoms) {
  Rcpp::NumericVector lon_vector= df["lon"];
  Rcpp::NumericVector lat_vector= df["lat"];
  Rcpp::NumericVector out_lon(size);
  Rcpp::NumericVector out_lat(size);
  
  for (int g = 0; g < size; g++) {
    srand(randoms[g]);
    long random = rand() % 100 +1;
    //std::cout << random;
  //  std::cout << "\n";
    
    long one = rand();
    long two = rand();
    Rcpp::NumericVector temp = mutateNumber(getAVN((one % 100000)/100000.0, lon_vector, lat_vector, avnValues, size), getAVN((two % 100000)/100000.0, lon_vector, lat_vector, avnValues, size));
    
    if (random < mutationRate) {
      out_lat[g] = temp[0] +  (rand() % (2*explore) - explore);
      out_lon[g] = temp[1] + (rand() % (2*explore) - explore) ;
    }
    else {
      out_lat[g] = temp[0];
      out_lon[g] = temp[1];
    }
  }
  
  Rcpp::DataFrame toBeReturned = 
    Rcpp::DataFrame::create(Rcpp::Named("lon")=out_lon,
                            Rcpp::Named("lat")=out_lat);
  
  return toBeReturned;
}