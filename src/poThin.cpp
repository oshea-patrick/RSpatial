#include<iostream>
#include<cmath>
#include<string>
#include <Rcpp.h>


// [[Rcpp::export('.getDist')]]
float getDist(float lon1, float lat1, float lon2, float lat2) {
  float R = 6378.137;
  float toRad = 3.14159/180;
  
  lon1 = lon1 * toRad;
  lon2 = lon2 * toRad;
  lat1 = lat1 * toRad;
  lat2 = lat2 * toRad;
  float dlon = lon2 - lon1;
  float dlat = lat2 - lat1;
  
  float a = std::pow(std::sin(dlat / 2), 2) + (std::cos(lat1) * std::cos(lat2) * std::pow(std::sin(dlon / 2),2));
  return (2 * std::atan2(std::sqrt(a), std::sqrt(1 - a)) * R);
}

// [[Rcpp::export('.poThin')]]
Rcpp::NumericVector poThin(Rcpp::DataFrame df, double spacing, int dimension, std::string lon, std::string lat) {
  // creates Distance Matrix and all of the variables to keep track of indices
  float** distMatrix = new float*[dimension];
  int index  = 0;
  int* to_be_deleted = new int[dimension];
  Rcpp::NumericVector lon_vector= df[lon];
  Rcpp::NumericVector lat_vector= df[lat];
  
  for (int i = 0; i< dimension; i++) {
    distMatrix[i] = new float[dimension];
  }
  
  // creates distances and initializes diagonals to -1 for pruning purposes
  for (int i = 0; i < dimension; i++) {
    for (int g = 0; g < dimension; g++) {
      if (g > i)
        distMatrix[i][g] = getDist(lon_vector[i], lat_vector[i], lon_vector[g], lat_vector[g]);
      else
        distMatrix[i][g] = -1;
    }
  }
  
  // figures out which points will be deleted
  for (int col = 0; col < dimension; col++) {
    for (int row  = col + 1; row < dimension; row++) {
      if (distMatrix[col][row] != -1 && distMatrix[col][row] > spacing){
        
        for (int match = 0; match < dimension; match++) {
          if (distMatrix[row][match] != -1 && distMatrix[row][match] <= spacing) {
            
            for (int i = 0; i < dimension; i++) {
              distMatrix[row][i] = -1;
              distMatrix[i][row] = -1;
            }
            
            to_be_deleted[index] = row;
            index++;
            break;
            
          }
        }
        
      }
    }
  }
  
  // converts to_be_deleted into a numeric matrix
  Rcpp::NumericVector out(index);
  for (int i = 0; i < index; i++)
    out[i] = to_be_deleted[i]+1;
  
  // turn array into vector
  return out;
}
