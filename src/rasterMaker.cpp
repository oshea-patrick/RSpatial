#include <Rcpp.h>
#include <cmath>

double roundT(double num, int degree) {
  return (((int)(num * pow(10,degree+1))) / (int)(10)) / pow(10, degree);
}

bool isValid(double num, double range) {
  return (num >= 0 && num < range);
}

// [[Rcpp::export('.rasterMaker')]]
Rcpp::NumericVector rasterMaker(Rcpp::DataFrame df, int minX, int maxY, int nCols, int nRows, int sizeOfGen,int sizeOfList, int degree, bool block) {
  Rcpp::NumericVector out(nCols*pow(100, degree)*nRows);
  
  Rcpp::NumericVector lon_vector= df["lon"];
  Rcpp::NumericVector lat_vector= df["lat"];
  
  for (int g = 0; g < sizeOfGen*sizeOfList; g++) {
    int i = g % sizeOfGen;
    int mid = ((maxY - roundT(lat_vector[i], degree)) * (nCols * pow(100, degree))) + (abs((roundT(lon_vector[i], degree)) - minX) * pow(10, degree));
    out[mid]++;
    if (block && (mid % (int)(nCols*pow(10, degree)) != 0 && mid % (int)(nCols*pow(10, degree)) != 1)) {
      // down
      if (isValid(mid+(nCols*pow(10, degree)) , nCols*pow(100, degree)*nCols))
        out[mid + (int)(nCols*pow(10, degree))]++;
      // up
      if (isValid(mid-(nCols*pow(10, degree)) , nCols*pow(100, degree)*nCols))
        out[mid - (int)(nCols*pow(10, degree))]++;
      
      // right
      if (isValid(mid+1 , nCols*pow(100, degree)*nCols))
        out[mid+1]++;
      // down right
      if (isValid(mid+1+(nCols*pow(10, degree)) , nCols*pow(100, degree)*nCols))
        out[mid + (int)(nCols*pow(10, degree)) + 1]++;
      // up right
      if (isValid(mid- (nCols*pow(10, degree)) + 1 , nCols*pow(100, degree)*nCols))
        out[mid - (int)(nCols*pow(10, degree)) + 1]++;
      
      // left
      if (isValid(mid-1 , nCols*pow(100, degree)*nCols))
        out[mid-1]++;
      // up left
      if (isValid(mid - (nCols*pow(10, degree)) - 1 , nCols*pow(100, degree)*nCols))
        out[mid - (int)(nCols*pow(10, degree)) - 1]++;
      // down left
      if (isValid(mid+ (nCols*pow(10, degree)) - 1 , nCols*pow(100, degree)*nCols))
        out[mid + (int)(nCols*pow(10, degree)) - 1]++;
      
      
    } 
  }
  return out;
}

// [[Rcpp::export('.rasterMakerAdjust')]]
Rcpp::NumericVector rasterMakerAdjust(Rcpp::DataFrame df, int minX, int maxY, int nCols, int nRows, int sizeOfGen,int sizeOfList, int degree, bool block, int horizontal, int vertical) {
  Rcpp::NumericVector out(nCols*pow(100, degree)*nRows);
  
  Rcpp::NumericVector lon_vector= df["lon"];
  Rcpp::NumericVector lat_vector= df["lat"];
  
  for (int g = 0; g < sizeOfGen*sizeOfList; g++) {
    int i = g % sizeOfGen;
    int mid = ((maxY - roundT(lat_vector[i], degree)) * (nCols * pow(100, degree))) + (abs((roundT(lon_vector[i], degree)) - minX) * pow(10, degree)) + horizontal + (int)(vertical * nCols * pow(10, degree));
    if (isValid(mid, nCols*pow(100, degree)*nCols))
      out[mid]++;
    if (block && (mid % (int)(nCols*pow(10, degree)) != 0 && mid % (int)(nCols*pow(10, degree)) != 1)) {
      // down
      if (isValid(mid+(nCols*pow(10, degree)) , nCols*pow(100, degree)*nCols))
        out[mid + (int)(nCols*pow(10, degree))]++;
      // up
      if (isValid(mid-(nCols*pow(10, degree)) , nCols*pow(100, degree)*nCols))
        out[mid - (int)(nCols*pow(10, degree))]++;
      
      // right
      if (isValid(mid+1 , nCols*pow(100, degree)*nCols))
        out[mid+1]++;
      // down right
      if (isValid(mid+1+(nCols*pow(10, degree)) , nCols*pow(100, degree)*nCols))
        out[mid + (int)(nCols*pow(10, degree)) + 1]++;
      // up right
      if (isValid(mid- (nCols*pow(10, degree)) + 1 , nCols*pow(100, degree)*nCols))
        out[mid - (int)(nCols*pow(10, degree)) + 1]++;
      
      // left
      if (isValid(mid-1 , nCols*pow(100, degree)*nCols))
        out[mid-1]++;
      // up left
      if (isValid(mid - (nCols*pow(10, degree)) - 1 , nCols*pow(100, degree)*nCols))
        out[mid - (int)(nCols*pow(10, degree)) - 1]++;
      // down left
      if (isValid(mid+ (nCols*pow(10, degree)) - 1 , nCols*pow(100, degree)*nCols))
        out[mid + (int)(nCols*pow(10, degree)) - 1]++;
      
      
    } 
  }
  return out;
}
