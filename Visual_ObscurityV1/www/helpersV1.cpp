// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace std;

// Internal C++ function for calculating number of hits
// along each row and col for each pixel.
arma::mat sumRowCol(arma::mat idxArr){
  int nrow = idxArr.n_rows;
  int ncol = idxArr.n_cols;
  
  //create vectors of row and column sums.
  arma::rowvec colSum = arma::sum(idxArr, 0);
  arma::colvec rowSum = arma::sum(idxArr, 1);
  
  //Initialize output array
  arma::mat densArr(nrow, ncol);
  
  // Find row and column sum by adding respective values from vectors
  for( int r = 0; r < nrow; r++){
    for( int c = 0; c < ncol; c++){
      densArr(r,c) = colSum[c] + rowSum[r];
    }
  }
  
  return densArr;
}

// Internal C++ function for moving window sum.
// Also filters out low values.
arma::mat windowSum(arma::mat densArr, int windowSize){
  int nrow = densArr.n_rows;
  int ncol = densArr.n_cols;
  
  // Max density used in threshold.
  int maxDens = densArr.max();
  int sum = 0;
  //Initialize output array
  arma::mat outArr(nrow, ncol);
  
  //Select each pixel, providing buffer on edges for window.
  for( int r = windowSize; r < nrow - windowSize; r++){
    for( int c = windowSize; c < ncol - windowSize; c++){
      
      //Calculate window values
      sum = 0;
      for(int i = -windowSize; i <=windowSize; i++){
        for(int j = -windowSize; j <=windowSize; j++){
          sum = sum + densArr(r+i,c+j);
        }
      }

      // Filter out low values
      if( densArr(r,c) > maxDens/2){
        outArr(r,c) = sum;
      }else{
        outArr(r,c) = 0;
      }

    }
  }
  return outArr;
}


arma::mat findPanel( arma::mat densArr, int
                       densSens){
  arma::rowvec colSum = arma::sum(densArr, 0);
  arma::colvec rowSum = arma::sum(densArr, 1);
  
  int nrow = densArr.n_rows;
  int ncol = densArr.n_cols;
  
  int maxDensR = rowSum.max();
  int maxDensC = colSum.max();
  
  arma::mat outArr(nrow,ncol);
  
  
  for(int r = 0; r < nrow; r++){
    for(int c = 0; c < ncol; c++){
      if(rowSum[r] > maxDensR/densSens && 
         colSum[c] > maxDensC/densSens ){
        outArr(r,c) = 1;
        
      }
    }
  }
  
  return outArr;
}

// Calculates color hit densities by summing hits along each row and 
// column--the idea being that within the target panel, there are
// high densities in both row and col directions.

// [[Rcpp::export]]
arma::mat findDensity( arma::mat pixelIdx, vector<int> imageDims, 
                       int windowSize, int panelSens){
  // imageDims is (width, height)
  // initialize pixel array as (row = height, col = width)
  
  // Initiaialize Output Array
  arma::mat pixArr;
  pixArr.zeros(imageDims[1], imageDims[0]);
  
  // Number of hit pixels. pixelIdx are x,y coords from
  // countcolors package.
  int pixelIdxRows = pixelIdx.n_rows;

  // Mark hit pixels from coord list on output array
  for(int i = 0; i < pixelIdxRows; i++){
    pixArr(pixelIdx(i,0)-1, pixelIdx(i,1)-1) = 1;
  }
  
  // Sum hits for each pixel along row and column. See function for more.
  arma::mat densArr = sumRowCol(pixArr);
  
  // // Smooth irregularities with moving window
  // densArr = windowSum(densArr, windowSize);
  // 
  // 
  // densArr = findPanel(densArr, panelSens);

  return densArr;
}

// [[Rcpp::export]]
vector<int> panelCoords( arma::mat densArr, int sens){
  arma::rowvec colSum = arma::sum(densArr, 0);
  arma::colvec rowSum = arma::sum(densArr, 1);
  
  int nrow = densArr.n_rows;
  int ncol = densArr.n_cols;
  
  vector<int> coords = {0,0,0,0};
  
  for(int r = 0; r < nrow; r++){
    if(rowSum[r+1] > sens && rowSum[r] <= sens){ coords[3] = r + 1; }
    if(rowSum[r] > sens && rowSum[r+1] <= sens){ coords[1] = r + 1; }
  }
  for(int c = 0; c < ncol; c++){
    if(colSum[c+1] > sens && colSum[c] <= sens){ coords[0] = c + 1; }
    if(colSum[c] > sens && colSum[c+1] <= sens){ coords[2] = c + 1; }
  }

  
  return coords;
}
