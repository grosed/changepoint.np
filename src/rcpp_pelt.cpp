#include "Rcpp.h"
using namespace Rcpp;

#include <vector>
#include <string>
#include <cstring>

#include "pelt.h"


//[[Rcpp::export]]
List rcpp_pelt(std::vector<double>& sumstats, double& beta, int& nquantiles)
{

  int n = sumstats.size()/nquantiles - 1;
  std::vector<double> lastchangelike(n+1);
  std::vector<int> lastchangecpts(n+1);
  std::vector<int> numchangecpts(n+1);
  std::vector<int> cptsout(n+1); 
  int error = 0;
  int minseglen = 1;
  char** dummy;  
  pelt::pelt(dummy,&sumstats[0],&n,&beta,&cptsout[0],&error,&minseglen,&nquantiles,&lastchangelike[0],&lastchangecpts[0],&numchangecpts[0]);

  return List::create(cptsout,lastchangelike[n]);
  
}
