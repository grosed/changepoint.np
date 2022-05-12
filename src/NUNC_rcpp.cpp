#include <Rcpp.h>
#include <boost/circular_buffer.hpp>
#include <tuple>
#include "NUNC_step.h"

// [[Rcpp::depends(BH)]]    

using namespace Rcpp;


// [[Rcpp::export(.stepGlobal)]]
List NUNCstepGlobal (std::vector<double>& data, List pastInfo, const double threshold, const std::vector<double> quantiles) {
  
  boost::circular_buffer<double> cb(data.size());
  
  for (auto d : data) {
    cb.push_back(d);
  }
  
  Info I = {pastInfo[0], pastInfo[1], pastInfo[2], pastInfo[3]};
  
  I = nuncLongMem(cb, std::move(I), threshold, quantiles);
  

  return List::create(Rcpp::Named("t") = I.t,
                      Rcpp::Named("changepoint") = I.changepoint,
                      Rcpp::Named("zVals") = I.zVals);
}


// [[Rcpp::export(.stepLocal)]]
long long NUNCstepLocal (std::vector<double>& data, const double threshold, const std::vector<double> quantiles, const std::vector<long>& grid) {
  
  boost::circular_buffer<double> cb(data.size());
  
  for (auto d : data) {
    cb.push_back(d);
  }
  return nuncOnline(cb, threshold, quantiles, grid);
}



// [[Rcpp::export(.NUNCGlobal)]]
List NUNCGlobal (Rcpp::Function dataGen, const long w, const double beta, const long K) {

    boost::circular_buffer<double> cb(w); // this is the window
  
  auto c = -log(2 * (double)w - 1.0);
  auto thres = (double)K * beta / (-c);
  std::vector<double> quantiles(K);

  Info I = {0, -1, {}};
  
  while (true) {
    if (I.t < w) {
      cb.push_back(Rcpp::as<double>(dataGen()));
      I.t ++;
      
    } else if (I.t == w) {
      cb.push_back(Rcpp::as<double>(dataGen()));
      quantiles = quantilesRcpp(cb, K, w, c); // here we initialize and we're ready to go
      I.zVals = initCDF(cb, quantiles);
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      
    } else {
      cb.push_back(Rcpp::as<double>(dataGen()));
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      if (I.changepoint != -1) {
        break;
      }
    }
  }
  
  return List::create(Rcpp::Named("t") = I.t,
                      Rcpp::Named("changepoint") = I.changepoint,
                      Rcpp::Named("zVals") = I.zVals,
                      Rcpp::Named("method") = "global");
    
}


// [[Rcpp::export(.NUNCLocal)]]
List NUNCLocal (Rcpp::Function dataGen, const long w, const double beta, const long K, const std::vector<long>& grid) {
  boost::circular_buffer<double> cb(w); // this is the window
  
  auto c = -log(2 * (double)w - 1.0);
  auto thres = (double)K * beta / (-c);
  std::vector<double> quantiles(K);
  
  
  // here you run the offline algo
  size_t t = 0;
  long long changepoint = -1; 
  while(true) {
    if (t < w) {
      cb.push_back(Rcpp::as<double>(dataGen()));
      t ++;
    } else {
      cb.push_back(Rcpp::as<double>(dataGen()));
      quantiles = quantilesRcpp(cb, K, w, c);
      //changepoint = nuncOnline(cb, thres, quantiles);
      changepoint = nuncOnline(cb, thres, quantiles, grid);
      t++;
      if (changepoint != -1) {
        changepoint += (t - w);
        break;
      }
    }
  }
  
  return List::create(Rcpp::Named("t") = t,
                      Rcpp::Named("changepoint") = changepoint,
                      Rcpp::Named("method") = "local");
  
}


// [[Rcpp::export(.NUNCsemiParam)]]
List NUNCSemiParametric (Rcpp::Function dataGen, const long w, const double beta, std::vector<double> quantiles) {
  
  boost::circular_buffer<double> cb(w); // this is the window
  
  auto K = quantiles.size();
  auto c = -log(2 * (double)w - 1.0);
  auto thres = (double) K * beta / (-c);

  Info I = {0, -1, {}};
  
  while (true) {
    if (I.t < w) {
      cb.push_back(Rcpp::as<double>(dataGen()));
      I.t ++;
      
    } else if (I.t == w) {
      cb.push_back(Rcpp::as<double>(dataGen()));
      I.zVals = initCDF(cb, quantiles);
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      
    } else {
      cb.push_back(Rcpp::as<double>(dataGen()));
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      if (I.changepoint != -1) {
        break;
      }
    }
  }
  
  return List::create(Rcpp::Named("t") = I.t,
                      Rcpp::Named("changepoint") = I.changepoint,
                      Rcpp::Named("zVals") = I.zVals,
                      Rcpp::Named("method") = "semi-param");
  
}



// [[Rcpp::export(.NUNCGlobalOffline)]]
List NUNCGlobalOffline (std::vector<double>& data, const long w, const double beta, const long K) {
  
  boost::circular_buffer<double> cb(w); // this is the window
  
  auto c = -log(2 * (double)w - 1.0);
  auto thres = (double)K * beta / (-c);  // TODO: Nuke this
  std::vector<double> quantiles(K);
  std::list<double> Q_history;


  Info I = {0, -1, 0, {}};
  
  for (auto x : data) {
    if (I.t < w) {
      cb.push_back(x);
      I.t ++;
      Q_history.push_back(I.Q);

    } else if (I.t == w) {
      cb.push_back(x);
      quantiles = quantilesRcpp(cb, K, w, c); // here we initialize and we're ready to go
      I.zVals = initCDF(cb, quantiles);
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      Q_history.push_back(I.Q);
    } else {
      cb.push_back(x);
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      Q_history.push_back(I.Q);
      if (I.changepoint != -1) {
        break;
      }
    }
  }
  
  return List::create(Rcpp::Named("t") = I.t,
                      Rcpp::Named("changepoint") = I.changepoint,
                      Rcpp::Named("zVals") = I.zVals,
                      Rcpp::Named("Qhistory") = Q_history,
                      Rcpp::Named("method") = "global");
  
}


// [[Rcpp::export(.NUNCLocalOffline)]]
List NUNCLocalOffline (std::vector<double>& data, const long w, const double beta, const long K, const std::vector<long>& grid) {
  
  boost::circular_buffer<double> cb(w); // this is the window
  
  auto c = -log(2 * (double)w - 1.0);
  auto thres = (double)K * beta / (-c);
  std::vector<double> quantiles(K);
  
  size_t t = 0;
  long long changepoint = -1; 
  for (auto x : data) {
    if (t < w) {
      cb.push_back(x);
      t ++;
    } else {
      cb.push_back(x);
      quantiles = quantilesRcpp(cb, K, w, c);
      // changepoint = nuncOnline(cb, thres, quantiles);
      changepoint = nuncOnline(cb, thres, quantiles, grid);
      t++;
      if (changepoint != -1) {
        changepoint += (t - w);
        break;
      }
    }
  }
  
  return List::create(Rcpp::Named("t") = t,
                      Rcpp::Named("changepoint") = changepoint,
                      Rcpp::Named("method") = "local");
}


// [[Rcpp::export(.NUNCsemiParamOffline)]]
List NUNCsemiParametricOffline (std::vector<double>& data, const long w, const double beta, std::vector<double> quantiles) {
  
  boost::circular_buffer<double> cb(w); // this is the window
  auto K = quantiles.size();
  auto c = -log(2 * (double)w - 1.0);
  auto thres = (double)K * beta / (-c);

  // here you run the offline algo
  Info I = {0, -1, {}};
  
  for (auto x : data) {
    if (I.t < w) {
      cb.push_back(x);
      I.t ++;
      
    } else if (I.t == w) {
      cb.push_back(x);
      I.zVals = initCDF(cb, quantiles);
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      
    } else {
      cb.push_back(x);
      I = nuncLongMem(cb, std::move(I), thres, quantiles);
      if (I.changepoint != -1) {
        break;
      }
    }
  }
    
  return List::create(Rcpp::Named("t") = I.t,
                      Rcpp::Named("changepoint") = I.changepoint,
                      Rcpp::Named("zVals") = I.zVals,
                      Rcpp::Named("method") = "semi-param");
}
