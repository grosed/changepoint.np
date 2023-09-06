#include <Rcpp.h>

#include <vector>
#include <list>
#include <memory>
#include <thread>
#include <chrono>
#include <future>


#include "normal.mean.h"
#include "np.conditional.h"
#include "np.average.h"
#include "np.max.h"
#include "op.pruned.h"


template <typename Ftype, typename... Args>
std::vector<int> run_method(Ftype& f,Args... args)
{
  // Create a std::promise object
  std::promise<void> exitSignal;
  // fetch std::future object associated with promise
  std::shared_future<void> futureObj = exitSignal.get_future();
  // start worker thread
  auto future = std::async(std::launch::async,f,args...,std::move(futureObj));
  // check for user interrupt and lack of resources and rethrow to R if necessary 
  try
    {
      while(std::future_status::ready != future.wait_for(std::chrono::milliseconds(0)))
	{
	  Rcpp::checkUserInterrupt();
	}
    }  
  catch(std::bad_alloc &e)
    {
      exitSignal.set_value();
      auto result = future.get(); // wait for it to tidy up
      Rcpp::stop("insufficient memory");
    }
  catch(...)
    {
      exitSignal.set_value();
      auto result = future.get(); // wait for it to tidy up
      Rcpp::stop("user interrupt");
    }
  // return the result
  return future.get();
}


template <typename cftype>
struct model
{
  int m_n;
  double m_penalty;
  std::shared_ptr<cftype> m_sp_cf;

  model() {};
  
  template <typename Arg1, typename... Args>
  void setcost(const Arg1& data, Args... args)
  {
    m_n = data.size();
    m_sp_cf = std::shared_ptr<cftype>(new cftype(data,args...));
  }

  void setpenalty(const double& penalty)
  {
    m_penalty = penalty;
  }

  double penalty()
  {
    return m_penalty;
  }
  
  double cost(const int& i,const int& j) const
  {
    return (*m_sp_cf)(i,j);
  }

  std::vector<int> changepoints()
  {
    return run_method(changed::methods::op_pruned<cftype>,*m_sp_cf,m_penalty,m_n);
  }

};


typedef model<changed::cost::normal::mean> normal_mean;
typedef model<changed::cost::np::average> np_average;
typedef model<changed::cost::np::conditional> np_conditional;
typedef model<changed::cost::np::max> np_max;


RCPP_MODULE(changed){
    using namespace Rcpp;
    class_<normal_mean>("normal_mean")
      .constructor()
      .method("cost", &normal_mean::cost ,"cost")
      .method("setcost", &normal_mean::setcost<std::vector<double> > , "set cost")
      .method("setpenalty", &normal_mean::setpenalty , "set penalty")
      .method("penalty", &normal_mean::penalty , "get penalty")
      .method("changepoints", &normal_mean::changepoints , "get changepoints")
    ;
    class_<np_average>("np_average")
      .constructor()
      .method("cost", &np_average::cost ,"cost")
      .method("setcost", &np_average::setcost<std::vector<double>,std::vector<double> > , "set cost")
      .method("setpenalty", &np_average::setpenalty , "set penalty")
      .method("penalty", &np_average::penalty , "get penalty")
      .method("changepoints", &np_average::changepoints , "get changepoints")
    ;
    class_<np_conditional>("np_conditional")
      .constructor()
      .method("cost", &np_conditional::cost ,"cost")
      .method("setcost", &np_conditional::setcost<std::vector<double>,std::vector<double> > , "set cost")
      .method("setpenalty", &np_conditional::setpenalty , "set penalty")
      .method("penalty", &np_conditional::penalty , "get penalty")
      .method("changepoints", &np_conditional::changepoints , "get changepoints")
    ;
    class_<np_max>("np_max")
      .constructor()
      .method("cost", &np_max::cost ,"cost")
      .method("setcost", &np_max::setcost<std::vector<double>,std::vector<double> > , "set cost")
      .method("setpenalty", &np_max::setpenalty , "set penalty")
      .method("penalty", &np_max::penalty , "get penalty")
      .method("changepoints", &np_max::changepoints , "get changepoints")
    ;
	
}
 

