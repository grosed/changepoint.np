#ifndef ___OP_PRUNED_H___
#define ___OP_PRUNED_H___

#include <vector>
#include <algorithm>

#include <thread>
#include <chrono>
#include <future>


#include "sumstats.h"

namespace changed
{
  namespace methods
  {
      template <typename Ftype>
      // requires std::invocable<Ftype,const int&, const int&>
      std::vector<int> op_pruned(const Ftype& cf,
				 const double& beta,
				 const int& n,
				 std::shared_future<void> futureobj)
      {
	try
	  {
	    std::vector<double> F(n+1);
	    F[0] = -beta;
	    std::vector<int>  cpts(n);
	    std::vector<double> C(n);
	    std::vector<int> R(n);
	    R[0] = 0;
	    int ncands = 1;
	    for(int tstar = 1; tstar <= n; tstar++)
	      {
		if(futureobj.valid() && !(futureobj.wait_for(std::chrono::milliseconds(0)) == std::future_status::timeout))
		  {
		    throw std::exception();
		  }
		std::transform(std::begin(R),std::begin(R) + ncands,std::begin(C),[&](const auto& t){return F[t] + cf(t+1,tstar) + beta;});
		auto it = std::min_element(std::begin(C), std::begin(C) + ncands);
		F[tstar] = *it;
		cpts[tstar-1] = tstar - R[std::distance(std::begin(C),it)];
		int j = ncands;
		ncands = 0;
		for(int i = 0;i < j; i++)
		  {
		    if(F[R[i]] + cf(R[i]+1,tstar) <= F[tstar] + beta)
		      {
			R[ncands++] = R[i];
		      }
		  }
		R[ncands++] = tstar;
	      }	
	    return cpts;
	  }
	catch(const std::exception& e)
	  {
	    std::vector<int> empty;
	    return(empty); // interrupted - send back an empty container      
	  }
      }
      
  } // namespace methods
} // namespace changed


#endif
