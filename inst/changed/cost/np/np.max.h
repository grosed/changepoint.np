#ifndef ___NP_MAX_H___
#define ___NP_MAX_H___

#include <vector>
#include <cmath>
#include <limits>

namespace changed
{
  namespace cost
  { 
    namespace np
    {      
      template <typename Rtype,typename Ctype>
	struct max_template
	{
	  std::vector<std::vector<Ctype> > S;
	  max_template(const std::vector<Ctype>&, const std::vector<Ctype>&);
	  int n;
	  Ctype operator()(const Rtype&,const Rtype&) const;	  
	};
      
      template <typename Rtype,typename Ctype>
      max_template<Rtype,Ctype>::max_template(const std::vector<Ctype>& X,
							      const std::vector<Ctype>& Q)
	{
	  n = X.size();
	  S = std::vector<std::vector<Ctype> >(n+1);
	  S[0] = std::vector<Ctype>(Q.size()-1,0);	  
	  // indicate
	  for(int i = 1; i <= n; i++)
	    {
	      S[i] = std::vector<Ctype>(Q.size()-1);
	      std::transform(std::begin(Q)+1,std::end(Q)-1,std::begin(S[i]),
			     [&X,&i](const auto& a)
			     {
			       if(X[i-1] < a) return 1.0;
			       if(X[i-1] == a) return 0.5;
			       return 0.0;
			     }
			     );
	    }
	  // accumalate 
	  for(int i = 0; i < n; i++)
	    {
	      for(int j = 0; j < Q.size()-2; j++)
		{
		  S[i+1][j] += S[i][j]; 
		}
	    }	  
	}

      
      template <typename Rtype,typename Ctype>
	Ctype max_template<Rtype,Ctype>::operator()(const Rtype& i,const Rtype& j) const
	{	
	  if(j <= i)
	  {
             return 0.0;
	  }
	  Ctype t = (Ctype)(j - i + 1);
	  std::vector<Ctype> M(S[0].size());
	  std::transform(std::begin(S[j]),std::end(S[j]),std::begin(S[i-1]),std::begin(M),
			 [&t](const auto& a, const auto& b)
		         {
			   auto m = a - b;
			   double val;
			   if(m == 0 || m == t)
			     {
			       val =  0.0;
			     }
			   else
			     {
			       val = -m*std::log(m/t) - (t-m)*std::log(1-m/t);
			     }
			   return val; 
			 }
			 );
	  auto val = 2*(*std::max_element(M.begin(),M.end()));
	  return val;
	  
	}      
      typedef max_template<int,double> max;
    } // namespace np
  } // namespace cost
} // namespace changed



#endif
