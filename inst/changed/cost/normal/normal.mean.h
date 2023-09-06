#ifndef ___NORMAL_MEAN_H___
#define ___NORMAL_MEAN_H___


#include <vector>

#include "sumstats.h"

namespace changed
{
  namespace cost
  { 
    namespace normal
    {      
      template <typename Rtype,typename Ctype>
	struct mean_template
	{
	  sumstats<Ctype> S;  
	  mean_template(const std::vector<Ctype>&);
	  Ctype operator()(const Rtype&,const Rtype&) const;
	};
      
      
      template <typename Rtype,typename Ctype>
	mean_template<Rtype,Ctype>::mean_template(const std::vector<Ctype>& X) : S(X) {}
      
      template <typename Rtype,typename Ctype>
	Ctype mean_template<Rtype,Ctype>::operator()(const Rtype& i,const Rtype& j) const
	{
	  Ctype val = S.SX[j] - S.SX[i-1];
	  val *= val;
	  val /= (j - i + 1);
	  val = -val;
	  val += S.SXX[j] - S.SXX[i-1];
	  return val;
	}
      typedef mean_template<int,double> mean;
    } // namespace normal
  } // namespace cost
} // namespace changed



#endif
