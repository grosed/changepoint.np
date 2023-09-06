#ifndef ___NORMAL_SUMSTATS_H___
#define ___NORMAL_SUMSTATS_H___

#include <vector>
#include <algorithm>
#include <numeric>

namespace changed
{
  namespace cost
  {
    namespace normal
    {
      template <typename Ctype>
	struct sumstats
	{
	  std::vector<Ctype> SX;
	  std::vector<Ctype> SXX;
	  sumstats(const std::vector<Ctype>&);
	  std::vector<Ctype> getSX() const;
	  std::vector<Ctype> getSXX() const;
	};

      template <typename Ctype>
      sumstats<Ctype>::sumstats(const std::vector<Ctype>& X)
	{
	  SX = std::vector<Ctype>(X.size()+1,0.0);
	  SXX = std::vector<Ctype>(X.size()+1,0.0);
	  std::transform(X.begin(),X.end(),SX.begin()+1,[](const auto& x){return x;});
	  std::partial_sum(SX.begin(), SX.end(),SX.begin());
	  std::transform(X.begin(),X.end(),SXX.begin()+1,[](const auto& x){return x*x;});
	  std::partial_sum(SXX.begin(), SXX.end(),SXX.begin());
	}
      
      template <typename Ctype>
	std::vector<Ctype> sumstats<Ctype>::getSX() const
	{
	  return SX;
	}
      
      template <typename Ctype>
	std::vector<Ctype> sumstats<Ctype>::getSXX() const
	{
	  return SXX;
	}
    } // namespace normal
  } // namespace cost
} // namespace changed


#endif


