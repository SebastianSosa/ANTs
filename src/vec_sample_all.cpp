#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

template <int RTYPE>
Vector<RTYPE> sample(const Vector<RTYPE>& vec)
{
  Vector<RTYPE> res=Rcpp::sample(vec,vec.size(), FALSE);

  return res;
}

template <int RTYPE>
IntegerVector as_factor(const Vector<RTYPE>& x ) {
  Vector<RTYPE> levs = sort_unique(x);
  IntegerVector out = match(x, levs);
  out.attr("levels") = as<CharacterVector>(levs);
  out.attr("class") = "factor";
  return out;
}

} 

// [[Rcpp::export]]
SEXP vec_sample_all(SEXP vec) {
  switch (TYPEOF(vec)) {
  case NILSXP: {Rcpp::stop("Argument vec is NULL vector, cannot sample vector");}
  case BUILTINSXP: {Rcpp::stop("Argument vec is builtin non-special forms vector, cannot sample vector");}
  case RAWSXP: {Rcpp::stop("Argument vec is raw bytes vector, cannot sample vector");}
  case INTSXP: {
    if(Rf_isFactor(vec)){
      IntegerVector v1=vec;
      bool test1=Rf_isNull(v1.attr("class"));
      if(test1==FALSE){
        std::string r1=v1.attr("class");
        if(r1=="factor"){
          SEXP res =impl::sample(as<CharacterVector>(vec));
          res=impl::as_factor(as<CharacterVector>(res));
          return res;
        }
        else{
          SEXP res =impl::sample(as<IntegerVector>(vec));
          res=impl::as_factor(as<IntegerVector>(res));
          return res;
        }
      }
    }
    else{
      return impl::sample(as<IntegerVector>(vec));
    }
    
  }
  case REALSXP: {
    if(Rf_isFactor(vec)){
      SEXP res =impl::sample(as<NumericVector>(vec));
      res=impl::as_factor(as<NumericVector>(res));
      return res;
    }
    else{
      return impl::sample(as<NumericVector>(vec));
    }
  }
  case STRSXP: {
    if(Rf_isFactor(vec)){
      SEXP res =impl::sample(as<CharacterVector>(vec));
      res=impl::as_factor(as<CharacterVector>(res));
      return res;
    }
    else{
      return impl::sample(as<CharacterVector>(vec));
    }
  }
  case LGLSXP: {
    return impl::sample(as<LogicalVector>(vec));
  }
  case CPLXSXP: {
    return impl::sample(as<ComplexVector>(vec));
  }
  default: {
    //Rcpp::stop(
      //"Invalid SEXPTYPE %d (%s).\n",
      //TYPEOF(vec), type2name(vec)
    //);
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(vec)
    );
    return R_NilValue;
  }
  }
}