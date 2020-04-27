
#include <Rcpp.h>
using namespace Rcpp;
namespace impl {
  template <int RTYPE>
  Vector<RTYPE> merge(const Vector<RTYPE>& vec1, const Vector<RTYPE>& vec2)
  {
    if(TYPEOF(vec1) != TYPEOF(vec2)){
      Rcpp::Rcout<<"\n";
      Rcpp::Rcout<<"vector 1 type: "<<TYPEOF(vec1);
      Rcpp::Rcout<<"\n";
      Rcpp::Rcout<<"vector 2 type: "<<TYPEOF(vec2);
      Rcpp::Rcout<<"\n"<<std::endl;
    
      Rcpp::stop("Vectors are not of the same type"); 
    }
    else{
      
      Vector<RTYPE>  vec(vec1.size()+vec2.size());
      std::copy(vec1.begin(), vec1.end(), vec.begin());
      std::copy(vec2.begin(), vec2.end(), vec.begin()+vec1.size());
      
      return vec;
    }
  }
  
  template <int RTYPE>
  IntegerVector as_factor(const Vector<RTYPE>& vec1 ) {
    Vector<RTYPE> levs = sort_unique(vec1);
    IntegerVector out = match(vec1, levs);
    out.attr("levels") = as<CharacterVector>(levs);
    out.attr("class") = "factor";
    return out;
  }

}

// [[Rcpp::export]]
SEXP vec_merge(SEXP vec1, SEXP vec2) {
  switch (TYPEOF(vec1)) {
  case NILSXP: {Rcpp::stop("Argument vec is NULL vector, cannot merge vector");}
  case BUILTINSXP: {Rcpp::stop("Argument vec is builtin non-special forms vector, cannot merge vector");}
  case RAWSXP: {Rcpp::stop("Argument vec is raw bytes vector, cannot merge vector");}
  case INTSXP: {
    if(Rf_isFactor(vec1)){
      IntegerVector v1=vec1;
      IntegerVector v2=vec2;
      bool test1=Rf_isNull(v1.attr("class"));
      bool test2=Rf_isNull(v2.attr("class"));
      
      if(test1==FALSE && test2==FALSE){
        std::string r1=v1.attr("class");
        std::string r2=v1.attr("class");

        if(r1=="factor"){
          CharacterVector v1=vec1;
          CharacterVector v2=vec2;
          CharacterVector vec3(v1.size()+v2.size());
          std::copy(v1.begin(), v1.end(), vec3.begin());
          std::copy(v2.begin(), v2.end(), vec3.begin()+v1.size());
          CharacterVector levs = sort_unique(vec3);
          levs=na_omit(levs);
          IntegerVector vec = match(vec3, levs);
          vec.attr("levels") = levs;
          vec.attr("class") = "factor";
          return vec;
        }
        else{
          SEXP res =impl::merge(as<IntegerVector>(vec1), as<IntegerVector>(vec2));
          res=impl::as_factor(as<IntegerVector>(res));
          return res;
        }
      }
    }
    else{
      return impl::merge(as<IntegerVector>(vec1), as<IntegerVector>(vec2));
    }
  }
  case REALSXP: {
    return impl::merge(as<NumericVector>(vec1), as<NumericVector>(vec2));
  }
  case STRSXP: {
    return impl::merge(as<CharacterVector>(vec1), as<CharacterVector>(vec2));
  }
  case LGLSXP: {
    return impl::merge(as<LogicalVector>(vec1), as<LogicalVector>(vec2));
  }
  case CPLXSXP: {
    return impl::merge(as<ComplexVector>(vec1), as<ComplexVector>(vec2));
  }
  default: {
    warning(
      "Invalid SEXPTYPE %d (%s).\n",
      TYPEOF(vec1), type2name(vec1)
    );
    return R_NilValue;
  }
  }
}
