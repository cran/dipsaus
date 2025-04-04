#include <cstdlib>
#include <Rcpp.h>
#include "utils.h"

// [[Rcpp::export]]
SEXP check_missing_dots(const SEXP env){
  if( TYPEOF(env) != ENVSXP ){
    Rcpp::stop("`check_missing_dots` is asking for an environment");
  }
  SEXP dots = Rf_findVarInFrame(env, R_DotsSymbol);

  std::vector<bool> is_missing(0);

  int count = 0;

  if( dots != R_NilValue ){
    SEXP el = R_NilValue;

    for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots) ){
      el = CAR(dots);
      if( el == R_MissingArg ){
        is_missing.push_back(true);
      } else {
        is_missing.push_back(false);
      }
      count++;
      if(count > 1000){
        Rcpp::stop("Cannot iterate through all dots. Are you calling `missing_dots` with correct environment?");
      }
    }
  }

  return(Rcpp::wrap(is_missing));
}

// [[Rcpp::export]]
std::string object_address(SEXP x) {
  std::ostringstream addr;
  addr << x;
  return addr.str();
}

// [[Rcpp::export]]
SEXP sumsquared(SEXP &x){
  // only support vector, matrix (numerical)
  const SEXPTYPE stype = get_sexp_type(x);

  R_xlen_t xlen = Rf_xlength(x);
  SEXP res = R_NilValue;
  int protects = 0;

  switch(stype) {
  case LGLSXP: {
    R_xlen_t re = 0;
    int* ptr = LOGICAL(x);
    for(R_xlen_t ii = 0; ii < xlen; ii++, ptr++){
      if(*ptr && *ptr != NA_LOGICAL){
        re++;
      }
    }
    res = PROTECT(Rcpp::wrap(re));
    protects++;
    break;
  }
  case INTSXP: {
    R_xlen_t re = 0;
    int* ptr = INTEGER(x);
    for(R_xlen_t ii = 0; ii < xlen; ii++, ptr++){
      if(R_finite(*ptr)){
        re += (*ptr) * (*ptr);
      }
    }
    res = PROTECT(Rcpp::wrap(re));
    protects++;
    break;
  }
  case REALSXP: {
    double re = 0;
    double* ptr = REAL(x);
    for(R_xlen_t ii = 0; ii < xlen; ii++, ptr++){
      if(R_finite(*ptr)){
        re += (*ptr) * (*ptr);
      }
    }
    res = PROTECT(Rcpp::wrap(re));
    protects++;
    break;
  }
  default: {
    Rcpp::stop("Unsupported data type: numerical (integer, logical, double) vector or matrix allowed.");
  }
  }
  if(protects > 0){
    UNPROTECT(protects);
  }
  return res;
}

// [[Rcpp::export]]
SEXPTYPE get_sexp_type(const SEXP &x){
  return TYPEOF(x);
}

// [[Rcpp::export]]
SEXP set_dim(SEXP &x, SEXP &dim){
  Rf_setAttrib(x, R_DimSymbol, dim);
  Rf_setAttrib(x, R_NamesSymbol, R_NilValue);
  return R_NilValue;
}

void get_index(std::vector<int64_t>::iterator ptr, int64_t ii, const TinyParallel::RVector<int>& dims){
  int64_t rem = 0;
  int64_t leap = 1;
  std::size_t jj;

  if(ii == NA_INTEGER){
    for( jj = 0; jj < dims.length(); jj++ ){
      *ptr = NA_INTEGER;
      ptr++;
    }
    return;
  }

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}

void get_index(Rcpp::IntegerVector::iterator ptr, int64_t ii,
               const Rcpp::IntegerVector& dims){
  int64_t rem = 0;
  int64_t leap = 1;
  R_xlen_t jj;

  if(ii == NA_INTEGER){
    for( jj = 0; jj < dims.length(); jj++ ){
      *ptr = NA_INTEGER;
      ptr++;
    }
    return;
  }

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}

void get_index(std::vector<int64_t>::iterator ptr, int64_t ii,
               const Rcpp::IntegerVector& dims){
  int64_t rem = 0;
  int64_t leap = 1;
  R_xlen_t jj;

  if(ii == NA_INTEGER){
    for( jj = 0; jj < dims.length(); jj++ ){
      *ptr = NA_INTEGER;
      ptr++;
    }
    return;
  }

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}




int64_t get_ii(Rcpp::IntegerVector idx, Rcpp::IntegerVector dim){

  int64_t ii = 0;
  int64_t leap = 1;

  for(R_xlen_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}

int64_t get_ii(std::vector<int64_t> idx, Rcpp::IntegerVector dim){

  int64_t ii = 0;
  int64_t leap = 1;

  for(std::size_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}
int64_t get_ii(TinyParallel::RVector<int64_t> idx, Rcpp::IntegerVector dim){

  int64_t ii = 0;
  int64_t leap = 1;

  for(std::size_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}

int64_t get_ii(std::vector<int64_t> idx, TinyParallel::RVector<int> dim){
  int64_t ii = 0;
  int64_t leap = 1;

  for(std::size_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}




int64_t length_from_dim(Rcpp::IntegerVector dim){

  if(!dim.length()){
    return 0;
  }

  int64_t len = Rcpp::algorithm::prod_nona(dim.begin(), dim.end());

  // for(auto& el : dim){
  //   len *= el;
  // }
  return len;
}

void next_array_index(
    Rcpp::IntegerVector::iterator begin,
    Rcpp::IntegerVector::iterator end,
    Rcpp::IntegerVector::iterator dim){

  *begin += 1;
  for(; begin != end; begin++ ){
    if( *begin >= *dim ){
      *begin = 0;
      *(begin+1) += 1;
    }else{
      break;
    }
    dim++;
  }
}





double add_sqrt(const double e1, const double e2){
  return e1 + std::sqrt(e2);
}

double add_log10(const double e1, const double e2){
  return e1 + std::log10(e2);
}

double add_square(const double e1, const double e2){
  return e1 + e2 * e2;
}

// [[Rcpp::export]]
bool is_namespace(SEXP &rho) {
  if (rho == R_BaseNamespace)
    return true;
  else if (TYPEOF(rho) == ENVSXP) {
    SEXP info = Rf_findVarInFrame(rho, Rf_install(".__NAMESPACE__."));
    if (info != R_UnboundValue && TYPEOF(info) == ENVSXP) {
      PROTECT(info);
      SEXP spec = Rf_findVarInFrame(info, Rf_install("spec"));
      UNPROTECT(1);
      if (spec != R_UnboundValue &&
          TYPEOF(spec) == STRSXP && LENGTH(spec) > 0)
        return true;
      else
        return false;
    }
    else return false;
  }
  else return false;
}

// [[Rcpp::export]]
bool is_env_from_package(SEXP &x, const bool& recursive) {

  Rcpp::Environment env;

  switch (TYPEOF(x))
  {
    case NILSXP:
    case PROMSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case EXTPTRSXP:
    // 25	S4SXP	S4 classes not of simple type
    // case S4SXP:
      return true;
    case ENVSXP:
      env = Rcpp::Environment(x);
      break;
    case CLOSXP: {
      Rcpp::Function f(x);
      // env = CLOENV(x);
      env = f.environment();
      break;
    }
    default: {
      // env = Rf_getAttrib(x, Rf_install(".Environment"));
      SEXP attr = Rf_getAttrib(x, Rf_install(".Environment"));
      if (TYPEOF(attr) != ENVSXP) return false;
      env = Rcpp::Environment(attr);
    }
  }

  if( TYPEOF(env) - ENVSXP != 0 ) { return false; }

  if( env == Rcpp::Environment::global_env() ) { return false; }
  if( env == Rcpp::Environment::empty_env() ) { return false; }
  if( env == Rcpp::Environment::base_env() ) { return true; }

  SEXP env_impl = Rcpp::wrap(env);
  if( is_namespace(env_impl) ) { return true; }

  // recursively check
  if( recursive ) {
    Rcpp::Environment parent_env = env.parent();
    if (parent_env == Rcpp::Environment::empty_env()) return false;
    SEXP penv = Rcpp::wrap(parent_env);
    return is_env_from_package(penv, recursive);
    // SEXP enclos = ENCLOS(env);
    // return is_env_from_package( enclos, recursive );
  }

  return false;

}

/*** R
(function(...){ check_missing_dots(environment()) })()
b <- local({function(){2}})
get_sexpinfo_obj(b)
PackFlags(3, 0, 0, 0, 1)
b()
get_sexpinfo_obj(b)
# 263171
PackFlags(3, 64, 0, 0, 1)
a <- get_enclosing_env(b)
*/
