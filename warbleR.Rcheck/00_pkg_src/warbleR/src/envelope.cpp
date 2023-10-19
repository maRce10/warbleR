#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//' @title Calculates the absolute amplitude envelope
//' @usage envelope(x, ssmooth = 0)
//' @param x Numeric vector with amplitude values. Required.
//' @param ssmooth Numeric vector of length 1 indicating the size of the sliding window use to smooth envelopes. Default is 0 (no smoothing).
//' @return An amplitude envelope.
//' @export
//' @name envelope
//' @details The function calculates the absolute amplitude envelope of an amplitude vector using compiled C code which is usually several times faster.
//' @seealso \code{\link[seewave]{env}}.
//' @rawNamespace useDynLib(warbleR)  
//' @examples{
//' data(tico)
//' 
//' amp_env <- envelope(tico@left, ssmooth = 100)
//' }
//' @references {
//' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
//' }
//' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) & Paula Monge
// last modification on nov-17-2020 (MAS)


double media (NumericVector amp_v,int indice, int length){ 
  double suma = 0.0, med = 0.0; 
  double cont = 0.0;
  
  for (int index = indice; index <= length; index++){
    suma += amp_v[index]; 
    cont++;
  }
  
  med = suma / cont; 
  return med; 
}

// [[Rcpp::export]]
NumericVector envelope(NumericVector x, int ssmooth = 0){
    //extract vector size
    int tam = x.size();
    NumericVector abs_amp_v(tam);
    
    // get absolute amplitude values
    for (int index = 0; index < tam; index++){
      abs_amp_v[index] = abs(x[index]);
    }
    
    double half_ssmooth = ceil(ssmooth / 2.0);
    
    //smoothing
    if (ssmooth != 0){
      
      //create null vector
      NumericVector  smooth_abs_amp_v(tam);
    
      for (int index = 0; index < tam; index++){
        smooth_abs_amp_v[index] = 0;
      }
      
      // start of smoothing neighborhood
      int strt = 0;
      
      for(int index = 0; index < tam; index++){
        
        if(index - half_ssmooth > 0){
            strt = index - half_ssmooth;
        }
        
        // end of smoothing neighborhood
        int end = tam-1;
        
        if (index + half_ssmooth < tam){
          end = index + half_ssmooth;
        }

        smooth_abs_amp_v[index] = media(abs_amp_v, strt, end);
      }
      
      return smooth_abs_amp_v;
    }
    else{
     return abs_amp_v;
    }
}
