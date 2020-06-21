# internal warbleR function, not to be called by users. it calculates descriptors of wavelet packet decompositions 
# based on A. Selin, J. Turunen, and J. T. Tanttu, "Wavelets in recognition of bird sounds" EURASIP Journal on Advances in Signal Pro- cessing, vol. 2007, Article ID 51806, 9 pages, 2007.
# @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on oct-7-2019 (MAS)

wpd_feature_wrblr_int <- function(wave, normalize = FALSE, thr1 = 6, thr2 = 0.5){

  #force wave object to have a length exp2
  # get target duration
  exp2 <- `^`(2, 1:1000)
  
  # target duration
  trg <- exp2[which.min(!exp2 > length(wave@left))]
  
  # margin to add at both sides
  mar <- rep(0, (trg - length(wave@left)) / 2)
  
  # amplitude vector
  amp.vctr <- c(mar, wave@left, mar)
  
  # fix if still not exp2
  if(length(amp.vctr) < trg)
    amp.vctr <- c(amp.vctr, trg - length(amp.vctr))
  
  # compute wavelet packet decomposition
  out <- wavethresh::wp(amp.vctr, filter.number = 4)
  
  # coefficients
  mat <- out$wp
  bins <- dim(mat)[2]
  num_wpd_coefs <- dim(mat)[1]

  # sum of mean of Eb for each bin   
  arr_meanEb <- sapply(1:bins, function(e){
    
    sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2)) / num_wpd_coefs
  })
  
  # find max E and position of max E
  mx <- max(arr_meanEb) # max
  ps <- which.max(arr_meanEb) # position
  
  # spread
  # s <- Spread(mat, bins, num_wpd_coefs)
  sprds <- sapply(1:bins, function(w){
    
    # caculate threshold as in equation 6
    umbral <- (sum(sapply(1:num_wpd_coefs, function(i) mat[i, w]^2)) /  num_wpd_coefs) / thr1
    
    value <- mat[, w]^2
    j <- rep(1, length(value))
    
    # make lower values 0
    j[value <= umbral] <- 0
    
    # convert to NA the one lower than value
    value[value <= umbral] <- NA
    
    return(c(sum(value, na.rm = TRUE) / sum(j)))
  })
  
  s <- colSums(t(sprds))
  
  # measure spread
  sprd <- s[1] / s[2]
  
  # add up square coefs by bin
  sum.coefs <- sapply(1:bins, function(e){
    sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2))
  }) 

  # force to a range 0-1 (proportional to maximum)
  sum.coefs <- sum.coefs / max(sum.coefs)
  
  # get the ones above threshold
  w <- sum(sum.coefs > thr2)
  
  # w <- sum(sapply(1:bins, function(e){
  #   if(sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2)) > thr) return(1) else return(0)
  # })) 
  
  result <- c(mx, ps, sprd, w)
  
  if (normalize) {
    
      count <- sum(mat[, result[2]] > (sum(sapply(1:num_wpd_coefs, function(i) mat[i, w]^2)) /  num_wpd_coefs) / 6)
      
      if(count > 0)
        result[1] <- result[1] / count

      result <- result * c(1, 1/16, 1/100, 1/20) 
  }
  
  names(result) <- c("max.energy", "position", "spread", "width")
  
  return(result)
}
