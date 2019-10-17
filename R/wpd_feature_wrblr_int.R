# internal warbleR function, not to be called by users. it calculates descriptors of wavelet packet decompositions 
# based on A. Selin, J. Turunen, and J. T. Tanttu, "Wavelets in recognition of bird sounds" EURASIP Journal on Advances in Signal Pro- cessing, vol. 2007, Article ID 51806, 9 pages, 2007.
# @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
# last modification on oct-7-2019 (MAS)
# library(wavethresh)

wpd_feature_wrblr_int <- function(wave, normalize = FALSE, thr = 1.3){

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
  out <- wavethresh::wp(amp.vctr, filter.number = 10)
  
  # coeficientes
  mat <- out$wp
  bins <- dim(mat)[2]
  num_wpd_coefs <- dim(mat)[1]
  
  arr_meanEb <- sapply(1:bins, function(e){
    
    sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2)) / bins
    
  })
  
  emAndP <- c(arr_meanEb[which.max(arr_meanEb)], which.max(arr_meanEb))
  
  # maximum
  mx <- emAndP[1]
  #position
  ps <- emAndP[2]
  
  # spread
  # s <- Spread(mat, bins, num_wpd_coefs)
  sprds <- sapply(1:bins, function(w){
    
    # umbral <- calculateThreshold(mat, w, num_wpd_coefs)
    umbral <- (sum(sapply(1:num_wpd_coefs, function(i) mat[i, w]^2)) /  num_wpd_coefs) / 6
    
    value <- mat[, w]^2
    j <- rep(1, length(value))
    j[value <= umbral] <- 0
    value[value <= umbral] <- 0
    
    return(c(sum(value), sum(j)))
  })
  
  s <- colSums(t(sprds))
  
  sprd <- s[1] / s[2]
  # w <- W(mat, bins, num_wpd_coefs)

  w <- sum(sapply(1:bins, function(e){
    if(sum(sapply(1:num_wpd_coefs, function(i) mat[i, e]^2)) > thr) return(1) else return(0)
  })) 
  
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


# data(tico)
# 
# wpd_features(tico, normalize = T)
# 
# wave <- cutw(tico, from = 0, to = 1, output = "Wave")
# wave <- cutw(tico, from = 1, to = 1.1, output = "Wave")
# 
# wpd_features(wave, normalize = T)
# 
# 
# # vector de prueba
# wave <- rnorm(1:512)
# wave <- tico@left
# out <- wp(wave, filter.number = 10)
# 
# # coeficientes
# matriz<-out$wp
# # matriz[8,8]= 35.0
# bins<-dim(matriz)[2]
# num_coefs<-dim(matriz)[1]
# 
# wpd_feature_wrblr_int(wave, normalize = T)
# 
# all.equal(wpd_features(wave),ORGwpd_features(matriz, bins, num_coefs))
# 
# all.equal(wpd_features(wave, normalize = T),
# ORGwpd_features_normalized(matriz, bins, num_wpd_coefs))
# 
