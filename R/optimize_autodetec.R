optimize_autodetec <- function(X, Y, threshold = seq(1, 25, 1), power = 1, wl = 512, ssmooth = 200, hold.time = 0, mindur = NULL, maxdur = NULL, parallel = 1, by.sound.file = FALSE, bp = NULL){
 
        
      exp_grd <- expand.grid(threshold = threshold, wl = wl, power = power, ssmooth = ssmooth, hold.time = hold.time, mindur = if(is.null(mindur)) -Inf else mindur, maxdur = if(is.null(maxdur)) Inf else maxdur)
      
        
      grid_FUN <- function(exp_grd, X, Y){

        grid_results <- lapply(1:nrow(exp_grd), function(x){
          
          ad <- autodetec(X = Y, wl = exp_grd$wl[x], threshold = exp_grd$threshold[x], ssmooth = exp_grd$ssmooth[x], mindur = exp_grd$mindur[x], maxdur = exp_grd$maxdur[x], parallel = parallel, pb = FALSE, power = exp_grd$power[x], hold.time = exp_grd$hold.time[x], bp = bp)
          
          ad$..row.id <- 1:nrow(ad)    
          
          ad <- ad[!is.na(ad$start), ]
          
          if (nrow(ad) > 0){
            performance_l <- lapply(seq_len(nrow(X)), function(y){
              
            detections <- ad[ad$sound.files == X$sound.files[y]  & (ad$start >= X$start[y] & ad$start < X$end[y]) | (ad$end > X$start[y] & ad$end <= X$end[y]) | (ad$start <= X$start[y] & ad$end >= X$end[y]) | (ad$start >= X$start[y] & ad$end  <= X$end[y]),]
              
            result <- if (nrow(detections > 0))
              data.frame(
                count = nrow(detections),
                prop.time = sum(detections$end - detections$start) / (X$end[y] - X$start[y]),
                mean.duration.true.positives = mean(detections$end - detections$start),
                mean.duration.false.positives = mean((ad$end - ad$start)[(!ad$..row.id %in%  detections$..row.id) &
                                                                           ad$sound.files == X$sound.files[y]])
              )
            else
              data.frame(
                count = 0,
                prop.time = 0,
                mean.duration.true.positives = 0,
                mean.duration.false.positives = mean((ad$end - ad$start)[ad$sound.files == X$sound.files[y]])
              )
            
            return(result)
            })
                    
         performance <- do.call(rbind, performance_l)
        
         out <-
           data.frame(
             threshold = exp_grd$threshold[x],
             wl = exp_grd$wl[x],
             power = exp_grd$power[x],
             ssmooth = exp_grd$ssmooth[x],
             hold.time = exp_grd$hold.time[x],
             mindur = exp_grd$mindur[x],
             maxdur = exp_grd$maxdur[x],
             true.positives = sum(performance$count > 0),
             false.positives = nrow(ad) - sum(performance$count),
             split.positives = sum(performance$count > 1) ,
             mean.duration.true.positives = mean(performance$mean.duration.true.positives),
             mean.duration.false.positives = mean(performance$mean.duration.false.positives),
             proportional.time.true.positives = mean(performance$prop.time),
             sensitivity = sum(performance$count > 0) / nrow(X)
             )
           
        
        out$mean.duration.false.positives[is.na(out$mean.duration.false.positives)] <- 0
        out$false.positives[out$false.positives < 0] <- 0      
        out$specificity <- out$true.positives / (out$true.positives + out$false.positives)
          } else
            
            out <-
            data.frame(
              threshold = exp_grd$threshold[x],
              wl = exp_grd$wl[x],
              power = exp_grd$power[x],
              ssmooth = exp_grd$ssmooth[x],
              hold.time = exp_grd$hold.time[x],
              mindur = exp_grd$mindur[x],
              maxdur = exp_grd$maxdur[x],
              true.positives = NA,
              false.positives = NA,
              split.positives = NA,
              mean.duration.true.positives = NA,
              mean.duration.false.positives = NA,
              proportional.time.true.positives = NA,
              sensitivity = NA,
              specificity = NA
            )
          
          
         return(out)
          })
      
        results <- do.call(rbind, grid_results)
        
        return(results)
        }
        
      
    if (by.sound.file) {
      by.rec <- lapply(unique(X$sound.files), function(w) {
        W <- grid_FUN(exp_grd, X[X$sound.files == w, ], Y)
        W$sound.files <- w
        return(W)
        })
      
      output <- do.call(rbind, by.rec)
      } else
        output <- grid_FUN(exp_grd, X, Y)
      
      
      return(output)
}
