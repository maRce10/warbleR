test_that("basic catalog", {
  
  data("lbh.est")

  # order by sound file name
  lbh.est <- lbh.est[order(lbh.est$sound.files),]

  # create new sound file name
  nsf <- sapply(strsplit(lbh.est$sound.files, ".wav",fixed = TRUE), "[",1)

  slc <- vector(length = nrow(lbh.est))
  slc[1] <- 1

  for(i in 2:length(slc))
  if (nsf[i - 1] == nsf[i]) slc[i] <- slc[i - 1] + 1 else
  slc[i] <- 1

  nsf <- paste(nsf, slc, sep = "_")

  # rename sound files
  Y <- rename_est_waves(X = lbh.est, new.sound.files = nsf)
  
  expect_equal(Y$sound.files[1], "BR2-A1-1_1")
})
