if(getRversion() < "3.3.0")
{
  stop("Your version of R is too old. This package requires R-3.3.0 or newer on Windows.")
}

if (Sys.getenv("LIB_FFTW") == "")
{
  if (!file.exists("../windows/include/fftw3.h") ||
      !file.exists(paste0("../windows/lib/", .Platform$r_arch, "/fftw3.a")))
  {
    download.file("https://github.com/wavx/rpkg-libs/raw/master/fftw3/fftw3.zip", "lib.zip", quiet = TRUE)

    dir.create("../windows", showWarnings = FALSE)
    unzip("lib.zip", exdir = "../windows", files = c("include/fftw3.h",  paste0("lib/", .Platform$r_arch, "/libfftw3.a")))
    unlink("lib.zip")
  }
}

if (!file.exists("../windows/include/soxr-lsr.h") ||
    !file.exists(paste0("../windows/lib/", .Platform$r_arch, "/libsoxr-lsr.a")))
{
  download.file("https://github.com/wavx/rpkg-libs/raw/master/libsoxr-lsr/libsoxr-lsr.zip", "lib.zip", quiet = TRUE)

  dir.create("../windows", showWarnings = FALSE)
  unzip(
    "lib.zip",
    exdir = "../windows",
    files = c("include/soxr.h", "include/soxr-lsr.h",
              paste0("lib/", .Platform$r_arch, c("/libsoxr.a", "/libsoxr-lsr.a"))
    )
  )
  unlink("lib.zip")
}

