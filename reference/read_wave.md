# A wrapper for tuneR's readWave that read sound files listed within selection tables

`read_wave` is a wrapper for tuneR's
[`readWave`](https://rdrr.io/pkg/tuneR/man/readWave.html) function that
read sound files listed in data frames and selection tables

## Usage

``` r
read_wave(...)
```

## Arguments

- ...:

  arguments to be passed internally to
  [`read_sound_file`](https://marce10.github.io/warbleR/reference/read_sound_file.md).

## Value

An object of class "Wave".

## Details

The function is a wrapper for
[`read_sound_file`](https://marce10.github.io/warbleR/reference/read_sound_file.md).
The function is slated for deprecation and will be removed in future
versions of the package. Please use
[`read_sound_file`](https://marce10.github.io/warbleR/reference/read_sound_file.md)
instead.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # write wave files with lower case file extension
  data(list = c("Phae.long1"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

  # read from selection table
  read_wave(X = lbh_selec_table, index = 1, path = tempdir())

  # from extended selection table
  library(NatureSounds)
  read_wave(X = lbh.est, index = 1)

  # read WAV
  filepath <- system.file("extdata", "recording.wav", package = "bioacoustics")
  read_wave(filepath)

  # read MP3
  filepath <- system.file("extdata", "recording.mp3", package = "bioacoustics")
  read_wave(filepath)

  # URL file
  read_wave(X = "https://www.xeno-canto.org/513948/download")
}
#> This function is slated for deprecation in future versions. Please use the function read_sound_file() instead.
#> This function is slated for deprecation in future versions. Please use the function read_sound_file() instead.
#> This function is slated for deprecation in future versions. Please use the function read_sound_file() instead.
#> This function is slated for deprecation in future versions. Please use the function read_sound_file() instead.
#> This function is slated for deprecation in future versions. Please use the function read_sound_file() instead.
#> 
#> Wave Object
#>  Number of Samples:      274176
#>  Duration (seconds):     6.22
#>  Samplingrate (Hertz):   44100
#>  Channels (Mono/Stereo): Mono
#>  PCM (integer format):   TRUE
#>  Bit (8/16/24/32/64):    16 
#> 
```
