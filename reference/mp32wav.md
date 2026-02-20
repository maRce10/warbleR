# Convert .mp3 files to .wav

`mp32wav` converts several .mp3 files in working directory to .wav
format

## Usage

``` r
mp32wav(
  samp.rate = NULL,
  parallel = 1,
  path = NULL,
  dest.path = NULL,
  bit.depth = 16,
  pb = TRUE,
  overwrite = FALSE
)
```

## Arguments

- samp.rate:

  Sampling rate in kHz at which the .wav files should be written. If not
  provided the sample rate of the original .mp3 file is used. THIS
  FEATURE IS CURRENTLY NOT AVAILABLE. However, downsampling can be done
  after .mp3's have been converted using the
  [`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md)
  function (which uses [SOX](https://sourceforge.net/projects/sox/)
  instead). Default is `NULL` (e.g. keep original sampling rate).

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the .mp3 files
  are located. If `NULL` (default) then the current working directory is
  used.

- dest.path:

  Character string containing the directory path where the .wav files
  will be saved. If `NULL` (default) then the folder containing the
  sound files will be used.

- bit.depth:

  Character string containing the units to be used for amplitude
  normalization. Check
  [`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html) for
  details. Default is 16.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- overwrite:

  Logical. Control whether a .wav sound file that is already in the
  working directory should be overwritten.

## Value

.wav files saved in the working directory with same name as original mp3
files.

## Details

The function will convert all mp3 files in working directory or 'path'
supplied to wav format. [bioacoustics
package](https://cran.r-project.org/package=bioacoustics) must be
installed when changing sampling rates (i.e. if 'samp.rate' is
supplied). Note that sound files are normalized using
[`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html) so they can
be written by
[`writeWave`](https://rdrr.io/pkg/tuneR/man/writeWave.html).

convert all .mp3 files in working directory to .wav format. Function
used internally to read .mp3 files
([`readMP3`](https://rdrr.io/pkg/tuneR/man/readMP3.html)) sometimes
crashes.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
if (FALSE) { # \dontrun{
# download mp3 files from xeno-canto
query_xc(qword = "Phaethornis aethopygus", download = TRUE, path = tempdir())

# Convert all files to .wav format
mp32wav(path = tempdir(), dest.path = tempdir())

# check this folder!!
tempdir()
} # }
```
