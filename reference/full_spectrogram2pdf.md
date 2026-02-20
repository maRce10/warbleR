# `full_spectrogram2pdf` combines [`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md) images in .jpeg format to a single pdf file.

`full_spectrogram2pdf` combines
[`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md)
images in .jpeg format to a single pdf file.

## Usage

``` r
full_spectrogram2pdf(
  keep.img = TRUE,
  overwrite = FALSE,
  parallel = 1,
  path = NULL,
  pb = TRUE
)
```

## Arguments

- keep.img:

  Logical argument. Indicates whether jpeg files should be kept
  (default) or remove. (including sound file and page number) should be
  magnified. Default is 1.

- overwrite:

  Logical argument. If `TRUE` all jpeg pdf will be produced again when
  code is rerun. If `FALSE` only the ones missing will be produced.
  Default is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

## Value

Image files in pdf format with spectrograms of entire sound files in the
working directory.

## Details

The function combines spectrograms for complete sound files from the
[`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md)
function into a single pdf (for each sound file).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md),
[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# save sound file examples
data(list = c("Phae.long1", "Phae.long2"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

full_spectrograms(
  sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
  it = "jpeg", path = tempdir()
)

# now create single pdf removing jpeg
full_spectrogram2pdf(keep.img = FALSE, path = tempdir())

# check this floder
tempdir()
} # }
```
