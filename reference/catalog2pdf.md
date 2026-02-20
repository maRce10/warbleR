# Combine [`catalog`](https://marce10.github.io/warbleR/reference/catalog.md) images into pdfs

`catalog2pdf` combines
[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md) jpeg
images into pdfs

## Usage

``` r
catalog2pdf(
  keep.img = TRUE,
  overwrite = FALSE,
  parallel = 1,
  path = NULL,
  pb = TRUE,
  by.img.suffix = FALSE,
  ...
)
```

## Arguments

- keep.img:

  Logical argument. Indicates whether jpeg files should be kept
  (default) or remove.

- overwrite:

  Logical argument. If `TRUE` all jpeg pdf will be produced again when
  code is rerun. If `FALSE` only the ones missing will be produced.
  Default is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the catalog image
  files are located. If `NULL` (default) then the current working
  directory is used.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- by.img.suffix:

  Logical. If `TRUE` catalogs with the same image suffix will be put
  together in a single pdf (so one pdf per image suffix in the catalog
  images). Default is `FALSE` (i.e. no suffix).

- ...:

  Additional arguments to be passed to the internal pdf creating
  function [`pdf`](https://rdrr.io/r/grDevices/pdf.html) for customizing
  output.

## Value

Image files in pdf format with spectrogram catalogs in the working
directory.

## Details

The function combines catalog images in .jpeg format from the
[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md)
function into pdfs. Images must be saved in .jpeg format. Note that
using lower resolution and smaller dimension (width and height) when
creating catalogs will substantially decrease the size of pdf files
(which could be pretty big).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`full_spectrogram2pdf`](https://marce10.github.io/warbleR/reference/full_spectrogram2pdf.md),
[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# save sound file examples
data(list = c("Phae.long1", "Phae.long2"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

catalog(X = lbh_selec_table, nrow = 2, ncol = 4)

# now create single pdf removing jpeg
catalog2pdf(keep.img = FALSE, path = tempdir())

# check this floder
tempdir()
} # }
```
