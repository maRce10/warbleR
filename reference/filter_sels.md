# Subset selection data frames based on manually filtered image files

`filter_sels` subsets selection data frames based on image files that
have been manually filtered.

## Usage

``` r
filter_sels(
  X,
  path = NULL,
  lspec = FALSE,
  img.suffix = NULL,
  it = "jpeg",
  incl.wav = TRUE,
  missing = FALSE,
  index = FALSE
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or data
  frame with the following columns: 1) "sound.files": name of the .wav
  files, 2) "selec": number of the selections.

- path:

  Character string containing the directory path where the image files
  are located. If `NULL` (default) then the current working directory is
  used.
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)
  'wav.path' argument does not apply.

- lspec:

  A logical argument indicating if the image files to be use for
  filtering were produced by the function
  [`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md).
  All the image files that correspond to a sound file must be deleted in
  order to be filtered out.

- img.suffix:

  A character vector of length 1 with the suffix (label) at the end of
  the names of the image files. Default is `NULL` (i.e. no suffix as in
  the images produced by
  [`spectrograms`](https://marce10.github.io/warbleR/reference/spectrograms.md)).
  Ignored if `lspec = TRUE`.

- it:

  A character vector of length 1 giving the image type ("tiff", "jpeg"
  or "pdf") Default is "jpeg". Note that pdf files can only be generated
  by
  [`full_spectrogram2pdf`](https://marce10.github.io/warbleR/reference/full_spectrogram2pdf.md).

- incl.wav:

  Logical. To indicate if sound files extensions are included ( `TRUE`,
  default) or not in the image file names.

- missing:

  Logical. Controls whether the output data frame (or row index if is
  `index = TRUE`) contains the selections with images in the working
  directory (Default, `missing = FALSE`) or the ones with no image.

- index:

  Logical. If `TRUE` and `missing = FALSE` the row index for the
  selections with images in the working directory is returned. If
  `missing = TRUE`) then the row index of the ones with no image is
  returned instead. Default is `FALSE`.

## Value

If all sound files are ok, returns message "All files are ok!".
Otherwise returns "These file(s) cannot be read" message with names of
the corrupted sound files.

## Details

This function subsets selections (or sound files if `lspec` is `TRUE`)
listed in a data frame based on the image files from
spectrogram-creating functions (e.g.
[`spectrograms`](https://marce10.github.io/warbleR/reference/spectrograms.md))
in the working directory. Only the selections/sound files with and image
in the working directory will remain. This is useful for excluding
selections from undesired signals. Note that the image files should be
in the working directory (or the directory provided in 'path').

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

spectrograms(lbh_selec_table,
  flim = c(0, 11), inner.mar = c(4, 4.5, 2, 1), outer.mar = c(4, 2, 2, 1),
  picsize = 2, res = 300, cexlab = 2, mar = 0.05, wl = 300, path = tempdir()
)

# go to the working directory (tempdir()) and delete some images

# filter selection data frame
fmloc <- filter_sels(X = lbh_selec_table, path = tempdir())

# this data frame does not have the selections corresponding to the images that were deleted
fmloc

# now using lspec images
full_spectrograms(
  sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300, ovlp = 10,
  path = tempdir()
)

# go to the working directory (tempdir()) and delete lspec
# images (the ones with several rows of spectrograms)

# filter selection data frame
fmloc2 <- filter_sels(
  X = lbh_selec_table, lspec = TRUE,
  path = tempdir()
)
} # }
```
