# Create long spectrograms of entire sound files

`full_spectrograms` produces image files with spectrograms of entire
sound files split into multiple rows.

## Usage

``` r
full_spectrograms(
  X = NULL,
  flim = NULL,
  sxrow = 5,
  rows = 10,
  collevels = seq(-40, 0, 1),
  ovlp = 50,
  parallel = 1,
  wl = 512,
  gr = FALSE,
  pal = reverse.gray.colors.2,
  cex = 1,
  it = "jpeg",
  flist = NULL,
  overwrite = TRUE,
  path = NULL,
  pb = TRUE,
  fast.spec = FALSE,
  labels = "selec",
  horizontal = FALSE,
  song = NULL,
  suffix = NULL,
  dest.path = NULL,
  only.annotated = FALSE,
  ...
)
```

## Arguments

- X:

  'selection_table' object or any data frame with columns for sound file
  name (sound.files), selection number (selec), and start and end time
  of signal (start and end). If given, a transparent box is plotted
  around each selection and the selections are labeled with the
  selection number (and selection comment, if available). Default is
  `NULL`. If supplied a secondary row is displayed under each
  spectrogram showing the detection (either cross-correlation scores or
  wave envelopes) values across time.

- flim:

  A numeric vector of length 2 indicating the highest and lowest
  frequency limits (kHz) of the spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`. Alternatively, a character vector similar to `c("-1", "1")` in
  which the first number is the value to be added to the minimum bottom
  frequency in 'X' and the second the value to be added to the maximum
  top frequency in 'X'. This is computed independently for each sound
  file so the frequency limit better fits the frequency range of the
  annotated signals. This is useful when plotting annotated spectrograms
  with marked differences in the frequency range of annotations among
  sond files. Note that top frequency adjustment is ignored if 'song'
  labels are included (argument 'song').

- sxrow:

  A numeric vector of length 1. Specifies seconds of spectrogram per
  row. Default is 5.

- rows:

  A numeric vector of length 1. Specifies number of rows per image file.
  Default is 10.

- collevels:

  A numeric vector of length 3. Specifies levels to partition the
  amplitude range of the spectrogram (in dB). The more levels the higher
  the resolution of the spectrogram. Default is seq(-40, 0, 1).

- ovlp:

  Numeric vector of length 1 specifying % of overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is 50. High values of ovlp slow down the function but produce more
  accurate selection limits (when X is provided).

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- gr:

  Logical argument to add grid to spectrogram. Default is `FALSE`.

- pal:

  Color palette function for spectrogram. Default is
  reverse.gray.colors.2. See
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html) for more
  palettes.

- cex:

  A numeric vector of length 1 giving the amount by which text
  (including sound file and page number) should be magnified. Default is
  1.

- it:

  A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted. Default is "jpeg".

- flist:

  character vector or factor indicating the subset of files that will be
  analyzed. Ignored if X is provided.

- overwrite:

  Logical argument. If `TRUE` all selections will be analyzed again when
  code is rerun. If `FALSE` only the selections that do not have a image
  file in the working directory will be analyzed. Default is `FALSE`.

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- fast.spec:

  Logical. If `TRUE` then image function is used internally to create
  spectrograms, which substantially increases performance (much faster),
  although some options become unavailable, as collevels, and sc
  (amplitude scale). This option is indicated for signals with high
  background noise levels. Palette colors
  [`gray.1`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.3`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`topo.1`](https://rdrr.io/pkg/monitoR/man/specCols.html) and
  [`rainbow.1`](https://rdrr.io/pkg/monitoR/man/specCols.html) (which
  should be imported from the package monitoR) seem to work better with
  'fast' spectrograms. Palette colors
  [`gray.1`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.3`](https://rdrr.io/pkg/monitoR/man/specCols.html) offer
  decreasing darkness levels.

- labels:

  Character string with the name of the column(s) for selection
  labeling. Default is 'selec'. Set to `NULL` to remove labels.

- horizontal:

  Logical. Controls if the images are produced as horizontal or vertical
  pages. Default is `FALSE`.

- song:

  Character string with the name of the column to used as a label a for
  higher organization level in the song (similar to 'song_colm' in
  [`song_analysis`](https://marce10.github.io/warbleR/reference/song_analysis.md)).
  If supplied then lines above the selections belonging to the same
  'song' are plotted. Ignored if 'X' is not provided.

- suffix:

  Character vector of length 1. Suffix for the output image file (to be
  added at the end of the default file name). Default is `NULL`.

- dest.path:

  Character string containing the directory path where the image files
  will be saved. If `NULL` (default) then the folder containing the
  sound files will be used instead.

- only.annotated:

  Logical argument to control if only the pages that contained annotated
  sounds (from 'X') are printed. Only used if 'X' is supplied.

- ...:

  Additional arguments for image formatting. It accepts 'width',
  'height' (which will overwrite 'horizontal') and 'res' as in
  [`png`](https://rdrr.io/r/grDevices/png.html).

## Value

image files with spectrograms of entire sound files in the working
directory. Multiple pages can be returned, depending on the length of
each sound file.

## Details

The function creates spectrograms for complete sound files, printing the
name of the sound files and the "page" number (p1-p2...) at the upper
right corner of the image files. If 'X' is supplied, the function
delimits and labels the selections. This function aims to facilitate
visual inspection of multiple files as well as visual classification of
vocalization units and the analysis of animal vocal sequences.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`full_spectrogram2pdf`](https://marce10.github.io/warbleR/reference/full_spectrogram2pdf.md),
[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md),
[`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# save sound file examples to temporary working directory
data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

full_spectrograms(
  sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
  path = tempdir()
)

# including selections
full_spectrograms(
  sxrow = 2, rows = 8, X = lbh_selec_table,
  pal = reverse.heat.colors, overwrite = TRUE, wl = 300, path = tempdir()
)

# check this floder
# tempdir()
} # }
```
