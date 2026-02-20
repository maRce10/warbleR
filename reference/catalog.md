# Create catalogs of vocal signals

`catalog` produces spectrograms of selections (signals) split into
multiple rows and columns.

## Usage

``` r
catalog(
  X,
  flim = NULL,
  nrow = 4,
  ncol = 3,
  same.time.scale = TRUE,
  collevels = seq(-40, 0, 1),
  ovlp = 50,
  parallel = 1,
  mar = 0.05,
  prop.mar = NULL,
  lab.mar = 1,
  wl = 512,
  wn = "hanning",
  gr = FALSE,
  pal = reverse.gray.colors.2,
  it = "jpeg",
  path = NULL,
  pb = TRUE,
  fast.spec = FALSE,
  res = 100,
  orientation = "v",
  labels = c("sound.files", "selec"),
  height = NULL,
  width = NULL,
  tags = NULL,
  tag.pal = list(temp.colors, heat.colors, topo.colors),
  legend = 3,
  cex = 1,
  leg.wd = 1,
  img.suffix = NULL,
  img.prefix = NULL,
  tag.widths = c(1, 1),
  hatching = 0,
  breaks = c(5, 5),
  group.tag = NULL,
  spec.mar = 0,
  spec.bg = "white",
  max.group.cols = NULL,
  sub.legend = FALSE,
  rm.axes = FALSE,
  title = NULL,
  by.row = TRUE,
  box = TRUE,
  highlight = FALSE,
  alpha = 0.5
)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' or data frame with
  columns for sound file name (sound.files), selection number (selec),
  and start and end time of signal (start and end). Default is `NULL`.

- flim:

  A numeric vector of length 2 indicating the highest and lowest
  frequency limits (kHz) of the spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`.

- nrow:

  A numeric vector of length 1. Specifies number of rows. Default is 4.

- ncol:

  A numeric vector of length 1. Specifies number of columns. Default is
  3.

- same.time.scale:

  Logical. Controls if all spectrograms are in the same time scale (i.e.
  have the same duration).

- collevels:

  A numeric vector of length 3. Specifies levels to partition the
  amplitude range of the spectrogram (in dB). The more levels the higher
  the resolution of the spectrogram. Default is seq(-40, 0, 1).
  seq(-115, 0, 1) will produces spectrograms similar to other acoustic
  analysis software packages.

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

- mar:

  Numeric vector of length 1. Specifies the margins (in seconds)
  adjacent to the start and end points of selections, delineating
  spectrogram limits. Default is 0.05.

- prop.mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of selections as a proportion of the duration of
  the signal. If provided 'mar' argument is ignored. Default is `NULL`.
  Useful when having high variation in signal duration. Ignored if
  `same.time.scale = FALSE`. Must be \> 0 and \<= 1.

- lab.mar:

  Numeric vector of length 1. Specifies the space allocated to labels
  and tags (the upper margin). Default is 1.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- wn:

  Character vector of length 1 specifying the window function name. See
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for name
  options. Default is "hanning".

- gr:

  Logical argument to add grid to spectrogram. Default is `FALSE`.

- pal:

  Color palette function for spectrogram. Default is
  reverse.gray.colors.2. See
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html) for more
  palettes. Palettes as
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html) may work
  better when `fast.spec = TRUE`.

- it:

  A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted. Default is "jpeg".

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
  'fast.spec' spectrograms. Palette colors
  [`gray.1`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.3`](https://rdrr.io/pkg/monitoR/man/specCols.html) offer
  decreasing darkness levels.

- res:

  Numeric argument of length 1. Controls image resolution. Default is
  100 (faster) although 300 is recommended for publication/presentation
  quality. Note that high resolution produce significantly bigger image
  files. This could be problematic when creating pdf files using
  `catalog`.

- orientation:

  String. Indicates whether a letter page size image is produced in
  vertical ('v' option) or horizontal orientation ('h' option). Note
  that width and height can also be specified.

- labels:

  String vector. Provides the column names that will be used as labels
  above the corresponding spectrograms.

- height:

  Numeric. Single value (in inches) indicating the height of the output
  image files. Default is 11 for vertical orientation.

- width:

  Numeric. Single value (in inches) indicating the width of the output
  image files. Default is 8.5 for vertical orientation.

- tags:

  String vector. Provides the column names that will be used for the
  color tagging legend above. Tags can also be numeric. Continuous
  variables would be break down in 10 color classes.

- tag.pal:

  List of color palette function for tags. Should be of length 1, 2
  or 3. Default is `list(temp.colors, heat.colors, topo.colors)`.

- legend:

  A numeric vector of length 1 controlling a legend for color tags is
  added. Ignored if no tags are provided. Four values are allowed:

  - `0`: No label

  - `1`: Label for the first color tag

  - `2`: Label for the second color tag

  - `3`: Labels both color tags

  Default is 3. Currently no legend can be set for group tags. Use
  labels instead.

- cex:

  A numeric vector of length 1 giving the amount by which text
  (including labels and axis) should be magnified. Default is 1.

- leg.wd:

  Numeric. Controls the width of the legend column. Default is 1.

- img.suffix:

  A character vector of length 1 with a suffix (label) to add at the end
  of the names of image files. Default is `NULL` (no suffix). Useful to
  label catalogs from different individuals, species or sites.

- img.prefix:

  A character vector of length 1 with a prefix (label) to add at the
  beginning of the names of image files. Default is `NULL` (no prefix).
  Useful to label catalogs from different individuals, species or sites
  and ensure they will be grouped together when sorted by file name.

- tag.widths:

  A numeric vector of length 2 to control the relative width of the
  color tags (when 2 tags are provided).

- hatching:

  A numeric vector of length 1 controlling cross-hatching is used for
  color tags. Several cross-hatching patterns are used to make tags with
  similar colors more distinguishable. Four values are allowed:

  - `0`: No cross-hatching

  - `1`: Cross-hatching the first color tag

  - `2`: Cross-hatching the second color tag

  - `3`: Cross-hatching both color tags

- breaks:

  Numeric vector of length 1 or 2 controlling the number of intervals in
  which a numeric tag will be divided. The numbers control the first and
  second tags respectively. Ignored if tags are not numeric. Default is
  `c(5, 5)`.

- group.tag:

  Character vector of length 1 indicating the column name to be used to
  color the empty plot areas around the spectrograms. If provided
  selections that belong to the same tag level are clumped together in
  the catalog (the 'X' data frame is sorted by that column). This tags
  cannot be included in the legend so it would be better to use the
  label field to identify the different levels.

- spec.mar:

  Numeric vector of length 1 to add space at the top, left and right
  sides of the spectrogram. Useful to better display the grouping of
  selections when 'group.tag' is provided. Internally applied for
  setting 'mar' using [`par`](https://rdrr.io/r/graphics/par.html).

- spec.bg:

  Character vector of length 1 to control the background color of the
  spectrogram. Default is 'white'. Ignored if `group.tag = NULL`.

- max.group.cols:

  Numeric vector of length 1 indicating the number of different colors
  that will be used for group tags (see 'group.tag' argument). If
  provided (and the number is smaller than the number of levels in the
  'group.tag' column) the colors will be recycled, although ensuring
  that adjacent groups do not share the same color. Useful when the
  'group.tag' has many levels and the colors assigned become very
  similar. Default is `NULL`.

- sub.legend:

  Logical. If `TRUE` then only the levels present on each page are shown
  in the legend. Default is `FALSE`.

- rm.axes:

  Logical. If `TRUE` frequency and time axes are excluded. Default is
  `FALSE`.

- title:

  Character vector of length 1 to set the title of catalogs.

- by.row:

  Logical. If `TRUE` (default) catalogs are filled by rows.

- box:

  Logical. If `TRUE` (default) a box is drawn around spectrograms and
  corresponding labels and tags.

- highlight:

  Logical. If `TRUE` a transparent white layer is plotted on the
  spectrogram areas outside the selection. The level of transparency is
  controlled with the argument 'alpha'. Default is `FAlSE`.

- alpha:

  Numeric vector of length 1 controlling the level of transparency when
  highlighting selections (i.e. when `highlight = TRUE`, see highlight
  argument. Default is 0.5.

## Value

Image files with spectrogram catalogs in the working directory. Multiple
pages can be returned, depending on the length of each sound file.

## Details

This functions aims to simplify the visual exploration of multiple
vocalizations. The function plots a matrix of spectrograms from a
selection table. Spectrograms can be labeled or color tagged to
facilitate exploring variation related to a parameter of interest (e.g.
location, song type). A legend will be added to help match colors with
tag levels (if legend is \> 0). Different color palettes can be used for
each tag. Numeric tags are split in intervals (the number of intervals
can be controlled with break argument). The width and height can also be
adjusted to fit more column and/or rows. This files can be put together
in a single pdf file with
[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md).
We recommend using low resolution (~60-100) and smaller dimensions
(width & height \< 10) if aiming to generate pdfs (otherwise pdfs could
be pretty big).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples
