# Interactive view of spectrograms to tailor selections

`tailor_sels` produces an interactive spectrographic view in which the
start/end times and frequency range of acoustic signals listed in a data
frame can be adjusted.

## Usage

``` r
tailor_sels(
  X = NULL,
  wl = 512,
  flim = c(0, 22),
  wn = "hanning",
  mar = 0.5,
  osci = TRUE,
  pal = reverse.gray.colors.2,
  ovlp = 70,
  auto.next = FALSE,
  pause = 1,
  comments = TRUE,
  path = NULL,
  frange = TRUE,
  fast.spec = FALSE,
  ext.window = TRUE,
  width = 15,
  height = 5,
  index = NULL,
  collevels = NULL,
  title = c("sound.files", "selec"),
  ts.df = NULL,
  col = "#E37222",
  alpha = 0.7,
  auto.contour = FALSE,
  ...
)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' object or data frame
  with the following columns: 1) "sound.files": name of the .wav
  files, 2) "selec": number of the selections, 3) "start": start time of
  selections, 4) "end": end time of selections. Notice that, if an
  output file ("seltailor_output.csv") is found in the working directory
  it will be given priority over an input data frame.

- wl:

  A numeric vector of length 1 specifying the spectrogram window length.
  Default is 512.

- flim:

  A numeric vector of length 2 specifying the frequency limit (in kHz)
  of the spectrogram, as in the function
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  c(0,22).

- wn:

  A character vector of length 1 specifying the window function (by
  default "hanning"). See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for more
  options.

- mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of the selections to define spectrogram limits.
  Default is 0.5.

- osci:

  Logical argument. If `TRUE` adds a oscillogram whenever the
  spectrograms are produced with higher resolution (see seltime).
  Default is `TRUE`. The external program must be closed before resuming
  analysis. Default is `NULL`.

- pal:

  A color palette function to be used to assign colors in the plot, as
  in [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is reverse.gray.colors.2. See Details.

- ovlp:

  Numeric vector of length 1 specifying the percent overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  70.

- auto.next:

  Logical argument to control whether the functions moves automatically
  to the next selection. The time interval before moving to the next
  selection is controlled by the 'pause' argument. Ignored if
  `ts.df = TRUE`.

- pause:

  Numeric vector of length 1. Controls the duration of the waiting
  period before moving to the next selection (in seconds). Default is 1.

- comments:

  Logical argument specifying if 'sel.comment' (when in data frame)
  should be included in the title of the spectrograms. Default is
  `TRUE`.

- path:

  Character string containing the directory path where the sound files
  are located.

- frange:

  Logical argument specifying whether limits on frequency range should
  be recorded. If `TRUE` (default) time and frequency limits are
  recorded.

- fast.spec:

  Logical. If `TRUE` then image function is used internally to create
  spectrograms, which substantially increases performance (much faster),
  although some options become unavailable, as sc (amplitude scale).
  This option is indicated for signals with high background noise
  levels. Palette colors
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

- ext.window:

  Logical. If `TRUE` then and external graphic window is used. Default
  dimensions can be set using the 'width' and 'height' arguments.
  Default is `TRUE`.

- width:

  Numeric of length 1 controlling the width of the external graphic
  window. Ignored if `ext.window = FALSE`. Default is 15.

- height:

  Numeric of length 1 controlling the height of the external graphic
  window. Ignored if `ext.window = FALSE`. Default is 5.

- index:

  Numeric vector indicating which selections (rows) of 'X' should be
  tailored. Default is `NULL`. Ignored when the process is resumed. This
  can be useful when combined with
  [`filter_sels`](https://marce10.github.io/warbleR/reference/filter_sels.md))
  output (see 'index' argument in
  [`filter_sels`](https://marce10.github.io/warbleR/reference/filter_sels.md)).

- collevels:

  Numeric. Set of levels used to partition the amplitude range (see
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html)).

- title:

  Character vector with the names of the columns to be included in the
  title for each selection.

- ts.df:

  Optional. Data frame with frequency contour time series of signals to
  be tailored. If provided then 'autonext' is set to `FALSE`. Default is
  `NULL`. The data frame must include the 'sound.files' and 'selec'
  columns for the same selections included in 'X'.

- col:

  Character vector defining the color of the points when 'ts.df' is
  provided. Default is "#E37222" (orange).

- alpha:

  Numeric of length one to adjust transparency of points when adjusting
  frequency contours.

- auto.contour:

  Logical. If `TRUE` contours are displayed automatically (without
  having to click on 'contour'). Note that adjusting the selection box
  (frequency/time limits) won't be available. Default is `FALSE`.
  Ignored if 'ts.df' is not provided.

- ...:

  Additional arguments to be passed to the internal spectrogram creating
  function for customizing graphical output. The function is a modified
  version of [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html),
  so it takes the same arguments.

## Value

data frame similar to X with the and a .csv file saved in the working
directory with start and end time of selections.

## Details

This function produces an interactive spectrographic view in which users
can select new time/frequency coordinates the selections. 4 "buttons"
are provided at the upper right side of the spectrogram that allow to
stop the analysis (stop symbol, a solid rectangle), go to the next sound
file ("\>\>"), return to the previous selection ("\<\<") or delete the
current selection ("X"). An additional "button" to tailored frequency
contour is shown when 'ts.df' is provided. The button contains a symbol
with a 4 point contour. When a unit has been selected, the function
plots dotted lines in the start and end of the selection in the
spectrogram (or a box if `frange = TRUE`). Only the last selection is
kept for each selection that is adjusted. The function produces a .csv
file (seltailor_output.csv) with the same information than the input
data frame, except for the new time coordinates, plus a new column
(X\$tailored) indicating if the selection has been tailored. The file is
saved in the working directory and is updated every time the user moves
into the next sound file ("\>\>") or stop the process (stop "button").
It also return the same data frame as and object in the R environment.
If no selection is made (by clicking on "\>\>") the original
time/frequency coordinates are kept. When resuming the process (after
"stop" and re-running the function in the same working directory), the
function will continue working on the selections that have not been
analyzed. When deleting a file (X button) an orange "X" when returning
to that selection. If X is used again the selection is recovered. The
function also displays a progress bar right on top of the spectrogram.
The zoom can be adjusted by setting the `mar` argument. To fix contours
a data.frame containing the 'sound.files' and 'selec' columns as in 'X'
as well as the frequency values at each contour step must be provided.
The function plots points corresponding to the time/frequency
coordinates of each element of the contour. Clicking on the spectrogram
will substitute the frequency value of the points. The contour point
closest in time to the "click" will be replaced by the frequency value
of the "click".

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

tailor_sels(X = lbh_selec_table, flim = c(1, 12), wl = 300, auto.next = TRUE, path = tempdir())

# Read output .csv file
seltailor.df <- read.csv(file.path(tempdir(), "seltailor_output.csv"))
seltailor.df

# check this directory for .csv file after stopping function
tempdir()
} # }
```
