# Plot a mosaic of spectrograms with varying display parameters

`tweak_spectro` plots a mosaic of spectrograms with varying display
parameters to facilitate selection of display parameters

## Usage

``` r
tweak_spectro(
  X,
  length.out = 5,
  ovlp = 90,
  wl = c(100, 1000),
  wn = "hanning",
  collev.min = -40,
  pal = "reverse.gray.colors.2",
  path = NULL,
  rm.axes = TRUE,
  ...
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or data
  frame with a single row and columns for sound file name (sound.files),
  selection number (selec), and start and end time of signal (start and
  end). Default is `NULL`.

- length.out:

  Numeric vector of length 1 controlling the number of sublevels of the
  numeric arguments for which a range has been provided. Ranges are
  allowed for 'ovlp', 'wl', and 'collev.min' arguments.

- ovlp:

  Numeric vector of length 1 or 2 specifying % of overlap (or
  lower/upper values the desired range) between two consecutive windows,
  as in [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).
  Default is 90.

- wl:

  A numeric vector of length 1 or 2 specifying the window length
  (length 1) or the lower and upper range limits of the desired window
  length range (length 2) for creating spectrograms. Default is c(100,
  1000).

- wn:

  Character vector specifying the window function names to be used.
  Several names can be provided. See
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for name
  options. Default is "hanning". If "all", then all window functions
  available are used.

- collev.min:

  A (negative) numeric vector of length 1 or 2. Determines the first
  argument to use in 'collevels' for the internal spectrogram creating
  function. This replaces the first element in the 'collevels' as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Note that
  'collevels' is not available in this function `tweak_spectro`.

- pal:

  Color palette function for spectrogram. Default is
  "reverse.gray.colors.2". Several palettes can be provided in a
  character vector. Note that, contrary to other `warbleR` and `seewave`
  functions, the palette most be provided as character string rather
  than as a function. See
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html) for more
  palettes.

- path:

  Character string containing the directory path where the sound file
  are located.

- rm.axes:

  Logical. If `TRUE` frequency and time axes are excluded. Default is
  `TRUE`.

- ...:

  Additional arguments to be passed to
  [`catalog`](https://marce10.github.io/warbleR/reference/catalog.md)
  function for customizing graphical output. Check out
  [`catalog`](https://marce10.github.io/warbleR/reference/catalog.md)
  for more details.

## Value

Image files with spectrograms of entire sound files in the working
directory. Multiple pages can be returned, depending on the length of
each sound file.

## Details

This functions aims to simplify the selection of spectrogram parameters.
The function plots, for a single selection, a mosaic of spectrograms
with varying display parameters. For numeric arguments the upper and
lower limits of a range can be provided. The following arguments accept
can have varying values:

- `wl`: Windows length (numeric range)

- `ovlp`: Overlap (numeric range)

- `collev.min`: Minimum value of the color levels (numeric range)

- `wn`: window function names (character)

- `pal`: palette (character)

Outputs are similar to those of
[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md). The
output image files can be put together in a single pdf file with
[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md).
We recommend using low resolution (~60-100) and smaller dimensions
(width & height \< 10) if aiming to generate pdfs (otherwise pdfs could
be pretty big).

## See also

[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples
