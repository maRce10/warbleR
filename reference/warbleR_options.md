# Setting warbleR options

`warbleR_options` sets global parameters for warbleR functions

## Usage

``` r
warbleR_options(reset = FALSE, ...)
```

## Arguments

- reset:

  Logical. If `TRUE` then all global parameters are removed. Default is
  `FALSE`.

- ...:

  Arguments in \`parameter = value\` form, or a list of tagged values.
  The tags (i.e. parameters) must come from the list of parameters
  described below.

## Value

When parameters are set by warbleR_options, their former values are
returned in an invisible named list. Such a list can be passed as an
argument to pboptions to restore the parameter values. If the function
is called with no arguments the current option values are printed.

## Details

The function aims to simplify the use of parameters that apply to many
warbleR functions (i.e. global parameters) by setting a default value
that will be used to any function in downstream analyses. Tags that are
set with warbleR_options will be used iby the functions that share those
arguments. However, if an argument is set within a function call it will
overwrite the values set by warbleR_options. Hence, the functions remain
'flexible' as their parameters can also be modified 'on the fly'. The
following tags are available:

- `bp`: Numeric vector of length 2 giving the lower and upper limits of
  a frequency bandpass filter (in kHz).

- `collevels`: A numeric vector of length 3. Specifies levels to
  partition the amplitude range of the spectrogram (in dB) as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). The more
  levels the higher the resolution of the spectrogram. The lower the
  first value the darker the spectrograms.

- `flim`: A numeric vector of length 2 for the frequency limit in kHz of
  the spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- `it`: A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted.

- `osci`: Logical argument to add an oscillogram underneath spectrogram,
  as in [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- `pal`: A color palette function to be used to assign colors in the
  plot, as in [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- `parallel`: Numeric. Controls whether parallel computing is applied.
  It specifies the number of cores to be used in iterative functions.

- `pb`: Logical argument to control whether progress bar is used.

- `res`: Numeric argument of length 1. Controls image resolution in all
  image creating functions.

- `wav.path`: Character string containing the directory path where the
  sound files are located. Used as 'path' in all functions in which
  sound files are read.

- `wl`: A numeric vector of length 1 specifying the window length for
  creating spectrogram (either for plotting or for measuring spectrogram
  parameters).

- `wn`: Character vector of length 1 specifying the window name for
  creating spectrogram (either for plotting or for measuring spectrogram
  parameters). See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for
  options.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data and save in temporary working directory
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # sig2noise with progress bar (by default is TRUE)
  a <- sig2noise(X = lbh_selec_table, mar = 0.1, path = tempdir())

  # set progress bar to FALSE with warbleR_options
  warbleR_options(pb = FALSE, path = tempdir())

  # sig2noise without progress bar
  a <- sig2noise(X = lbh_selec_table, mar = 0.1)

  # sig2noise with progress bar by setting it within the function call (overwritting options)
  a <- sig2noise(X = lbh_selec_table, pb = TRUE, mar = 0.1)

  # sig2noise without progress bar using warbleR_options setting again
  a <- sig2noise(X = lbh_selec_table, mar = 0.1)
}
```
