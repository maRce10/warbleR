# Measure relative sound pressure level

`sound_pressure_level` measures relative (uncalibrated) sound pressure
level in signals referenced in a selection table.

## Usage

``` r
sound_pressure_level(
  X,
  reference = 20,
  parallel = 1,
  path = NULL,
  pb = TRUE,
  type = "single",
  wl = 100,
  bp = NULL,
  remove.bgn = FALSE,
  mar = NULL,
  envelope = "abs"
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or any
  data frame with columns for sound file name (sound.files), selection
  number (selec), and start and end time of signal (start and end).

- reference:

  Numeric vector of length 1 indicating the pressure (in µPa) to be used
  as reference. Alternatively, a character vector with the name of a
  numeric column containing reference values for each row can be
  supplied. Default is 20 (µPa). NOT YET IMPLEMENTED.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing). It can also be set globally using the 'parallel' option
  (see
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used. It can also be set globally using the 'wav.path' option (see
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).

- pb:

  Logical argument to control if progress bar is shown. Default is
  `TRUE`. It can also be set globally using the 'pb' option (see
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).

- type:

  Character string controlling how SPL is measured: \#'

  - `single`: single SPL value obtained on the entire signal. Default.

  - `mean`: average of SPL values measured across the signal.

  - `peak`: maximum of several SPL values measured across the signal.

- wl:

  A numeric vector of length 1 specifying the spectrogram window length.
  Default is 512.

- bp:

  Numeric vector of length 2 giving the lower and upper limits of a
  frequency bandpass filter (in kHz). Alternatively, when set to
  'freq.range', the function will use the 'bottom.freq' and 'top.freq'
  for each signal as the bandpass range. Default is `NULL` (no bandpass
  filter).

- remove.bgn:

  Logical argument to control if SPL from background noise is excluded
  from the measured signal SPL. Default is `FALSE`.

- mar:

  numeric vector of length 1. Specifies the margins adjacent to the
  start point of selection over which to measure background noise.

- envelope:

  Character string vector with the method to calculate amplitude
  envelopes (in which SPL is measured), as in
  [`env`](https://rdrr.io/pkg/seewave/man/env.html). Must be either
  'abs' (absolute envelope, default) or 'hil' (Hilbert transformation).

## Value

The object supplied in 'X' with a new variable with the sound pressure
level values ('SPL' or 'peak.amplitude' column, see argument
'peak.amplitude') in decibels.

## Details

Sound pressure level (SPL) is a logarithmic measure of the effective
pressure of a sound relative to a reference, so it's a measure of sound
intensity. SPL is measured as the root mean square of the amplitude
vector, and as such is only a useful metric of the variation in loudness
for signals within the same recording (or recorded with the same
equipment and gain).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191. [Wikipedia: Sound pressure
level](https://en.wikipedia.org/wiki/Sound_pressure)

## See also

[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md).

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
{
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files

  spl <- sound_pressure_level(
    X = lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
    parallel = 1, pb = TRUE, path = tempdir()
  )
}
```
