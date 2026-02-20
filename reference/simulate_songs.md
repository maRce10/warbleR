# Simulate animal vocalizations

`simulate_songs` simulate animal vocalizations in a wave object under
brownian motion frequency drift.

## Usage

``` r
simulate_songs(
  n = 1,
  durs = 0.2,
  harms = 3,
  harm.amps = c(1, 0.5, 0.2),
  am.amps = 1,
  gaps = 0.1,
  freqs = 5,
  samp.rate = 44.1,
  sig2 = 0.5,
  steps = 10,
  bgn = 0.5,
  seed = NULL,
  diff.fun = "GBM",
  fin = 0.1,
  fout = 0.2,
  shape = "linear",
  selec.table = FALSE,
  file.name = NULL,
  path = NULL,
  hrm.freqs = c(1/2, 1/3, 2/3, 1/4, 3/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10),
  freq.range = 4
)
```

## Arguments

- n:

  Number of song subunits (e.g. elements). Default is 1.

- durs:

  Numeric vector with the duration of subunits in seconds. It should
  either be a single value (which would be used for all subunits) or a
  vector of length `n`.

- harms:

  NUmeric vector of length 1 specifying the number of harmonics to
  simulate. 1 indicates that only the fundamental frequency harmonic
  will be simulated.

- harm.amps:

  Numeric vector with the relative amplitude of each of the harmonics
  (including the fundamental frequency).

- am.amps:

  Numeric vector with the relative amplitude for each step (see 'step'
  argument) to simulate amplitude modulation (only applied to the
  fundamental frequency). Should have the same length as the number of
  steps. Default is 1 (no amplitude modulation).

- gaps:

  Numeric vector with the duration of gaps (silence between subunits) in
  seconds. It should either be a single value (which would be used for
  all subunits) or a vector of length `n + 1`.

- freqs:

  Numeric vector with the initial frequency of the subunits (and ending
  frequency if `diff.fun == "BB"`) in kHz. It should either be a single
  value (which would be used for all subunits) or a vector of length
  `n`.

- samp.rate:

  Numeric vector of length 1. Sets the sampling frequency of the wave
  object (in kHz). Default is 44.1.

- sig2:

  Numeric vector defining the sigma value of the brownian motion model.
  It should either be a single value (which would be used for all
  subunits) or a vector of length `n + 1`. Higher values will produce
  faster frequency modulations. Only applied if `diff.fun == "GBM"`.
  Default is 0.1. Check the
  [`GBM`](https://rdrr.io/pkg/Sim.DiffProc/man/ABM.html) function from
  the Sim.DiffProc package for more details.

- steps:

  Numeric vector of length 1. Controls the mean number of segments in
  which each song subunit is split during the brownian motion process.
  If not all subunits have the same duration, longer units will be split
  in more steps (although the average duration subunit will have the
  predefined number of steps). Default is 10.

- bgn:

  Numeric vector of length 1 indicating the background noise level. 0
  means no additional noise will 1 means noise at the same amplitude
  than the song subunits. Default is 0.5.

- seed:

  Numeric vector of length 1. This allows users to get the same results
  in different runs (using
  [`set.seed`](https://rdrr.io/r/base/Random.html) internally). Default
  is `NULL`.

- diff.fun:

  Character vector of length 1 controlling the function used to simulate
  the brownian motion process of frequency drift across time. Only "BB",
  "GBM" and "pure.tone" are accepted at this time. Check the
  [`GBM`](https://rdrr.io/pkg/Sim.DiffProc/man/ABM.html) function from
  the Sim.DiffProc package for more details.

- fin:

  Numeric vector of length 1 setting the proportion of the sub-unit to
  fade-in amplitude (value between 0 and 1). Default is 0.1. Note that
  'fin' + 'fout' cannot be higher than 1.

- fout:

  Numeric vector of length 1 setting the proportion of the sub-unit to
  fade-out amplitude (value between 0 and 1). Default is 0.2. Note that
  'fin' + 'fout' cannot be higher than 1.

- shape:

  Character string of length 1 controlling the shape of in and out
  amplitude fading of the song sub-units ('fin' and 'fout'). "linear"
  (default), "exp" (exponential), and "cos" (cosine) are currently
  allowed.

- selec.table:

  Logical. If `TRUE` the function returns a list with two elements: 1) a
  data frame containing the start/end time, and bottom/top frequency of
  the sub-units and 2) the wave object containing the simulated songs.
  If `FALSE` (default) no objects are returned. Regardless of the value
  of this argument a .wav file is always saved in the working directory.

- file.name:

  Character string for naming the ".wav" file. Ignored if 'selec.table'
  is `FALSE`. If not provided the date-time stamp will be used.

- path:

  Character string with the directory path where the sound file should
  be saved. Ignored if 'selec.table' is `FALSE`. If `NULL` (default)
  then the current working directory is used.

- hrm.freqs:

  Numeric vector with the frequencies of the harmonics relative to the
  fundamental frequency. The default values are c(1/2, 1/3, 2/3, 1/4,
  3/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10).

- freq.range:

  Numeric vector of length 1 with the frequency range around the
  simulated frequency in which signals will modulate. Default is 4 which
  means that sounds will range +/- 2 kHz around the target frequency. If
  `NULL` the frequency range is not constrained.

## Value

A wave object containing the simulated songs. If 'selec.table' is `TRUE`
the function saves the wave object as a '.wav' sound file in the working
directory (or 'path') and returns a list including 1) a selection table
with the start/end time, and bottom/top frequency of the sub-units and
2) the wave object.

## Details

This functions uses a geometric (`diff.fun == "GBM"`) or Brownian bridge
(`diff.fun == "BB"`) motion stochastic process to simulate modulation in
animal vocalizations (i.e. frequency traces across time). The function
can also simulate pure tones (`diff.fun == "pure.tone"`, 'sig2' is
ignored). Several song subunits (e.g. elements) can be simulated as well
as the corresponding harmonics. Modulated sounds are adjusted so the
mean frequency of the frequency contour is equal to the target frequency
supplied by the user.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`tweak_spectro`](https://marce10.github.io/warbleR/reference/tweak_spectro.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# simulate a song with 3 elements and no harmonics
sm_sng <- simulate_songs(n = 3, harms = 1)

# plot spectro
seewave::spectro(sm_sng)

# simulate a song with 5 elements and 2 extra harmonics
sm_sng2 <- simulate_songs(n = 5, harms = 3)

# plot spectrogram
seewave::spectro(sm_sng2)

# six pure tones with frequency ranging form 4 to 6 and returning selection table
sm_sng <- simulate_songs(
  n = 6, harms = 1, seed = 1, diff.fun = "pure.tone",
  freqs = seq(4, 6, length.out = 6), selec.table = TRUE,
  path = tempdir()
)

# plot spectro
seewave::spectro(sm_sng$wave, flim = c(2, 8))

# selection table
sm_sng$selec.table
} # }
```
