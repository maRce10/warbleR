# Measure signal-to-noise ratio

`sig2noise` measures signal-to-noise ratio across multiple files.

## Usage

``` r
sig2noise(
  X,
  mar,
  parallel = 1,
  path = NULL,
  pb = TRUE,
  type = 1,
  eq.dur = FALSE,
  in.dB = TRUE,
  before = FALSE,
  lim.dB = TRUE,
  bp = NULL,
  wl = 10
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or any
  data frame with columns for sound file name (sound.files), selection
  number (selec), and start and end time of signal (start and end).

- mar:

  numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of selection over which to measure noise.

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

  Numeric. Determine the formula to be used to calculate the
  signal-to-noise ratio (S = signal , N = background noise):

  - `1`: ratio of S mean amplitude envelope to N mean amplitude envelope
    (`mean(env(S))/mean(env(N))`)

  - `2`: ratio of S amplitude envelope RMS (root mean square) to N
    amplitude envelope RMS (`rms(env(S))/rms(env(N))`)

  - `3`: ratio of the difference between S amplitude envelope RMS and N
    amplitude envelope RMS to N amplitude envelope RMS
    (`(rms(env(S)) - rms(env(N)))/rms(env(N))`)

- eq.dur:

  Logical. Controls whether the noise segment that is measured has the
  same duration than the signal (if `TRUE`, default `FALSE`). If `TRUE`
  then 'mar' argument is ignored.

- in.dB:

  Logical. Controls whether the signal-to-noise ratio is returned in
  decibels (20\*log10(SNR)). Default is `TRUE`.

- before:

  Logical. If `TRUE` noise is only measured right before the signal
  (instead of before and after). Default is `FALSE`.

- lim.dB:

  Logical. If `TRUE` the lowest signal-to-noise would be limited to -40
  dB (if `in.dB = TRUE`). This would remove NA's that can be produced
  when noise segments have a higher amplitude than the signal itself.
  Default is `TRUE`.

- bp:

  Numeric vector of length 2 giving the lower and upper limits of a
  frequency bandpass filter (in kHz). Default is `NULL`.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram for applying bandpass. Default is 10. Ignored if
  `bp = NULL`. It can also be set globally using the 'wl' option (see
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).
  Note that lower values will increase time resolution, which is more
  important for signal-to-noise ratio calculations.

## Value

The input 'X' object with a new column including the signal-to-noise
values.

## Details

Signal-to-noise ratio (SNR) is a measure of the level of a desired
signal compared to background noise. The function divides the mean
amplitude of the signal by the mean amplitude of the background noise
adjacent to the signal. A general margin to apply before and after the
acoustic signal must be specified. Setting margins for individual
signals that have been previously clipped from larger files may take
some optimization, as for calls within a larger file that are
irregularly separated. When margins overlap with another acoustic signal
nearby, the signal-to-noise ratio (SNR) will be inaccurate. Any SNR less
than or equal to one suggests background noise is equal to or
overpowering the acoustic signal.
[`snr_spectrograms`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md)
can be used to troubleshoot different noise margins.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191. [Wikipedia: Signal-to-noise
ratio](https://en.wikipedia.org/wiki/Signal-to-noise_ratio)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
{
  data(list = c("Phae.long1", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files

  # specifying the correct margin is important
  # use snr_spectrograms to troubleshoot margins for sound files
  sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
    mar = 0.2,
    path = tempdir()
  )

  # this smaller margin doesn't overlap neighboring signals
  sig2noise(lbh_selec_table[grep("Phae.long1", lbh_selec_table$sound.files), ],
    mar = 0.1,
    path = tempdir()
  )
}
#>      sound.files channel selec     start       end bottom.freq top.freq
#> 1 Phae.long1.wav       1     1 1.1693549 1.3423884    2.220105 8.604378
#> 2 Phae.long1.wav       1     2 2.1584085 2.3214565    2.169437 8.807053
#> 3 Phae.long1.wav       1     3 0.3433366 0.5182553    2.218294 8.756604
#>        SNR
#> 1 23.55816
#> 2 22.81263
#> 3 21.12933
```
