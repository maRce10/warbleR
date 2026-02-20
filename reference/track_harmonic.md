# Track harmonic frequency contour

`track_harmonic` tracks the frequency contour of the dominant harmonic.

## Usage

``` r
track_harmonic(
  wave,
  f,
  wl = 512,
  wn = "hanning",
  ovlp = 0,
  fftw = FALSE,
  at = NULL,
  tlim = NULL,
  threshold = 10,
  bandpass = NULL,
  clip = NULL,
  plot = TRUE,
  xlab = "Times (s)",
  ylab = "Frequency (kHz)",
  ylim = c(0, f/2000),
  adjust.wl = FALSE,
  dfrq = FALSE,
  ...
)
```

## Arguments

- wave:

  A 'wave' object produced by
  [`readWave`](https://rdrr.io/pkg/tuneR/man/readWave.html) or similar
  functions.

- f:

  Sampling frequency of the wave object (in Hz). Does not need to be
  specified if embedded in wave.

- wl:

  A numeric vector of length 1 specifying the window length for the FFT,
  default is 512.

- wn:

  Character vector of length 1 specifying window name. Default is
  "hanning". See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for more
  options. This is used for calculating the frequency spectrum (using
  [`meanspec`](https://rdrr.io/pkg/seewave/man/meanspec.html)) and
  producing the spectrogram (using
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html), if
  `plot = TRUE`).

- ovlp:

  Numeric vector of length 1 specifying % of overlap between two
  consecutive time windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  0.

- fftw:

  if TRUE calls the function FFT of the library fftw. See Notes of the
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html) function.
  Default is `FALSE`.

- at:

  Time position where the harmonic frequency contour has to be computed
  (in seconds). Default is `NULL`.

- tlim:

  time range in which to measure frequency contours. Default is `NULL`
  (which means it will measure across the entire wave object).

- threshold:

  Amplitude threshold (%) for dominant frequency and detection. Default
  is 10.

- bandpass:

  A numeric vector of length 2 for the lower and upper limits of a
  frequency bandpass filter (in kHz).

- clip:

  A numeric value to select dominant frequency values according to their
  amplitude in reference to a maximal value of 1 for the whole signal
  (has to be \>0 & \< 1).

- plot:

  Logical, if TRUE plots the dominant frequency against time. Default is
  `TRUE`.

- xlab:

  Label of the time axis.

- ylab:

  Label of the frequency axis.

- ylim:

  A numeric vector of length 2 for the frequency limit of the
  spectrogram (in kHz), as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  c(0, f/2000).

- adjust.wl:

  Logical. If `TRUE` 'wl' (window length) is reset to be lower than the
  number of samples in a selection if the number of samples is less than
  'wl'. Default is `FALSE`.

- dfrq:

  Logical. If `TRUE` seewave's
  [`dfreq`](https://rdrr.io/pkg/seewave/man/dfreq.html) is used instead.
  Default is `FALSE`.

- ...:

  Additional arguments to be passed to the plotting function.

## Details

This is a modified version of seewave's
[`dfreq`](https://rdrr.io/pkg/seewave/man/dfreq.html) function that
allows to track the frequency contour of a dominant harmonic even when
the highest amplitude jumps between harmonics. The arguments and default
values of the original
[`dfreq`](https://rdrr.io/pkg/seewave/man/dfreq.html) function have been
kept unchanged to facilitate switching between the 2 functions.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)
for tracking frequencies iteratively on selections tables.

## Author

Jerome Sueur, modified by Marcelo Araya-Salas
(<marcelo.araya@ucr.ac.cr>)
