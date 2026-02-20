# Estimate acoustic activity across sound files based on selections

`acoustic_activity` estimates acoustic activity across sound files based
on selections.

## Usage

``` r
acoustic_activity(
  X,
  time.window = 60,
  hop.size = 1,
  path = ".",
  Y = NULL,
  files = unique(X$sound.files),
  parallel = 1,
  pb = TRUE
)
```

## Arguments

- X:

  'selection_table' object or data frame with the following columns: 1)
  "sound.files": name of the .wav files, 2) "selec": number of the
  selections, 3) "start": start time of selections, 4) "end": end time
  of selections.

- time.window:

  Numeric. The time window (in s) in which to calculate acoustic
  activity. Default is 60 seconds.

- hop.size:

  Numeric. The hop size in seconds to calculate acoustic activity. It
  refers to the spacing between consecutive time windows. If
  `hop.size == time.window` then there is no overlap between time
  windows. Default is 1 second.

- path:

  Character string containing the directory path where the sound files
  are located. By default the current working directory is used.

- Y:

  Optional. A data frame with the duration of the sound files in 'X'. It
  must have the columns 'sound.files' and 'duration'. If not provided,
  durations will be estimated from the sound files in 'path' using
  [`duration_sound_files`](https://marce10.github.io/warbleR/reference/duration_sound_files.md).

- files:

  Character vector with the names of the sound files to be used in the
  analysis. Default is `unique(X$sound.files)`. Use
  `list.files(tempdir(), pattern = ".wav$")` (or modify according to
  file extension) for including all sound files in the 'path' supplied
  (even those with no selections in 'X').

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

## Value

A data frame including the following columns:

- `sound.files`: files in which acoustic activity was measured

- `start`: start of the time window where selections were counted (in
  seconds)

- `end`: end of the time window where selections were counted (in
  seconds)

- `counts`: number of selections in the time window (counted if the
  middle point of the selection is within the time window). Note that
  the last time window may not have the same length as the others if the
  sound file duration is not a multiple of the time window.

- `rate`: number of selections per second.

## Details

This function computes two features related to acoustic activity: \#'

- `counts`: number of sound events in the time window.

- `rate`: number of sound events per second in the time window.

Features are estimates across sound files based on selections. A sound
event is counted as present in a time window if its middle point
(`(X$end + X$start) / 2`) is within that window. Acoustic activity rates
(e.g. calls per minute) are a widely used metric in neuroscience
research, providing quantitative insight into rodent ultrasonic
vocalizations as indicators of affective states, social interactions,
and motivational processes (e.g. Rojas-Carvajal et al. 2023, Wardak et
al. 2024).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

Rojas-Carvajal, M., Leandro, R., & Brenes, J. C. (2023). Distinct acute
stressors exert an antagonistic effect on complex grooming during
novelty habituation in rats. Behavioural Processes, 212, 104931.

Rojas-Carvajal, M., Sequeira-Cordero, A., & Brenes, J. C. (2020).
Neurobehavioral effects of restricted and unpredictable environmental
enrichment in rats. Frontiers in pharmacology, 11, 674.

Wardak, A. D., Olszyński, K. H., Polowy, R., Matysiak, J., &
Filipkowski, R. K. (2024). Rats that learn to vocalize for food reward
emit longer and louder appetitive calls and fewer short aversive calls.
Plos one, 19(2), e0297174.

## See also

[`inflections`](https://marce10.github.io/warbleR/reference/inflections.md),
[`song_analysis`](https://marce10.github.io/warbleR/reference/song_analysis.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# get vocal activity by second
va <- acoustic_activity(X = lbh_selec_table, path = tempdir(), time.window = 1,
                   hop.size = 1)

# get the row with the highest rate per sound file
do.call(rbind, lapply(split(va, va$sound.files), function(x)
x[which.max(x$rate), ]))

#including a file with no annotations
writeWave(Phae.long1, file.path(tempdir(), "no_anns.wav"))

va <- acoustic_activity(X = lbh_selec_table, path = tempdir(), time.window = 1,
hop.size = 1, files = list.files(tempdir(), pattern = ".wav$"))
}
```
