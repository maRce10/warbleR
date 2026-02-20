# Check sound files

`check_sound_files` checks whether sound files can be read by subsequent
functions.

## Usage

``` r
check_sound_files(
  X = NULL,
  parallel = 1,
  path = NULL,
  check.header = FALSE,
  verbose = TRUE
)
```

## Arguments

- X:

  Optional. 'selection_table' object or data frame with the following
  columns: 1) "sound.files": name of the sound files, 2) "selec": number
  of the selections, 3) "start": start time of selections, 4) "end": end
  time of selections. If provided the function also returns the smallest
  number of samples from the listed selections, which limits the minimum
  window length (wl argument in other functions) that can be used in
  batch analyses. This could be useful for avoiding errors in downstream
  functions (e.g.
  [`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md)).

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- check.header:

  Logical. Checks whether number of samples in the file header matches
  that in the actual file (i.e. if the header is corrupted). This could
  significantly affect the performance of the function (much slower)
  particularly with long sound files.

- verbose:

  Logical to control whether the summary messages are printed to the
  console. Defaut is `TRUE`.

## Value

If all sound files are ok, returns message "All files can be read".
Otherwise returns the names of the corrupted sound files.

## Details

This function checks if sound files in the working directory can be
read. Users must set the working directory where they wish to check
sound files beforehand. If X is provided it also returns the smallest
number of samples from the selections listed in X (if all files can be
read). Note that corrupt files can be fixed using
[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md))
('sox' must be installed to be able to run this function). The function
is intended for a "quick and dirty" check of the sound files in a
selections data frame. For a more thorough analysis see
[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)
[`tailor_sels`](https://marce10.github.io/warbleR/reference/tailor_sels.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# without selection data frame
check_sound_files(path = tempdir())

# with selection data frame
check_sound_files(X = lbh_selec_table, path = tempdir())
}
#> All files can be read
#> 
#> All files can be read
#> 
#> smallest number of samples: 2838 (sound file:Phae.long2.wav; selection label: 2)
#> 
```
