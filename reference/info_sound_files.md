# Get sound file parameter information

`info_sound_files` summariz sound file information

## Usage

``` r
info_sound_files(
  path = NULL,
  files = NULL,
  parallel = 1,
  pb = TRUE,
  skip.error = FALSE,
  file.format = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$"
)
```

## Arguments

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- files:

  character vector indicating the set of files that will be
  consolidated. File names should not include the full file path.
  Optional.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

- skip.error:

  Logical to control if errors are omitted. If so, files that could not
  be read will be excluded and their name printed in the console.
  Default is `FALSE`, which will return an error if some files are
  problematic.

- file.format:

  Character string with the format of sound files. By default all sound
  file formats supported by warbleR are included
  ("\\.wav\$\|\\.wac\$\|\\.mp3\$\|\\.flac\$"). Note that several formats
  can be included using regular expression syntax as in
  [`grep`](https://rdrr.io/r/base/grep.html). For instance
  `"\\.wav$|\\.mp3$"` will only include .wav and .mp3 files.

## Value

A data frame with descriptive information about the sound files in the
working directory (or 'path'). See "details".

## Details

This function is a wrapper for
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
that returns a data frame with the following descriptive parameters for
each sound file in the working directory (or 'path'):

- `duration`: duration of selection in seconds

- `sample.rate`: sampling rate in kHz

- `channels`: number of channels

- `bits`: bit depth

- `wav.size`: sound file size in MB

- `samples`: number of samples in the sound file

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md),
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
&
[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

#get info
info_sound_files(path = tempdir())
}
#>         sound.files  duration sample.rate channels bits wav.size samples
#> 1  Phae.long1-1.wav 0.2730667        22.5        1   16 0.012332    6144
#> 2  Phae.long1-2.wav 0.2630667        22.5        1   16 0.011882    5919
#> 3  Phae.long1-3.wav 0.2749333        22.5        1   16 0.012416    6186
#> 4    Phae.long1.wav 2.5000444        22.5        1   16 0.112582   56251
#> 5  Phae.long2-1.wav 0.2325778        22.5        1   16 0.010510    5233
#> 6  Phae.long2-2.wav 0.2261333        22.5        1   16 0.010220    5088
#> 7    Phae.long2.wav 1.7000444        22.5        1   16 0.076582   38251
#> 8  Phae.long3-1.wav 0.2312444        22.5        1   16 0.010450    5203
#> 9  Phae.long3-2.wav 0.2301778        22.5        1   16 0.010402    5179
#> 10 Phae.long3-3.wav 0.2312000        22.5        1   16 0.010448    5202
#> 11   Phae.long3.wav 2.2000000        22.5        1   16 0.099080   49500
#> 12 Phae.long4-1.wav 0.2454222        22.5        1   16 0.011088    5522
#> 13 Phae.long4-2.wav 0.2441778        22.5        1   16 0.011032    5494
#> 14 Phae.long4-3.wav 0.2451111        22.5        1   16 0.011074    5515
#> 15   Phae.long4.wav 3.2000000        22.5        1   16 0.144080   72000
#> 16      no_anns.wav 2.5000444        22.5        1   16 0.112582   56251
```
