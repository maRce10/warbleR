# Remove channels in wave files

`remove_channels` remove channels in wave files

## Usage

``` r
remove_channels(files = NULL, channels, path = NULL, parallel = 1, pb = TRUE)
```

## Arguments

- files:

  Character vector indicating the files that will be analyzed. If not
  provided. Optional. then all wave files in the working directory (or
  path) will be processed.

- channels:

  Numeric vector indicating the index (or channel number) for the
  channels that will be kept (left = 1, right = 2; 3 to inf for
  multichannel sound files).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

## Value

Sound files that have been converted are saved in the new folder
"converted_sound_files". If \`img = TRUE\` then spectrogram images
highlighting the silence segments that were removed are also saved.

## Details

The function removes channels from wave files. It works on regular and
multichannel wave files. Converted files are saved in a new directory
("converted_sound_files") and original files are not modified.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md),
[`info_sound_files`](https://marce10.github.io/warbleR/reference/info_sound_files.md),

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# save sound file examples
data("Phae.long1")
Phae.long1.2 <- stereo(Phae.long1, Phae.long1)

writeWave(Phae.long1.2, file.path(tempdir(), "Phae.long1.2.wav"))

remove_channels(channels = 1, path = tempdir())

#check this floder
tempdir()
}
#> [1] "/tmp/Rtmp7ftaky"
```
