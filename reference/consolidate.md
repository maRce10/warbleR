# Consolidate (sound) files into a single directory

`consolidate` copies (sound) files scattered in several directories into
a single one.

## Usage

``` r
consolidate(
  files = NULL,
  path = NULL,
  dest.path = NULL,
  pb = TRUE,
  file.ext = ".wav$",
  parallel = 1,
  save.csv = TRUE,
  ...
)
```

## Arguments

- files:

  character vector indicating the subset of files that will be
  consolidated. File names should include the full file path. Optional.

- path:

  Character string containing the directory path where the sound files
  are located. 'wav.path' set by
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)
  is ignored. If `NULL` (default) then the current working directory is
  used.

- dest.path:

  Character string containing the directory path where the sound files
  will be saved. If `NULL` (default) then the current working directory
  is used.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- file.ext:

  Character string defining the file extension (i.e. format) for the
  files to be consolidated. Default is `'.wav$'` ignoring case. Several
  formats can be used: `".wav$|.wac$|.mp3$|.flac$"`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- save.csv:

  Logical. Controls whether a data frame containing sound file
  information is saved in the new directory. Default is `TRUE`.

- ...:

  Additional arguments to be passed to the internal
  [`file.copy`](https://rdrr.io/r/base/files.html) function for
  customizing file copying.

## Value

All (sound) files are consolidated (copied) to a single directory
("consolidated_files"). The function returns a data frame with each of
the files that were copied in a row and the following information:

- `original_dir` the path to the original file

- `old_name` the name of the original file

- `new_name` the name of the new file. This will be the same as
  'old_name' if the name was not duplicated (i.e. no files in other
  directories with the same name).

- `file_size_bytes` size of the file in bytes.

- `duplicate` indicates whether a file is likely to be duplicated (i.e.
  if files with the same name were found in other directories). If so it
  will be labeled as 'possible.dupl', otherwise it will contain NAs.

If `csv = TRUE` (default) a 'file_names_info.csv' file with the same
information as the output data frame is also saved in the consolidated
directory.

## Details

This function allows users to put files scattered in several directories
into a single one. By default it works on sound files in '.wav' format
but can work with other type of files (for instance '.txt' selection
files).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md)
for making sound files readable in R

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

# create first folder with 2 sound files
dir.create(file.path(tempdir(), "folder1"))
writeWave(Phae.long1, file.path(tempdir(), "folder1", "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "folder1", "Phae.long2.wav"))

# create second folder with 2 sound files
dir.create(file.path(tempdir(), "folder2"))
writeWave(Phae.long3, file.path(tempdir(), "folder2", "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "folder2", "Phae.long4.wav"))

# consolidate in a single folder
# consolidate(path = tempdir(), dest.path = tempdir())

# check this folder
tempdir()
}
#> [1] "/tmp/Rtmp7ftaky"
```
