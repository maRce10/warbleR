# Class 'extended_selection_table': selection table containing wave objects

Class for selections of signals in sound files and corresponding wave
objects

## Usage

``` r
is_extended_selection_table(x)
```

## Arguments

- x:

  R object

## Value

A logical argument indicating whether the object class is
'extended_selection_table'

## Details

An object of class `extended_selection_table` created by
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
is a list with the following elements:

- `selections`: data frame containing the frequency/time coordinates of
  the selections, sound file names, and any additional information

- `check.resutls`: results of the checks on data consistency using
  [check_sels](https://marce10.github.io/warbleR/reference/check_sels.md)

- `wave.objects`: list of wave objects corresponding to each selection

- `by.song`: a list with 1) a logical argument defining if the
  'extended_selection_table' was created 'by song' and 2) the name of
  the song column (see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md))

## See also

[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md),
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
Check if object is of class "extended_selection_table"

`is_extended_selection_table` Check if the object belongs to the class
"extended_selection_table"

[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md);
[`is_selection_table`](https://marce10.github.io/warbleR/reference/is_selection_table.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

  is_extended_selection_table(lbh_selec_table)

  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  st <- selection_table(lbh_selec_table,
    extended = TRUE,
    path = tempdir()
  )

  is_extended_selection_table(st)

  class(st)
}
#> all selections are OK 
#> 
#> [1] "extended_selection_table" "data.frame"              
```
