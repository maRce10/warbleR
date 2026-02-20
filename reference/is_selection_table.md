# Class 'selection_table': double-checked frequency/time coordinates of selections

Class for selections of signals in sound files

## Usage

``` r
is_selection_table(x)
```

## Arguments

- x:

  R object.

## Value

A logical argument indicating whether the object class is
'selection_table'

## Details

An object of class `selection_table` created by
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
is a list with the following elements:

- `selections`: data frame containing the frequency/time coordinates of
  the selections, sound file names, and any additional information

- `check.resutls`: results of the checks on data consistency using
  [check_sels](https://marce10.github.io/warbleR/reference/check_sels.md)

## See also

[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
Check if object is of class "selection_table"

`is_selection_table` Check if the object belongs to the class
"selection_table"

[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

  is_selection_table(lbh_selec_table)

  # save wave files in temporary directory
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  st <- selection_table(lbh_selec_table, path = tempdir())

  is_selection_table(st)

  class(st)
}
#> all selections are OK 
#> 
#> [1] "selection_table" "data.frame"     
```
