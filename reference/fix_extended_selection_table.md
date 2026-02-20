# Fix extended selection tables

`fix_extended_selection_table` fixes extended selection tables that have
lost their attributes

## Usage

``` r
fix_extended_selection_table(X, Y, to.by.song = FALSE)
```

## Arguments

- X:

  an object of class 'selection_table' or data frame that contains
  columns for sound file name (sound.files), selection number (selec),
  and start and end time of signal (start and end).

- Y:

  an object of class 'extended_selection_table'

- to.by.song:

  Logical argument to control if the attributes are formatted to a match
  a 'by.song' extended selection table. This is required when 'X' is
  created by collapsing an Y by song (see 'by.song' argument in
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)).
  Mostly needed internally by some warbleR functions.

## Value

An extended selection table.

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

# create extended selection table
ext_st <- selection_table(lbh_selec_table, extended = TRUE,
path = tempdir())

# remove attributes
st <- as.data.frame(ext_st)

# check class
class(st)

# fix selection table
st <- fix_extended_selection_table(X = st, Y = ext_st)

# check class
class(st)
}
#> all selections are OK 
#> 
#> [1] "extended_selection_table" "data.frame"              
```
