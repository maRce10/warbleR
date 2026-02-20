# Example matrix listing selections to be compared by [`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md)

`comp_matrix` is a character matrix with 2 columns indicating the
selections to be compared (column 1 vs column 2) by
[`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md).

## Usage

``` r
data(comp_matrix)
```

## Format

A data frame with 11 rows and 6 variables:

- sound.files:

  recording names

- channel:

  channel in which signal is found

- selec:

  selection numbers within recording

- start:

  start times of selected signal

- end:

  end times of selected signal

- bottom.freq:

  lower limit of frequency range

- top.freq:

  upper limit of frequency range

## Source

Marcelo Araya Salas, warbleR

## Details

A character matrix with 2 columns indicating the selections to be
compared (column 1 vs column 2) by
[`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md).
The first column contain the ID of the selection, which is given by
combining the 'sound.files' and 'selec' columns of 'X', separated by '-'
(i.e. `paste(X$sound.files, X$selec, sep = "-")`). The selection id's
refer to those on the example data "lbh_selec_table". The second column
refers to the sound files in which to search for the templates.
