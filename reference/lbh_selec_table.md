# Example data frame of selections (i.e. selection table).

A data frame containing the start, end, low and high frequency of
*Phaethornis longirostris* (Long-billed Hermit) songs from the example
sound files included in this package.

## Usage

``` r
data(lbh_selec_table)
```

## Format

A data frame with 11 rows and 7 columns:

- sound.files:

  sound file names

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

Marcelo Araya-Salas, warbleR
