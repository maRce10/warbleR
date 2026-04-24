# Sort columns in a more intuitive order

`sort_colms` sorts selection table columns in a more intuitive order.

## Usage

``` r
sort_colms(X)
```

## Arguments

- X:

  Data frame containing columns for sound file (sound.files), selection
  (selec), start and end time of signals ('start' and 'end') and low and
  high frequency ('bottom.freq' and 'top.freq', optional). See the
  example data 'lbh_selec_table'.

## Value

The same data as in the input data frame but with the most relevant
information for acoustic analysis located in the first columns.

## Details

The function returns the data from the input data frame with the most
relevant information for acoustic analysis located in the first columns.
The priority order for column names is: "sound.files", "channel",
"selec", "start", "end", "top.freq", and "bottom.freq".

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
library(warbleR)
data("lbh_selec_table")

# mess column order
lbh_selec_table <- lbh_selec_table[, sample(seq_len(ncol(lbh_selec_table)))]

# check names
names(lbh_selec_table)
#> [1] "channel"     "end"         "sound.files" "bottom.freq" "top.freq"   
#> [6] "start"       "selec"      

lbh_selec_table <- sort_colms(X = lbh_selec_table)

# check names again
names(lbh_selec_table)
#> [1] "sound.files" "channel"     "selec"       "start"       "end"        
#> [6] "top.freq"    "bottom.freq"
```
