# Rename wave objects and associated metadata in extended selection tables

`rename_est_waves` rename wave objects and associated metadata in
extended selection tables

## Usage

``` r
rename_est_waves(X, new.sound.files, new.selec = NULL)
```

## Arguments

- X:

  object of class 'extended_selection_table'.

- new.sound.files:

  Character vector of length equals to the number of wave objects in the
  extended selection table (`length(attr(X, "wave.objects"))`).Specifies
  the new names to be used for wave objects and sound file column. Note
  that this will rename wave objects and associated attributes and data
  in 'X'.

- new.selec:

  Numeric or character vector of length equals to the number of rows in
  'X' to specify the 'selec' column labels. Default is `NULL`. If not
  provided the 'selec' column is kept unchanged. Note that the
  combination of 'sound.files' and 'selec' columns must produce unique
  IDs for each selection (row).

## Value

An extended selection table with rename sound files names in data frame
and attributes. The function adds columns with the previous sound file
names (and 'selec' if provided).

## Details

This function allow users to change the names of 'sound.files' and
'selec' columns in extended selection tables. These names can become
very long after manipulations used to produce extended tables.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

Other extended selection table manipulation:
[`by_element_est()`](https://marce10.github.io/warbleR/reference/by_element_est.md),
[`resample_est()`](https://marce10.github.io/warbleR/reference/resample_est.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
data("lbh.est")

# order by sound file name
lbh.est <- lbh.est[order(lbh.est$sound.files),]

# create new sound file name
nsf <- sapply(strsplit(lbh.est$sound.files, ".wav",fixed = TRUE), "[",1)

slc <- vector(length = nrow(lbh.est))
slc[1] <- 1

for(i in 2:length(slc))
if (nsf[i - 1] == nsf[i]) slc[i] <- slc[i - 1] + 1 else
slc[i] <- 1

nsf <- paste(nsf, slc, sep = "_")

# rename sound files
Y <- rename_est_waves(X = lbh.est, new.sound.files = nsf)
}
```
