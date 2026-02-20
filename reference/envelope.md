# Calculates the absolute amplitude envelope

Calculates the absolute amplitude envelope

## Arguments

- x:

  Numeric vector with amplitude values. Required.

- ssmooth:

  Numeric vector of length 1 indicating the size of the sliding window
  use to smooth envelopes. Default is 0 (no smoothing).

## Value

An amplitude envelope.

## Details

The function calculates the absolute amplitude envelope of an amplitude
vector using compiled C code which is usually several times faster.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`env`](https://rdrr.io/pkg/seewave/man/env.html).

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) & Paula Monge

## Examples

``` r
{
data(tico)

amp_env <- envelope(tico@left, ssmooth = 100)
}
```
