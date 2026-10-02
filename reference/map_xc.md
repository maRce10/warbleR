# Maps of 'Xeno-Canto' recordings by species (deprecated)

`map_xc` has been deprecated. Use `suwo::map_locations()` from the
package [suwo](https://docs.ropensci.org/suwo/) instead, which maps
media records from 'Xeno-Canto' and other online repositories.

## Usage

``` r
map_xc(...)
```

## Arguments

- ...:

  Ignored. Kept so that existing code calling `map_xc()` gets an
  informative warning instead of an error.

## Value

`NULL` (invisibly).

## Details

This function has been deprecated as access to online nature media
repositories (including 'Xeno-Canto') is now provided by the package
[suwo](https://docs.ropensci.org/suwo/). Metadata obtained with
`suwo::query_xenocanto()` (or any other suwo query function) can be
mapped with `suwo::map_locations()`.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`query_xc`](https://marce10.github.io/warbleR/reference/query_xc.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre
