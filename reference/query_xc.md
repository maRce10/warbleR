# Access 'Xeno-Canto' recordings and metadata (deprecated)

`query_xc` has been deprecated. Use the package
[suwo](https://docs.ropensci.org/suwo/) instead:
`suwo::query_xenocanto()` retrieves 'Xeno-Canto' metadata and
`suwo::download_media()` downloads the recordings.

## Usage

``` r
query_xc(...)
```

## Arguments

- ...:

  Ignored. Kept so that existing code calling `query_xc()` gets an
  informative warning instead of an error.

## Value

`NULL` (invisibly).

## Details

This function has been deprecated as access to 'Xeno-Canto' (and other
online nature media repositories such as Macaulay Library, iNaturalist,
GBIF and WikiAves) is now provided by the package
[suwo](https://docs.ropensci.org/suwo/). Note that 'Xeno-Canto' queries
now require an API key (see `?suwo::query_xenocanto`). For example:


    # install.packages("suwo")
    library(suwo)

    # search for metadata
    p_anth <- query_xenocanto(species = "Phaethornis anthophilus")

    # download recordings
    download_media(metadata = p_anth, path = tempdir())

## See also

[`map_xc`](https://marce10.github.io/warbleR/reference/map_xc.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)
