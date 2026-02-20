# Open working directory

`open_wd` opens the working directory in the default file browser.

## Usage

``` r
open_wd(path = getwd(), verbose = TRUE)
```

## Arguments

- path:

  Directory path to be opened. By default it's the working directory.
  'wav.path' set by
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)
  is ignored in this case.

- verbose:

  Logical to control whether the 'path' is printed in the console.
  Defaut is `TRUE`.

## Value

Opens the working directory using the default file browser.

## Details

The function opens the working directory using the default file browser
and prints the working directory in the R console. This function aims to
simplify the manipulation of sound files and other files produced by
many of the [warbleR](https://cran.r-project.org/package=warbleR)
function.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`move_images`](https://marce10.github.io/warbleR/reference/move_images.md)

Other data manipulation:
[`move_images()`](https://marce10.github.io/warbleR/reference/move_images.md),
[`split_sound_files()`](https://marce10.github.io/warbleR/reference/split_sound_files.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
open_wd()
}
#> [1] "/home/runner/work/warbleR/warbleR/docs/reference opened in file browser"
```
