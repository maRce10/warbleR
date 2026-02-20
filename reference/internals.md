# `warbleR` Internal Functions

These functions are used by other functions in the warbleR package, but
are not intended to be called by users.

These 2 functions are modified versions of
[`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). The functions
are: `spectro.rectw.INTFUN.2` `spectro.rectw.INTFUN`

These 2 functions are modified versions of seewave's
[`filled.contour.modif2()`](https://rdrr.io/pkg/seewave/man/seewave.internal.html).
[`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). The functions
are: `filled.contour.color.INTFUN` `filled.contour.INTFUN`

`boxw.INTFUN` to create boxes around spectrograms in
[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md)

`rectw` is a modified version of
[`rect`](https://rdrr.io/r/graphics/rect.html) to add density
(cross-hatching lines)

These 2 functions are used for frequency range detection: `frd.INTFUN`
`frd.plot.INTFUN`

## Note

These functions are not to be called by the user.
