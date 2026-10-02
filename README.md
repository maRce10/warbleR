warbleR: Streamline Bioacoustic Analysis
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/warbleR)](https://cran.r-project.org/package=warbleR)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/warbleR)](https://cranlogs.r-pkg.org/badges/grand-total/warbleR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/maRce10/warbleR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/maRce10/warbleR?branch=master)
[![Dependencies](https://tinyverse.netlify.app/badge/warbleR)](https://CRAN.R-project.org/package=warbleR)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Paper
DOI](https://img.shields.io/badge/DOI-10.1111%2F2041--210X.12624-1f6feb.svg)](https://doi.org/10.1111/2041-210X.12624)
<!-- badges: end -->

<img src="man/figures/warbleR_sticker.png" alt="warbleR logo" align="right" width="25%"/>

**warbleR** is an R package for analyzing the structure of animal
acoustic signals at scale. Bring your own recordings (or open-access
ones from repositories like [Xeno-canto](https://xeno-canto.org/),
easily obtained with [suwo](https://docs.ropensci.org/suwo/)), annotate
them in a *selection table*, and run the whole pipeline — from file
wrangling and spectrograms to acoustic measurements and similarity
analyses — in batch, on as many signals as you have.

Built on top of [seewave](https://cran.r-project.org/package=seewave)
and [tuneR](https://cran.r-project.org/package=tuneR), warbleR adds the
workflow layer those packages leave to the user:

- 🗂️ **Selection-table-driven workflows** — every function loops over
  the signals listed in an annotation table, so one call handles one
  sound or ten thousand
- 📦 **Extended selection tables** — a single R object that bundles
  annotations *and* the audio clips, making analyses portable and easy
  to share
- ⚡ **Parallel processing** — most functions take a `parallel` argument
  to spread the work across cores
- 🔍 **Built-in quality checks** — spectrogram images and diagnostic
  tools let you verify each step before moving on

## What can you do with it?

| Task | Key functions |
|:---|:---|
| Inspect, convert and fix sound files | `info_sound_files()`, `check_sound_files()`, `fix_wavs()`, `mp32wav()`, `wav_2_flac()`, `split_sound_files()`, `remove_channels()` |
| Build and validate annotation tables | `selection_table()`, `check_sels()`, `tailor_sels()`, `cut_sels()`, `overlapping_sels()`, `consolidate()` |
| Create spectrograms | `spectrograms()`, `full_spectrograms()`, `color_spectro()`, `snr_spectrograms()`, `catalog()`, `phylo_spectro()` |
| Measure acoustic structure | `spectro_analysis()`, `mfcc_stats()`, `song_analysis()`, `freq_range()`, `sig2noise()`, `sound_pressure_level()`, `gaps()`, `wpd_features()` |
| Track frequency contours | `freq_ts()`, `track_freq_contour()`, `track_harmonic()`, `inflections()` |
| Compare signals | `cross_correlation()`, `freq_DTW()`, `multi_DTW()`, `waveform_similarity()`, `compare_methods()` |
| Analyze duet / chorus coordination | `test_coordination()`, `plot_coordination()` |
| Simulate signals | `simulate_songs()` |

See the [function
reference](https://marce10.github.io/warbleR/reference/) for the full
list.

## Installation

From CRAN:

``` r
install.packages("warbleR")
```

Development version from GitHub (requires
[remotes](https://cran.r-project.org/package=remotes)):

``` r
remotes::install_github("maRce10/warbleR")
```

## Quick example

The example recordings and annotations come from the
[NatureSounds](https://cran.r-project.org/package=NatureSounds) package
(installed with warbleR):

``` r
library(warbleR)

# load example long-billed hermit songs and their annotations
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

# save the sound files to a temporary folder
for (i in paste0("Phae.long", 1:4)) {
  tuneR::writeWave(get(i), file.path(tempdir(), paste0(i, ".wav")))
}

# check that annotations and sound files match
check_sels(lbh_selec_table, path = tempdir())

# measure spectral and temporal parameters for every annotated signal
params <- spectro_analysis(lbh_selec_table, path = tempdir())

# pairwise acoustic similarity via spectrographic cross-correlation
xc <- cross_correlation(lbh_selec_table, path = tempdir())
```

## Learn more

- 📘 [Intro to
  warbleR](https://marce10.github.io/warbleR/articles/a_warbleR.html) —
  an overview of the package
- 📝 [Annotation data
  format](https://marce10.github.io/warbleR/articles/b_annotation_data_format.html)
  — how input annotations (selection tables) should look
- 🔁 [All vignettes](https://marce10.github.io/warbleR/articles/) —
  worked examples of complete analysis workflows
- 📄 [Original paper](https://doi.org/10.1111/2041-210X.12624) in
  *Methods in Ecology and Evolution* (the package has grown a lot since,
  so check the vignettes for current usage)

## Related packages

| Package | What it does |
|:---|:---|
| [seewave](https://cran.r-project.org/package=seewave) & [tuneR](https://cran.r-project.org/package=tuneR) | Core sound analysis and manipulation of wave objects in R |
| [ohun](https://docs.ropensci.org/ohun/) | Automated detection of sound events, with tools to diagnose and optimize detection routines |
| [suwo](https://docs.ropensci.org/suwo/) | Search, download and map nature media (Xeno-canto, Macaulay Library, iNaturalist, GBIF, WikiAves) — replaces warbleR’s `query_xc()` and `map_xc()` |
| [baRulho](https://docs.ropensci.org/baRulho/) | Quantifying habitat-induced degradation of acoustic signals, with inputs/outputs compatible with warbleR |
| [Rraven](https://cran.r-project.org/package=Rraven) | Data exchange between R and [Raven](https://www.ravensoundsoftware.com/) (Cornell Lab of Ornithology), handy for using Raven as the annotation tool |
| [dynaSpec](https://cran.r-project.org/package=dynaSpec) | Dynamic spectrograms (spectrogram videos) |
| [NatureSounds](https://cran.r-project.org/package=NatureSounds) | Example recordings and annotations of animal sounds |

## Getting help & contributing

Found a bug or have a feature request? Please open an
[issue](https://github.com/maRce10/warbleR/issues). Contributions are
welcome — see the [contributing
guidelines](https://github.com/maRce10/warbleR/blob/master/CONTRIBUTING.md).

## Citation

If you use warbleR, please cite:

> Araya-Salas, M. & Smith-Vidaurre, G. (2017). warbleR: an R package to
> streamline analysis of animal acoustic signals. *Methods in Ecology
> and Evolution*, 8, 184–191. <https://doi.org/10.1111/2041-210X.12624>

Please also cite [tuneR](https://cran.r-project.org/package=tuneR) and
[seewave](https://cran.r-project.org/package=seewave) if you use any
function that creates spectrograms or measures acoustic parameters. You
can get all citations from R with `citation("warbleR")`.
