# warbleR: A package to streamline bioacoustic analysis

warbleR is intended to facilitate the analysis of the structure of
animal acoustic signals in R. Users can collect open-access avian
recordings or enter their own data into a workflow that facilitates
spectrographic visualization and measurement of acoustic parameters.
warbleR makes use of the fundamental sound analysis tools of the seewave
package, and offers new tools for acoustic structure analysis. These
tools are available for batch analysis of acoustic signals.

## Details

The main features of the package are:

- The use of loops to apply tasks through acoustic signals referenced in
  a selection table

- The production of images in the working folder with spectrograms that
  allow to organize data and verify acoustic analyzes

The package offers functions to:

- Explore and download Xeno Canto recordings

- Explore, organize and manipulate multiple sound files

- Detect signals automatically (in frequency and time)

- Create spectrograms of complete recordings or individual signals

- Run different measures of acoustic signal structure

- Evaluate the performance of measurement methods

- Catalog signals

- Characterize different structural levels in acoustic signals

- Statistical analysis of duet coordination

- Consolidate databases and annotation tables

Most of the functions allow the parallelization of tasks, which
distributes the tasks among several processors to improve computational
efficiency. Tools to evaluate the performance of the analysis at each
step are also available. In addition, warbleR satisfies the need for
rigorous open source bioacoustic analysis, which facilitates
opportunities for use in research and innovation of additional custom
analyzes.

The warbleR package offers three overarching categories of functions:

License: GPL (\>= 2)

## Obtaining animal vocalization data

[`simulate_songs`](https://marce10.github.io/warbleR/reference/simulate_songs.md):
Simulate animal vocalizations

## Managing sound files

[`read_sound_file`](https://marce10.github.io/warbleR/reference/read_sound_file.md):
Read sound files into 'wave' objects

[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md):
Create 'selection_table' class objects

[`mp32wav`](https://marce10.github.io/warbleR/reference/mp32wav.md):
Convert several .mp3 files in working directory to .wav format

[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md):
Check whether selections can be read by subsequent functions

[`check_sound_files`](https://marce10.github.io/warbleR/reference/check_sound_files.md):
Check whether .wav files can be read by subsequent functions and the
minimum windows length ("wl" argument) that can be used

[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md):
Fix .wav files so they can be read by other functions

[`split_sound_files`](https://marce10.github.io/warbleR/reference/split_sound_files.md):
Split sound fies in several sound files

[`resample_est`](https://marce10.github.io/warbleR/reference/resample_est.md):
Resample wave objects in extended selection tables

[`duration_sound_files`](https://marce10.github.io/warbleR/reference/duration_sound_files.md):
Determine the duration of sound files

[`cut_sels`](https://marce10.github.io/warbleR/reference/cut_sels.md):
Cut selections from a selection table into individual sound files

[`remove_silence`](https://marce10.github.io/warbleR/reference/remove_silence.md):
Remove silence segments from wave files

[`remove_channels`](https://marce10.github.io/warbleR/reference/remove_channels.md):
Remove channels in wave files

[`consolidate`](https://marce10.github.io/warbleR/reference/consolidate.md):
Consolidate sound files into a single folder

[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md):
Create double-checked and self-contained selection tables

[`fix_extended_selection_table`](https://marce10.github.io/warbleR/reference/fix_extended_selection_table.md):
Fix attributes of extended selection tables

## Exploring/analyzing signal structure

[`tailor_sels`](https://marce10.github.io/warbleR/reference/tailor_sels.md):
Interactive view of spectrograms to tailor start and end of selections

[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md):
Measure signal-to-noise ratio across multiple files

[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md):
Create spectrograms to visualize frequency measurements

[`filter_sels`](https://marce10.github.io/warbleR/reference/filter_sels.md):
Filter selection data frames based on filtered image files

[`freq_range`](https://marce10.github.io/warbleR/reference/freq_range.md):
Detect frequency range iteratively from signals in a selection table

[`freq_range_detec`](https://marce10.github.io/warbleR/reference/freq_range_detec.md):
Detect frequency range in a Wave object

[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md):
Measure acoustic parameters on selected acoustic signals

[`mfcc_stats`](https://marce10.github.io/warbleR/reference/mfcc_stats.md):
Calculate descriptive statistics on Mel-frequency cepstral coefficients

[`song_analysis`](https://marce10.github.io/warbleR/reference/song_analysis.md):
Measure acoustic parameters at higher levels of organization

[`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md):
Pairwise cross-correlation of multiple signals

[`gaps`](https://marce10.github.io/warbleR/reference/gaps.md): Measures
gap duration

[`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md):
Extract frequency contours the signal as a time series

[`freq_DTW`](https://marce10.github.io/warbleR/reference/freq_DTW.md):
Calculate acoustic dissimilarity using dynamic time warping on frequency
contours

[`wpd_features`](https://marce10.github.io/warbleR/reference/wpd_features.md):
Measure wavelet packet decomposition features

[`compare_methods`](https://marce10.github.io/warbleR/reference/compare_methods.md):
Produce graphs to visually assess performance of acoustic distance
measurements

[`test_coordination`](https://marce10.github.io/warbleR/reference/test_coordination.md):
Assess statistical significance of singing coordination

[`overlapping_sels`](https://marce10.github.io/warbleR/reference/overlapping_sels.md):
Find selections that overlap in time within a given sound file

[`track_harmonic`](https://marce10.github.io/warbleR/reference/track_harmonic.md):
Track harmonic frequency contour

## Graphical outputs

[`map_xc`](https://marce10.github.io/warbleR/reference/map_xc.md):
Create maps to visualize the geographic spread of 'Xeno-Canto'
recordings

[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md):
Produce a vocalization catalog with spectrograms in and array with
several rows and columns

[`catalog2pdf`](https://marce10.github.io/warbleR/reference/catalog2pdf.md):
Combine catalog images to single pdf files

[`plot_coordination`](https://marce10.github.io/warbleR/reference/plot_coordination.md):
Create graphs of coordinated singing

[`color_spectro`](https://marce10.github.io/warbleR/reference/color_spectro.md):
Highlight spectrogram regions

[`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md):
Produce spectrograms of whole recordings split into multiple rows

[`full_spectrogram2pdf`](https://marce10.github.io/warbleR/reference/full_spectrogram2pdf.md):
Combine
[`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md)
images to single pdf files

[`spectrograms`](https://marce10.github.io/warbleR/reference/spectrograms.md):
Create spectrograms of selections

[`snr_spectrograms`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md):
Create spectrograms to visualize margins over which noise will be
measured by sig2noise

[`phylo_spectro`](https://marce10.github.io/warbleR/reference/phylo_spectro.md):
Add spectrograms onto phylogenetic trees

[`tweak_spectro`](https://marce10.github.io/warbleR/reference/tweak_spectro.md):
Visually inspect effect of different settings for creating (and
improving) spectrograms

## See also

Useful links:

- <https://marce10.github.io/warbleR/>

- Report bugs at <https://github.com/maRce10/warbleR/issues/>

## Author

Marcelo Araya-Salas & Grace Smith Vidaurre

Maintainer: Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)
