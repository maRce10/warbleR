% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq_ts.R
\name{freq_ts}
\alias{freq_ts}
\title{Extract frequency contours as time series}
\usage{
freq_ts(
  X,
  type = "dominant",
  wl = 512,
  length.out = 20,
  wn = "hanning",
  ovlp = 70,
  bp = NULL,
  threshold = 15,
  img = TRUE,
  parallel = 1,
  path = NULL,
  img.suffix = "frequency.ts",
  pb = TRUE,
  clip.edges = FALSE,
  leglab = "frequency.ts",
  track.harm = FALSE,
  raw.contour = FALSE,
  adjust.wl = TRUE,
  ff.method = "seewave",
  entropy.range = c(2, 10),
  ...
)
}
\arguments{
\item{X}{object of class 'selection_table', 'extended_selection_table' or data
frame containing columns for sound file name (sound.files),
selection number (selec), and start and end time of signal (start and end).}

\item{type}{Character string to determine the type of contour to be detected. Three options are available, "dominant" (default), "fundamental" and "entropy".}

\item{wl}{A numeric vector of length 1 specifying the window length of the spectrogram, default
is 512.}

\item{length.out}{A numeric vector of length 1 giving the number of measurements of fundamental
frequency desired (the length of the time series).}

\item{wn}{Character vector of length 1 specifying window name. Default is
"hanning". See function \code{\link[seewave]{ftwindow}} for more options.}

\item{ovlp}{Numeric vector of length 1 specifying \% of overlap between two
consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 70.}

\item{bp}{A numeric vector of length 2 for the lower and upper limits of a
frequency bandpass filter (in kHz). Default is \code{NULL}.}

\item{threshold}{amplitude threshold (\%) for fundamental frequency detection. Default is 15.}

\item{img}{Logical argument. If \code{FALSE}, image files are not produced. Default is \code{TRUE}.}

\item{parallel}{Numeric. Controls whether parallel computing is applied.
It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).}

\item{path}{Character string containing the directory path where the sound files are located.
If \code{NULL} (default) then the current working directory is used.}

\item{img.suffix}{A character vector of length 1 with a suffix (label) to add at the end of the names of
image files.}

\item{pb}{Logical argument to control progress bar. Default is \code{TRUE}.}

\item{clip.edges}{Logical argument to control whether edges (start or end of signal) in
which amplitude values above the threshold were not detected will be removed. If
\code{TRUE} this edges will be excluded and signal contour will be calculated on the
remaining values. Default is \code{FALSE}.
#' @param leglab A character vector of length 1 or 2 containing the label(s) of the frequency contour legend
in the output image.}

\item{leglab}{A character vector of length 1 or 2 containing the label(s) of the frequency contour legend
in the output image.}

\item{track.harm}{Logical. If \code{TRUE} warbleR's \code{\link{track_harmonic}} function is
used to track dominant frequency contours. Otherwise seewave's \code{\link[seewave]{dfreq}} is used by default. Default is \code{FALSE}.}

\item{raw.contour}{Logical. If \code{TRUE} then a list with the original contours
(i.e. without interpolating values to make all contours of equal length) is returned (and no images are produced).}

\item{adjust.wl}{Logical. If \code{TRUE} 'wl' (window length) is reset to be lower than the
number of samples in a selection if the number of samples is less than 'wl'. Default is \code{TRUE}. Used only for dominant frequency detection.}

\item{ff.method}{Character. Selects the method used to detect fundamental
frequency contours. Either 'tuneR' (using \code{\link[tuneR]{FF}}) or 'seewave' (using
\code{\link[seewave]{fund}}). Default is 'seewave'. 'tuneR' performs
faster (and seems to be more accurate) than 'seewave'.}

\item{entropy.range}{Numeric vector of length 2. Range of frequency in which to display the entropy values
on the spectrogram (when img = TRUE). Default is c(2, 10). Negative values can be used in order to stretch more
the range.}

\item{...}{Additional arguments to be passed to \code{\link{track_freq_contour}}. for customizing
graphical output.}
}
\value{
A data frame with the fundamental frequency values measured across the signals. If img is
\code{TRUE} it also produces image files with the spectrograms of the signals listed in the
input data frame showing the location of the fundamental frequencies
(see \code{\link{track_freq_contour}} description for more details).
}
\description{
\code{freq_ts} extracts the fundamental frequency values as a time series.
}
\details{
This function extracts the dominant frequency, fundamental frequency or spectral entropy contours as time series.
The function uses the \code{\link[stats:approxfun]{approx}} function to interpolate values between frequency measures. If there are no frequencies above the amplitude threshold (for dominant and fundamental) at the beginning or end
 of the signals then NAs will be generated. On the other hand, if there are no frequencies
 above the amplitude threshold in between signal segments in which amplitude was
 detected then the values of this adjacent segments will be interpolated
 to fill out the missing values (e.g. no NAs in between detected amplitude segments).
}
\examples{
{
#load data
data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) #save sound files
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) #save sound files

# run function with dominant frequency
freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
wl = 300, pb = FALSE, path = tempdir())

# note a NA in the row 4 column 3 (dfreq-1)
# this can be removed by clipping edges (removing NAs at the start and/or end
# when no freq was detected)

freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
wl = 300, pb = FALSE, clip.edges = TRUE, path = tempdir())

# run function with fundamental frequency
freq_ts(lbh_selec_table, type = "fundamental", length.out = 50,
flim = c(1, 12),  bp = c(2, 9), wl = 300, path = tempdir())

# run function with spectral entropy
# without clip edges
freq_ts(X = lbh_selec_table, type = "entropy", threshold = 10,
clip.edges = FALSE, length.out = 10, sp.en.range = c(-25, 10), path = tempdir(),
 img = FALSE)

# with clip edges and length.out 10
freq_ts(X = lbh_selec_table, type = "entropy", threshold = 10, bp = c(2, 12),
clip.edges = TRUE, length.out = 10, path = tempdir(), img = FALSE)
}
}
\seealso{
\code{\link{sig2noise}}, \code{\link{track_freq_contour}}, \code{\link{freq_ts}}, \code{\link{freq_DTW}}
}
\author{
Marcelo Araya-Salas (\email{marcelo.araya@ucr.ac.cr})
}
