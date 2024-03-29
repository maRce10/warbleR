% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_detec.R
\name{auto_detec}
\alias{auto_detec}
\title{\code{auto_detec} automatically detects the start and end of vocalizations in sound files based
on amplitude, duration, and frequency range attributes.}
\usage{
auto_detec(X = NULL, wl = 512, threshold = 15, parallel = 1, power = 1,
   output = 'data.frame', thinning = 1, path = NULL, pb = TRUE, ssmooth = 0,
   bp = NULL, flist = NULL, hold.time = 0, mindur = NULL, maxdur = NULL, envt = NULL,
   msmooth = NULL, osci = NULL, xl = NULL, picsize = NULL, res = NULL, flim = NULL,
   ls = NULL, sxrow = NULL, rows = NULL,  redo = NULL, img = NULL, it = NULL,
   set = NULL, smadj = NULL, pal = NULL, fast.spec = NULL)
}
\arguments{
\item{X}{'selection_table' object or a data frame with columns
for sound file name (sound.files), selection number (selec), and start and end time of signal
(start and end). If provided the detection will be conducted only within
the selections in 'X'. Alternatively, an 'autodetec.output' object can be input. These objects are also generated by this function when \code{output = "list"}. If so the detection runs much faster as envelopes have been already calculated.}

\item{wl}{A numeric vector of length 1 specifying the window used internally by
\code{\link[seewave]{ffilter}} for bandpass filtering (so only applied when 'bp' is supplied). Default is 512.}

\item{threshold}{A numeric vector of length 1 specifying the amplitude threshold for detecting
signals (in \%).}

\item{parallel}{Numeric. Controls whether parallel computing is applied.
It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).}

\item{power}{A numeric vector of length 1 indicating a power factor applied to the amplitude envelope. Increasing power will reduce low amplitude modulations and increase high amplitude modulations, in order to reduce background noise. Default is 1 (no change).}

\item{output}{Character string indicating if the output should be a 'data.frame' with the detections (default) or a list (of class 'autodetec.output') containing both 1) the detections and 2) the amplitude envelopes (time vs amplitude) for each sound file. The list can be input into \code{\link{full_spectrograms}} to explore detections and associated amplitude envelopes.}

\item{thinning}{Numeric vector of length 1 in the range 0~1 indicating the proportional reduction of the number of
samples used to represent amplitude envelopes (i.e. the thinning of the envelopes). Usually amplitude envelopes have many more samples
than those needed to accurately represent amplitude variation in time, which affects the size of the
output (usually very large R objects / files). Default is  \code{1} (no thinning). Higher sampling rates can afford higher size reduction (e.g. lower thinning values). Reduction is conducted by interpolation using \code{\link[stats]{approx}}. Note that thinning may decrease time precision, and the higher the thinning the less precise the time detection.}

\item{path}{Character string containing the directory path where the sound files are located.
If \code{NULL} (default) then the current working directory is used.}

\item{pb}{Logical argument to control progress bar. Default is \code{TRUE}.}

\item{ssmooth}{A numeric vector of length 1 to smooth the amplitude envelope
with a sum smooth function. Default is 0. Note that smoothing is applied before thinning (see 'thinning' argument).}

\item{bp}{Numeric vector of length 2 giving the lower and upper limits of a
frequency bandpass filter (in kHz). Default is \code{NULL}.}

\item{flist}{character vector or factor indicating the subset of files that will be analyzed. Ignored
if X is provided.}

\item{hold.time}{Numeric vector of length 1. Specifies the time range at which selections will be merged (i.e. if 2 selections are separated by less than the specified hold.time they will be merged in to a single selection). Default is  \code{0}.}

\item{mindur}{Numeric vector of length 1 giving the shortest duration (in
seconds) of the signals to be detected. It removes signals below that
threshold.}

\item{maxdur}{Numeric vector of length 1 giving the longest duration (in
seconds) of the signals to be detected. It removes signals above that
threshold.}

\item{envt}{DEPRECATED.}

\item{msmooth}{DEPRECATED.}

\item{osci}{DEPRECATED.}

\item{xl}{DEPRECATED}

\item{picsize}{DEPRECATED}

\item{res}{DEPRECATED}

\item{flim}{DEPRECATED}

\item{ls}{DEPRECATED}

\item{sxrow}{DEPRECATED}

\item{rows}{DEPRECATED}

\item{redo}{DEPRECATED.}

\item{img}{DEPRECATED.}

\item{it}{DEPRECATED.}

\item{set}{DEPRECATED.}

\item{smadj}{DEPRECATED.}

\item{pal}{DEPRECATED.}

\item{fast.spec}{DEPRECATED.}
}
\value{
A data frame containing the start and end of each signal by
  sound file and selection number. If 'output = "list"' then a list including 1) a detection data frame, 2) amplitude envelopes and 3) parameters will be return. An additional column 'org.selec' is added when 'X' is provided (so detection can be traced back to the selections in 'X').
}
\description{
\code{auto_detec} automatically detects the start and end of vocalizations in sound files based
on amplitude, duration, and frequency range attributes.
}
\details{
This function determines the start and end of signals in the sound file selections listed
  in the input data frame ('X'). Alternatively, if no data frame is provided, the function detects signals across
  each entire sound file. It can also create long spectrograms highlighting the start and of the detected
  signals for all sound files in the working directory (if \code{img = TRUE}). Sound files should be located in the
   working directory or the path to the sound files should be provided using the 'path' argument. The input
   data frame should have the following columns: c("sound.files","selec","start","end"). This function uses a modified version of the \code{\link[seewave]{timer}} function from seewave package to detect signals. Note that warbleR function for signal detection will be deprecated in future warbleR versions. Look at the ohun package for automatic signal detection functions.
}
\examples{
{
  # Save to temporary working directory
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  ad <- auto_detec(
    threshold = 5, ssmooth = 300,
    bp = c(2, 9), wl = 300, path = tempdir()
  )
}

}
\references{
{
Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
}
}
\seealso{
\code{\link{cross_correlation}}
}
\author{
Marcelo Araya-Salas (\email{marcelo.araya@ucr.ac.cr}). Implements a
modified version of the timer function from seewave.
}
