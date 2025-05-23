% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/full_spectrogram2pdf.R
\name{full_spectrogram2pdf}
\alias{full_spectrogram2pdf}
\title{\code{full_spectrogram2pdf} combines \code{\link{full_spectrograms}} images in .jpeg format to a single pdf file.}
\usage{
full_spectrogram2pdf(
  keep.img = TRUE,
  overwrite = FALSE,
  parallel = 1,
  path = NULL,
  pb = TRUE
)
}
\arguments{
\item{keep.img}{Logical argument. Indicates whether jpeg files should be kept (default) or remove.
(including sound file and page number) should be magnified. Default is 1.}

\item{overwrite}{Logical argument. If \code{TRUE} all jpeg pdf will be produced again
when code is rerun. If \code{FALSE} only the ones missing will be produced. Default is \code{FALSE}.}

\item{parallel}{Numeric. Controls whether parallel computing is applied.
It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).}

\item{path}{Character string containing the directory path where the sound files are located.
If \code{NULL} (default) then the current working directory is used.}

\item{pb}{Logical argument to control progress bar. Default is \code{TRUE}.}
}
\value{
Image files in pdf format with spectrograms of entire sound files in the working directory.
}
\description{
\code{full_spectrogram2pdf} combines \code{\link{full_spectrograms}} images in .jpeg format to a single pdf file.
}
\details{
The function combines  spectrograms for complete sound files from the \code{\link{full_spectrograms}} function into
a single pdf (for each sound file).
}
\examples{
\dontrun{
# save sound file examples
data(list = c("Phae.long1", "Phae.long2"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

full_spectrograms(
  sxrow = 2, rows = 8, pal = reverse.heat.colors, wl = 300,
  it = "jpeg", path = tempdir()
)

# now create single pdf removing jpeg
full_spectrogram2pdf(keep.img = FALSE, path = tempdir())

# check this floder
tempdir()
}

}
\references{
Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
}
\seealso{
\code{\link{full_spectrograms}}, \code{\link{catalog2pdf}}
}
\author{
Marcelo Araya-Salas (\email{marcelo.araya@ucr.ac.cr})
}
