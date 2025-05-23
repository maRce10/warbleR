% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_coordination.R
\name{test_coordination}
\alias{test_coordination}
\title{Randomization test for singing coordination}
\usage{
test_coordination(
  X = NULL,
  iterations = 1000,
  ovlp.method = "count",
  randomization = "keep.gaps",
  less.than.chance = TRUE,
  parallel = 1,
  pb = TRUE,
  rm.incomp = FALSE,
  cutoff = 2,
  rm.solo = FALSE
)
}
\arguments{
\item{X}{Data frame containing columns for singing event (sing.event),
individual (indiv), and start and end time of signal (start and end).}

\item{iterations}{number of iterations for shuffling and calculation of the expected number of overlaps. Default is 1000.}

\item{ovlp.method}{Character string defining the method to measure the amount of overlap. Three methods are available:
\itemize{
   \item \code{count}: count the number of overlapping signals (default)
   \item \code{time.overlap}: measure the total duration (in s) in which signals overlap
   \item \code{time.closest}: measure the time (in s) to the other individual's closest signal. This is the only method that can take more than 2 individuals.
   }}

\item{randomization}{Character string defining the procedure for signal randomization. Three methods are available:
\itemize{
 \item \code{keep.gaps} the position of both signals and gaps (i.e. intervals between signals) are randomized. Default.
 \item \code{sample.gaps} gaps are simulated using a lognormal distribution with
 mean and standard deviation derived from the observed gaps. Signal position is randomized.
 \item \code{keep.song.order} only the position of gaps is randomized.
}
More details in Masco et al. (2015).}

\item{less.than.chance}{Logical. If \code{TRUE} the test evaluates whether overlaps occur less often than expected by chance.
If \code{FALSE} the opposite pattern is evaluated (whether overlaps occur more often than expected by chance).
Default is \code{TRUE}.}

\item{parallel}{Numeric. Controls whether parallel computing is applied.
It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).}

\item{pb}{Logical argument to control progress bar. Default is \code{TRUE}.}

\item{rm.incomp}{Logical. If \code{TRUE} removes the events that don't have 2 interacting individuals. Default is
\code{FALSE}.}

\item{cutoff}{Numeric. Determines the minimum number of signals per individual in a singing event. Events not meeting
this criterium are removed. Default is 2.
Note that randomization tests are not reliable with very small sample sizes. Ideally 10 or more signals per individual
should be available in each singing event.}

\item{rm.solo}{Logical. Controls if signals that are not alternated at the start or end of the
sequence are removed (if \code{TRUE}). For instance, the sequence of signals A-A-A-B-A-B-A-B-B-B (in which A and B represent different individuals, as in the 'indiv' column) would be subset to
A-B-A-B-A-B. Default is  \code{FALSE}.}
}
\value{
A data frame with the following columns:
\itemize{
   \item \code{sing.event}: singing event ID
   \item \code{obs.overlap}: observed amount of overlap (counts or total duration, depending on overlap method, see 'ovlp.method' argument)
   \item \code{mean.random.ovlp}: mean amount of overlap expected by chance
   \item \code{p.value}: p value
   \item \code{coor.score}: coordination score (\emph{sensu} Araya-Salas et al. 2017),
   calculated as:
   \deqn{(obs.overlap - mean.random.ovlp) / mean.random.ovlp}
   Positive values indicate a tendency to overlap while negative values indicate a tendency to alternate. NA values will be returned when events cannot be randomized (e.g. too few signals).
   }
}
\description{
Monte Carlo randomization test to assess the statistical significance of overlapping or alternating singing (or any other simultaneously occurring behavior).
}
\details{
This function calculates the probability of finding an equal or more extreme amount of song overlap (higher or lower) in a coordinated singing event (or any pair-coordinated behavior).
The function shuffles the sequences of signals and silence-between-signals for both individuals to produce
a null distribution of overlaps expected by chance. The observed overlaps is compared to this
expected values. The p-values are calculated as the proportion of random expected values that were lower (or higher)
than the observed value. All procedures described in Masco et al. (2015) are implemented. In addition, either the number (\code{ovlp.method = "count"}) or the total duration (\code{ovlp.method = "time.overlap"}) in which signals overlap can be used for estimating the overall degree of overlap. The function runs one test for each singing event in the input data frame. This function assumes that there are no overlaps between signals belonging to the same individual. See Masco et al. (2015) for recommendations on randomization procedures for specific signal structures.
}
\examples{
{
#load  simulated singing data (see data documentation)
data(sim_coor_sing)

# set global options (this can also be set within the function call)
warbleR_options(iterations = 100, pb = FALSE)

# testing if coordination happens less than expected by chance
test_coordination(sim_coor_sing)

# testing if coordination happens more than expected by chance
test_coordination(sim_coor_sing, less.than.chance = FALSE)

# using "duration" method and "keep.song.order" as randomization procedure
test_coordination(sim_coor_sing, ovlp.method =  "time.overlap",
randomization = "keep.song.order")
}

}
\references{
Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals.
 Methods in Ecology and Evolution, 8(2), 184-191.

Araya-Salas M., Wojczulanis-Jakubas K., Phillips E.M., Mennill D.J., Wright T.F.
(2017) To overlap or not to overlap: context-dependent coordinated singing in
lekking long-billed hermits. Animal Behavior  124, 57-65.

Keenan EL, Odom KJ, M Araya-Salas, KG Horton, M Strimas-Mackey,MA Meatte, NI Mann,PJ Slater,JJ Price, and CN Templeton . 2020. Breeding season length predicts duet coordination and consistency in Neotropical wrens (Troglodytidae). Proceeding of the Royal Society B. 20202482.

Masco, C., Allesina, S., Mennill, D. J., and Pruett-Jones, S. (2015). The Song
Overlap Null model Generator (SONG): a new tool for distinguishing between random
and non-random song overlap. Bioacoustics.

Rivera-Caceres K, E Quiros-Guerrero E, M Araya-Salas, C Templeton & W Searcy. (2018). Early development of vocal interaction rules in a duetting songbird. Royal Society Open Science. 5, 171791.

Rivera-Caceres K, E Quiros-Guerrero, M Araya-Salas & W Searcy. (2016). Neotropical wrens learn new duet as adults. Proceedings of the Royal Society B. 285, 20161819
}
\author{
Marcelo Araya-Salas (\email{marcelo.araya@ucr.ac.cr})
}
