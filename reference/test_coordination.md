# Randomization test for singing coordination

Monte Carlo randomization test to assess the statistical significance of
overlapping or alternating singing (or any other simultaneously
occurring behavior).

## Usage

``` r
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
```

## Arguments

- X:

  Data frame containing columns for singing event (sing.event),
  individual (indiv), and start and end time of signal (start and end).

- iterations:

  number of iterations for shuffling and calculation of the expected
  number of overlaps. Default is 1000.

- ovlp.method:

  Character string defining the method to measure the amount of overlap.
  Three methods are available:

  - `count`: count the number of overlapping signals (default)

  - `time.overlap`: measure the total duration (in s) in which signals
    overlap

  - `time.closest`: measure the time (in s) to the other individual's
    closest signal. This is the only method that can take more than 2
    individuals.

- randomization:

  Character string defining the procedure for signal randomization.
  Three methods are available:

  - `keep.gaps` the position of both signals and gaps (i.e. intervals
    between signals) are randomized. Default.

  - `sample.gaps` gaps are simulated using a lognormal distribution with
    mean and standard deviation derived from the observed gaps. Signal
    position is randomized.

  - `keep.song.order` only the position of gaps is randomized.

  More details in Masco et al. (2015).

- less.than.chance:

  Logical. If `TRUE` the test evaluates whether overlaps occur less
  often than expected by chance. If `FALSE` the opposite pattern is
  evaluated (whether overlaps occur more often than expected by chance).
  Default is `TRUE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- rm.incomp:

  Logical. If `TRUE` removes the events that don't have 2 interacting
  individuals. Default is `FALSE`.

- cutoff:

  Numeric. Determines the minimum number of signals per individual in a
  singing event. Events not meeting this criterium are removed. Default
  is 2. Note that randomization tests are not reliable with very small
  sample sizes. Ideally 10 or more signals per individual should be
  available in each singing event.

- rm.solo:

  Logical. Controls if signals that are not alternated at the start or
  end of the sequence are removed (if `TRUE`). For instance, the
  sequence of signals A-A-A-B-A-B-A-B-B-B (in which A and B represent
  different individuals, as in the 'indiv' column) would be subset to
  A-B-A-B-A-B. Default is `FALSE`.

## Value

A data frame with the following columns:

- `sing.event`: singing event ID

- `obs.overlap`: observed amount of overlap (counts or total duration,
  depending on overlap method, see 'ovlp.method' argument)

- `mean.random.ovlp`: mean amount of overlap expected by chance

- `p.value`: p value

- `coor.score`: coordination score (*sensu* Araya-Salas et al. 2017),
  calculated as: \$\$(obs.overlap - mean.random.ovlp) /
  mean.random.ovlp\$\$ Positive values indicate a tendency to overlap
  while negative values indicate a tendency to alternate. NA values will
  be returned when events cannot be randomized (e.g. too few signals).

## Details

This function calculates the probability of finding an equal or more
extreme amount of song overlap (higher or lower) in a coordinated
singing event (or any pair-coordinated behavior). The function shuffles
the sequences of signals and silence-between-signals for both
individuals to produce a null distribution of overlaps expected by
chance. The observed overlaps is compared to this expected values. The
p-values are calculated as the proportion of random expected values that
were lower (or higher) than the observed value. All procedures described
in Masco et al. (2015) are implemented. In addition, either the number
(`ovlp.method = "count"`) or the total duration
(`ovlp.method = "time.overlap"`) in which signals overlap can be used
for estimating the overall degree of overlap. The function runs one test
for each singing event in the input data frame. This function assumes
that there are no overlaps between signals belonging to the same
individual. See Masco et al. (2015) for recommendations on randomization
procedures for specific signal structures.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

Araya-Salas M., Wojczulanis-Jakubas K., Phillips E.M., Mennill D.J.,
Wright T.F. (2017) To overlap or not to overlap: context-dependent
coordinated singing in lekking long-billed hermits. Animal Behavior 124,
57-65.

Keenan EL, Odom KJ, M Araya-Salas, KG Horton, M Strimas-Mackey,MA
Meatte, NI Mann,PJ Slater,JJ Price, and CN Templeton . 2020. Breeding
season length predicts duet coordination and consistency in Neotropical
wrens (Troglodytidae). Proceeding of the Royal Society B. 20202482.

Masco, C., Allesina, S., Mennill, D. J., and Pruett-Jones, S. (2015).
The Song Overlap Null model Generator (SONG): a new tool for
distinguishing between random and non-random song overlap. Bioacoustics.

Rivera-Caceres K, E Quiros-Guerrero E, M Araya-Salas, C Templeton & W
Searcy. (2018). Early development of vocal interaction rules in a
duetting songbird. Royal Society Open Science. 5, 171791.

Rivera-Caceres K, E Quiros-Guerrero, M Araya-Salas & W Searcy. (2016).
Neotropical wrens learn new duet as adults. Proceedings of the Royal
Society B. 285, 20161819

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
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
#>   sing.event obs.ovlp mean.random.ovlp p.value coor.score
#> 1     altern 1.098593         3.364736    0.00     -0.673
#> 2       ovlp 5.991010         3.639684    1.00      0.646
#> 3    uncoord 3.097526         3.224544    0.42     -0.039
```
