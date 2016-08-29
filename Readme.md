Noisy data
==========
`P-values.R` illustrates how likely it is that significant P-values
are misleading when the magnitude of a between group difference is
small relative to within group variation. `P-values.pdf` was produced
by running this code.

`credible-intervals.R` illustrates how a Bayesian estimate of the
difference between the mean of (replicates of) the same populations
behaves with respect to coverage (the fraction of replicates in
which the 95% credible intervals contain the true difference) and
sign (the fraction of time in which the estimate of the difference has
the same sign as the true difference). The Bayesian model is implemented
in Stan (`point.stan`).