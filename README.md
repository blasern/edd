[![Build Status](https://travis-ci.org/blasern/edd.svg?branch=master)](https://travis-ci.org/blasern/edd)

# edd - Empirical distribution distances

This R package collects distance functions between empirical distributions. The purpose of the package is to provide a common framework for comparing empirical distributions and not to provide novel distance measures. It currently includes the following distance measures:
- [Wasserstein distance](https://en.wikipedia.org/wiki/Wasserstein_metric)
- [Kolmogorov distance](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test)
- [Hausdorff distance](https://en.wikipedia.org/wiki/Hausdorff_distance)
- [Symmetric Kullback–Leibler distance](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence)
- [Lp distance](https://en.wikipedia.org/wiki/Lp_space)
- [Moment distance](https://en.wikipedia.org/wiki/Method_of_moments_(probability_theory))
- [Ky Fan distance](https://en.wikipedia.org/wiki/Convergence_of_random_variables#Properties_2)

## Installation 

To install the latest development version from github:

    install.packages("devtools")
    devtools::install_github("blasern/edd")

## Development
 
If you find issues, please [let me know](https://github.com/blasern/edd/issues). 
If you would like to contribute, please [create a pull request](https://github.com/blasern/edd/compare).
