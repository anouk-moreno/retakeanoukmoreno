# Task 3: retakeanoukmoreno

##### *By: Anouk Moreno (Retake Exam 2025 Fortgeschrittene Statistische Software für Nebenfachstudierende)*

<!-- badges: start -->

<!-- badges: end -->

The goal of **retakeanoukmoreno** is to provide functions to prepare multiverse analysis datasets and visualize results through density plots and heatmap strips.

## Installation

You can install the development version of **retakeanoukmoreno** like so: from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("username/retakeanoukmoreno")
```

Make sure to replace username with your actual GitHub username!

## Example

This is a basic example which shows you how to generate a multi-verse plot:

``` r
library(retakeanoukmoreno)

# Prepare hurricane dataset
prep <- prep_data("hurricane")

# Plot combined density and heatmap strips
p <- plot_multiverse("hurricane")
p
```

Thank you :)

\- Anouk M.
