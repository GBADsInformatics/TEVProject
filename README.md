-   [Description](#description)
-   [Installation](#installation)
-   [Repository Structure](#repository-structure)
    -   [Make](#make)
    -   [Data](#data)
        -   [Metadata](#metadata)
    -   [Figures and Tables](#figures-and-tables)
    -   [Codes](#codes)
    -   [Documentation](#documentation)
    -   [Figures](#figures)
-   [Licence](#licence)

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![License: CC BY-SA
4.0](https://img.shields.io/badge/license-CC%20BY--SA%204.0-blue.svg)](https://cran.r-project.org/web/licenses/CC%20BY-SA%204.0)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Description
===========

This repository contains all `R` code, output data and figures for the
[*GBADS*](https://animalhealthmetrics.org/) paper “Approximating the
Global Economic (Market) Value of Farmed Animals”.

> > Schrobback, Peggy, Gabriel Dennis, Yin Li, Dianne Mayberry,
> > Alexandra Shaw, Theodore Knight-Jones, Thomas Lloyd Marsh, et
> > al. 2023. “Approximating the Global Economic (Market) Value of
> > Farmed Animals.” *Global Food Security* 39: 100722.
> > <a href="https://doi.org/https://doi.org/10.1016/j.gfs.2023.100722" class="uri">https://doi.org/https://doi.org/10.1016/j.gfs.2023.100722</a>.

Installation
============

To install this package run the following code

``` r
if (!require(remotes)) {
  install.packages("remotes")
}

remotes::install_github(repo = "GBADsInformatics/TEVProject")
```

Repository Structure
====================

This repository is structured similar to an R package, however, there
are minimal differences which make this package non-conforming to
certain standards.

Make
----

The `R` directory contains any common functions which are used across
the entire project. The `inst` directory contains `R` scripts which are
intended to be run via the GNU `Makefile` which specifies the required
processes to build each target. Targets are run using the syntax

``` bash
make name-of-file-to-make
```

Currently, not all the dependencies for the targets which download data
are specified in subsequent targets, due to some side effects of the
download process, however, all files in the `data/processed` and
`data/output` directories should be targets which can be recreated by
the project `Makefile`.

Project locations and directory structure are outlined in the project
configuration file `conf/config.yml`, in accessing these parameters in
code is managed by the `R` package `config`. The location of the
configuration file can be modified, however, if this is done, then one
must also modify the environment variable `R_CONFIG_FILE` to match this
new location.

Environment and package management is done using `renv`, `renv.lock`
specifies the hashes for each package used. Package imports are also
listed in the package `DESCRIPTION` file.

Data
----

Currently, the processed and output data is located in `data/processed/`
and `data/output`. Source data is located in `data/source` and is not
included in this repository due to its size. The versions of the source
data which was downloaded for this project is backed up on an internal
*CSIRO* cloud storage platform.

Output data is primarily stored in `parquet` files in the `data/output`
directory.

*(Note: These locations are specified in the project configuration yaml
file)*

### Metadata

Metadata for this project in JSON format for the GBADS Knowledge Engine
is stored in the directory `data/metadata` and can be generated using
the Makefile target `make data/metadata`.

Figures and Tables
------------------

Output figures and tables are located in `output/figures/`,
`output/tables/`.

*(Note: This is specified in the project configuration yaml file)*

Codes
-----

FAOSTAT item codes and country codes are located in `data/codes`.

*(Note: This is specified in the project configuration yaml file)*

Documentation
-------------

[pkgdown](https://pkgdown.r-lib.org/) Documentation is available in the
`docs/` directory and can be accessed locally.

Figures
-------

Paper figures can be seen in the [figures](output/figures/README.md)
directory.

Licence
=======

This project is under a creative commons licence.
