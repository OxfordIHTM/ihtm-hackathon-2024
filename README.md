
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Oxford International Health and Tropical Medicine Hackathon 2024 <img src="https://raw.githubusercontent.com/OxfordIHTM/open-reproducible-science/main/images/oxford_codehub.png" width="200px" align="right" />

<!-- badges: start -->

[![License for
data](https://img.shields.io/badge/license%20(for%20data)-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
[![License for
code](https://img.shields.io/badge/license%20(for%20code)-GPL3.0-blue.svg)](https://opensource.org/licenses/gpl-3.0.html)
<!-- badges: end -->

This repository contains instructions, data and code for the [University
of Oxford MSc in International Health and Tropical
Medicine](https://www.ox.ac.uk/admissions/graduate/courses/msc-international-health-and-tropical-medicine)
inaugural **Hackathon 2024**. The **Hackathon 2024** event is part of
the [MSc
course](https://www.ox.ac.uk/admissions/graduate/courses/msc-international-health-and-tropical-medicine)’s
[lecture series on Open Science and Reproducible Research in
R](https://oxford-ihtm.io/open-reproducible-science).

## Motivation

**Hackathon 2024** caps the students’ introduction to [Open Science and
Reproducible Research in
R](https://oxford-ihtm.io/open-reproducible-science) through an actual
global health project within which they are to serve as researchers/data
scientists. This exercise aims to provide the students a platform from
which to apply skills in working with data using R that they have been
learning and practising for about the past 6 weeks while at the same
time exposing them to a collaborative team environment.

## Format

**Hackathon 2024** is structured as a **problem-based learning**
exercise, a format that the course students are already familiar with
given similar approaches done for other lectures. Briefly, this PBL
exercise presents the problem first rather than teaching relevant
material and subsequently having students apply the knowledge to solve
the problem. Whilst the previous lectures in the [Open Science and
Reproducible Research in
R](https://oxford-ihtm.io/open-reproducible-science) series have
provided foundational skills in R, the PBL approach for this hackathon
will challenge the students to further explore and learn the extensive
functionalities R has to offer in order to appropriately solve the
problem/s they have been given to solve. This PBL is group-orientated
and simulates a collaborative research/data science working environment
facilitated through the use of [git](https://git-scm.com/) and
[GitHub](https://github.com).

Through this approach, the students are expected to:

1.  Examine and define the problem.

2.  Explore what they already know about underlying issues related to
    it.

3.  Determine what they need to learn and where they can acquire the
    information and tools necessary to solve the problem.

4.  Evaluate possible ways to solve the problem.

5.  Solve the problem.

6.  Report on their findings.

## The case

The case study along with the hackathon rules are presented here -
<https://oxford-ihtm.io/ihtm-hackathon-2024/case_study.html>.

## Repository/project structure

This R project has the following structure:

    ihtm-hackathon-2024
        |-- data/
        |-- docs
          |-- case_study.html
        |-- outputs/
        |-- packages.R
        |-- R/
        |-- reports
          |-- case_study.Rmd
          |-- sudan_health_nutrition.Rmd
        |-- sudan_health_nutrition_1.R
        |-- sudan_health_nutrition_2.R
        |-- sudan_health_nutrition_3.R
        |-- sudan_health_nutrition_4.R
        |-- sudan_health_nutrition_5.R
        |-- sudan_health_nutrition_6.R
        |-- sudan_health_nutrition.R

- `data/` contains the datasets required for this project. These
  datasets may either be provided by the module leader and/or retrieved
  grammatically by the hackathon participants and saved into this
  directly;

- `docs/` contains compiled reports produced from Rmarkdown documents;

  Currently, this folder contains the file `case_study.html` which is
  the HTML output of `case_study.Rmd` (see below) which details the case
  study and the rules for this hackathon;

- `outputs/` contains figures, intermediate or final results tables, and
  other outputs produced by this scientific workflow;

- `packages.R` is the overall R script that declares the R package
  dependencies of the entire project;

- `R/` contains functions created or that will be created for use in
  this project;

  Currently, this folder contains an R file called `get_data.R`. This
  file specifies a function called `get_data()` which retrieves and
  processes the data for this hackathon. It also saves this dataset as a
  CSV file called `nutrition_survey_dataset.csv` in the `data` folder;

- `reports/` contains Rmarkdown file/s for producing report/s produced
  by this scientific workflow;

  Currently, this directory contains the following files

  - `case_study.Rmd` - Rmd file that produces the `case_study.html` file
    found in the `docs` folder
  - `sudan_health_nutrition.Rmd` - Rmd file for producing the overall
    report

- `sudan_health_nutrition_1.R` is the R workflow script for section 1 of
  the analysis;

- `sudan_health_nutrition_2.R` is the R workflow script for section 3 of
  the analysis;

- `sudan_health_nutrition_3.R` is the R workflow script for section 3 of
  the analysis;

- `sudan_health_nutrition_4.R` is the R workflow script for section 4 of
  the analysis;

- `sudan_health_nutrition_5.R` is the R workflow script for section 5 of
  the analysis;

- `sudan_health_nutrition_6.R` is the R workflow script for section 6 of
  the analysis; and,

- `sudan_health_nutrition.R` is the overall R workflow script for the
  complete analysis and reporting.

## Reproducibility

This project is built on R version 4.3.2.

To work on this project, please follow these steps:

1.  Clone this project onto your local machine. Instructions on how this
    is done can be found
    [here](https://oxford-ihtm.io/ihtm-handbook/clone-repository.html).

2.  In your local clone of the project, please make sure to create a new
    `branch` from the `main` branch. You should name this `branch` in
    such a way that uniquely identifies it as your personal branch
    (i.e., give it your name). Please avoid blank spaces in branch
    names. If you need to put a space, use a `-` or a `_`.

3.  Install all declared R package dependencies found in `packages.R`.

    - Check if the packages listed in `packages.R` are already installed
      using the following code:

    ``` r
    installed.packages() |>
      (\(x) x[ , 1])() |>
      (\(x) x[c("name_of_package1", "name_of_package2", "name_of_package3")])()
    ```

    Please run this code direct to your R console rather than encoding
    in the R scripts in this project.

    Please make sure to replace the placeholder text `name_of_project1`
    etc with the actual names of the packages listed in the `packages.R`
    file.

    This code will show which of the packages listed in `packages.R` are
    already installed in your computer. Packages listed in `packages.R`
    that are not shown in the output of the code above are the packages
    that are not yet installed in your computer.

    - Install the packages that are not yet installed using the
      following code:

    ``` r
    install.packages(c("name_of_package1", "name_of_package2", "name_of_package3")
    ```

    Please run this code direct to your R console rather than encoding
    in the R scripts in this project.

    Please make sure to replace the placeholder text `name_of_project1`
    etc with the actual names of the packages listed in the `packages.R`
    file that are not yet installed in your computer.

Once all R package dependencies have been installed, you should now be
able to work on this project on your own branch and make
changes/contributions as directed by project lead.

## License

Unless otherwise specified, data used in this repository are licensed
under a [CC0 1.0
Universal](https://creativecommons.org/publicdomain/zero/1.0/) license.

All code in this repository are licensed under a [GNU General Public
License 3 (GPL-3)](https://opensource.org/licenses/gpl-3.0.html)
license.

<br/> <br/>