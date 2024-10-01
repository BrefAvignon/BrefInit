BrefInit v1.0.2
===================
*Constitution of the BRÉF database*

* Copyright 2019-2020 Vincent Labatut

BrefInit is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab website: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/BrefAvignon/BrefInit
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------

## Description
This set of `R` scripts was written to initialize BRÉF (*Base de données Révisée des Élu·es de France* -- Revised Database of Representatives Elected in France), a data base containing a description of all types of representatives elected in France under the Fifth Republic. It is mainly based on the RNE (*Répertoire National des Élus* -- National Registry of Elected Representatives), the open data base of the French Parliament (including both the National Assembly and Senate), as well as the European Parliament website. All details are available in our technical report [[LFM'20](#references)].

<p align="center">
  <img src="./logo_bref.svg" width="30%">
</p>

The data table produced by these scripts, as well as various plots and descriptive statistics, are available on [Zenodo](https://doi.org/10.5281/zenodo.13822771). Note that the next phase of the processing of the BRÉF data is implemented in a separate repository: see [BrefConversion](https://github.com/BrefAvignon/BrefConversion).


## Data
The raw data located in the `in` folder directly come from the mentioned sources (RNE and parliamentary datasets). They are required to execute the scripts, as those basically perform a series of transformations and verifications on these data. Because the files are too big, they have been zipped: it is thus necessary to unzip the files located in folder `in/extraction1/tables` before launching hte process.


## Organization
Here are the folders composing the project:
* Folder `in`: all the input files, including raw data, secondary data and verification files.
* Folder `log`: log files produced during the processing.
* Folder `out`: data files and plots produced during the processing.
* Folder `src`: all the source files.


## Installation
You just need to install `R` and the required packages:

1. Install the [`R` language](https://www.r-project.org/)
2. Download this project from GitHub and unzip.
3. Install the required packages: 
   1. Open the `R` console.
   2. Set the current directory as the working directory, using `setwd("<my directory>")`.
   3. Run the install script `src/install.R`.


## Use
In order to load the tables and generate the description files:

1. Open the `R` console.
2. Set the project root as the working directory, using `setwd` again.
3. Launch the `src/main.R` script, and the files will be generated in the `out` folder. Note that the process is very long (3-4 days in total).

This results in a number of tables and plots, as well as a unique table containing all the integrated and corrected data. It corresponds to the first verstion of the BRÉF, whose content was later moved to a proper PostgreSQL data base, resulting in the second version. This first version was used in the course of Noémie Févrat's PhD.


## Dependencies
* [`stringdist`](https://cran.r-project.org/web/packages/stringdist/index.html) package: used to compare names and detect spelling mistakes.
* [`future.apply`](https://cran.r-project.org/web/packages/future.apply/index.html) package: parallel computing.


## Todo
* ...


## Changelog
* 1.0.2: fixed errors in the documentations and comments, included the original data tables.
* 1.0.1: fixed a bug when loading CD (infinite loop when adding missing columns).
* 1.0.0: base version.


## References
 * **[LFM'20]** V. Labatut, N. Févrat & G. Marrel, *BRÉF – Base de données Révisée des Élu·es de France*, Technical Report, Avignon Université, 2020. [⟨hal-02886580⟩](https://hal.archives-ouvertes.fr/hal-02886580)
 * **[F'24]** N. Févrat, *Le "mandat de trop" ? La réélection des parlementaires et des maires en France et les conditions de sa remise en cause*, PhD Thesis, Avignon Université, 2024. [⟨tel-04550896⟩](https://hal.archives-ouvertes.fr/tel-04550896)
