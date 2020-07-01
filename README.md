BrefInit
===================
*Constitution of the BRÉF database*

* Copyright 2019-2020 Vincent Labatut

BrefInit is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab website: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/BrefInit
* Contact: vincent.labatut@univ-avignon.fr

-----------------------------------------------------------------------

## Description
This set of R scripts was written to initialize BRÉF (Base de données Révisée des Élu·es de France), a data base containing a description of all types of reprsentatives elected in France. It is mainly based on the RNE (Répertoire National des Élus), the open data base of the French Parliament (including both the National Assembly and Senate), as well as the European Parliament website. All details are available in report [LFM'20]. 


# Data
The raw data is required to execute the scripts, as those basically perform a series of transformations and verifications on these data. This results in a number of tables and plots, as well as a unique table containing all the integrated and corrected data. All of these files are eventually meant to be made available under an open data license, after an embargo matching the end of Noémie Févrat's PhD.
(**TODO** upate)


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


## Dependencies
* [`stringdist`](https://cran.r-project.org/web/packages/stringdist/index.html) package: used to compare names and detect spelling mistakes.
* [`future.apply`](https://cran.r-project.org/web/packages/future.apply/index.html) package: parallel computing.


## Todo
* Update the data link after the end of the embargo.


## References
 * **[LFM'20]** V. Labatut, N. Févrat & G. Marrel, *BRÉF – Base de données Révisée des Élu·es de France*, Technical Report, Avignon Université, 2020. [⟨hal-02886580⟩](https://hal.archives-ouvertes.fr/hal-02886580)
