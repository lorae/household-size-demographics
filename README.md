# Household Size Demographics

This repository provides full replication code for analyzing the extent to which American household sizes have changed over time, and by sociodemographic dimension. Detailed instructions for running the code follow.

# `data` directory
If you have cloned this repository from GitHub, it will include a `data` directory which contains an empty `ipums_microdata` directory. Because of the large file size, this data is not stored on GitHub. Either request the file directly from the authors or follow these instructions to download the data from IPUMS directly:

## Download from IPUMS USA

1. Navigate to the [IPUMS USA login page](https://uma.pop.umn.edu/usa/authentication/login). If you do not already have user credentials, you will need to set them up before proceeding. Log into the portal.

2. Request a data extract with the following information:

  **Sample** (count: 2)
  -   2000 5%
  -   2019 ACS 5yr
  
  **Variables** (count: 110)
  - [YEAR](https://usa.ipums.org/usa-action/variables/YEAR)
  - [MULTYEAR](https://usa.ipums.org/usa-action/variables/MULTYEAR)
  - TODO: fill out the rest

# running the code

The code for this project is stored in the `src` folder. Code is divided into two main directories: `scripts` and `utils`. The `scripts` directory contains executable code which runs the analyses. The `utils` foler contains necessary accessory modules, typically in the form of functions, that are sourced when certain scripts run. These functions are separated due to their complexity. Code underlying them can be inspected more directly when they are isolated, and they are subject to a battery of unit tests.

We'll now explain each of the `scripts` files in turn, which walk the researcher through data ingestion, throughput generation, and generation of output and figures.

**The scripts should be run in the following order**:
1. `import-ipums.R`
2. `process-ipums.R`

## import-ipums.R

This script serves two purposes:
1. Read in the IPUMS USA microdata from its raw, brittle format in the source .dat.gz file into a DuckDB database, which can be more agilely manipulated and analyzed.

2. Read IPUMS USA pull metadata and saved clean files into the docs/ folder that help with later data reconciliation and labelling.

This script leverages the `ipumsr` package for this purpose. Due to the relatively large size of the source data (2 GB as of June 2025), it requires up to 5 minutes to run.

**Inputs**:
- `data/ipums-microdata/usa_0020.xml`
- `data/ipums-microdata/usa_0020.dat.gz`

**Outputs**:
- 
- `docs/ipums-data-dictionary.html`
- `docs/ipums_value_labels.RData`

Outputs are used downstream for graph labelling, re-attaching labels after KOB regressions are done, and more. 
TODO: specify more here.

----
# File structure


----
# TODOS / musings

TODO: should the detailed headers on these scripts be fully supplanted by the contents of this README?

TODO: I'm going to have to write more on dataduck, potentially rename the package and come up with a mroe strategic vision for it and how it can be used in conjunction with these 3(!) related projects.