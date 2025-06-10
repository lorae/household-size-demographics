# Household Size Demographics

This repository provides full replication code for analyzing the extent to which American household sizes have changed over time, and by sociodemographic dimension. Detailed instructions for running the code follow.

# `data` directory
If you have cloned this repository from GitHub, it will include a `data` directory which contains an empty `ipums_microdata` file. Because of the large file size, this data is not stored on GitHub. Either request the file directly from the authors or follow these instructions to download the data from IPUMS directly:

## Download from IPUMS USA

1. Navigate to the [IPUMS USA login page](https://uma.pop.umn.edu/usa/authentication/login). If you do not already have user credentials, you will need to set them up before proceeding. Log into the portal.

2. Request a data extract with the following information:

  **Sample** (count: 2)
  2000 5%
  2019 ACS 5yr
  
  **Variables** (count: 110)
  [YEAR](https://usa.ipums.org/usa-action/variables/YEAR)
  [MULTYEAR](https://usa.ipums.org/usa-action/variables/MULTYEAR)
  