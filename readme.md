# Analysis Scripts

## Folder Overview
The scripts are saved in different folders, depending on what they do.
The first one is the `prep` folder.
Here the data is cleaned and prepared and additional variables are generated.
Note that the scripts expect the raw data to be located in a folder with the path (from the location of this readme file) `'../data/raw'`.
Next, the `stats` and `plotting` folders contain scripts for statistics and graphics.
Both folders save their results into the `output` folder.

## Runing the Analysis
To run the analysis as a whole you can just source the `main.R` file.
It will source all other files in the order in which they work together.
All outputs that are used in the paper are saved to the `outputs` folder.
