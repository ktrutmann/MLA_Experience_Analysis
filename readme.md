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

## `optimal_degsign.R`
The file `optimal_degsign.R` contains analyses to determine the optimal environment to maximize the impact of context sensitive belief updating.
The variables that are optimized are the probability of a price increase during a up- or downward drift (`prob_up`) and the length of a phase (`n_rounds_per_phase`).
The experiment was simulated using oTree bots in each combination of the experimental variables of interest.
For each scenario 200 participants were simulated, either updating through context sensitive Reinforcement Learning (using mean parameters from Trutmann, Heinke, Rieskamp, 2021), or through single-learning-rate Reinforcement Learning.
The target is to maximize the mean of the absolute difference between holding at the end of phase two of the two models.
The script should run on its own without dependencies on other scripts.