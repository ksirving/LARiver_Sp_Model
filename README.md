ReadME

This read me describes the folders of code for LAR flows study project

all models are outlined in - https://ftp.sccwrp.org/pub/download/DOCUMENTS/TechnicalReports/1154_LARiverAquaticLifeUses.pdf
(Stein et al., 2021a)

all scenario results are outlined in - https://ftp.sccwrp.org/pub/download/DOCUMENTS/TechnicalReports/1196_LARiverFlowEvaluations.pdf
(Stein et al. 2021b)

Note that this repo has been combined from several others with different authors,
so the naming conventions and numbering may not be consistent


## DataFormat folder

upload data, format and combine for use in models
covers Santa Ana Sucker and Arryoyo Chub (not used in this project)

## ModelingBuild

Builds the models for all species: SAS, Willow, typha. Note that Cladophora model build is not included

## ModelRuns

Runs the models for all species and hydraulics with current conditions, including threshold based models
predicts probability of occurrence (or equivalent)
calculates suitability over time and number of days for each node

## Results

summarises the output from model runs

## Synthesis

Combine all info from model runs
calculates flow limits 
creates table of suitability for species and node. final table combinea time stat 
and number of days by taking limiting outcome (done manually)

## Scenarios

runs though models for all species etc under scenarios. Results in Stein et al 2021b

## Data_Products

main data products for tech reports

