# PCR-GLOBWB-satellite-RF-reanalysis
Global reanalysis of the PCR-GLOBWB model using Random Forest and satellite products. 
Project is made for the Applied Data Science master thesis and is an continuation of Magni et al. (2023)

## Input data
Input data and outputs of the 30 arcmin run are available on Zenodo (input, output), obtained from Magni et al. (2023).

## Python module
For fast installation / update of necessary modules it is recommended to use the mamba package manager.
Current dependencies: numpy, pandas, alive_progress, netCDF4, xarray, multiprocessing.

The python module is used to normalize the static parameters from PCR-GLOBWB and for the normalization of the satellite products

After normalization of the satellite products. Values are extracted for grdc stations,

## R module 
The R module follows the post-processing phases described in manuscript. Dependencies can be installed using fun_0_install_dependencies.R. These are loaded at the beginning of each script using fun_0_load_library.R.

### Project_settings
Set settings for all codes
1. period: 2002-04-01 untill 2019-12-01
2. define number of available computer cores
3. define the selected satellite products
4. settings for the Randomf Forest model
5. Choose the number of random subsamples

### 0_preprocess_grdc
Merges stations from stationLatLon_daily.csv and stationLatLon_monthly.csv into stationLatLon.csv

### 0_preprocess_predictors
Parameters: generates timeseries of static catchment attributes (.csv)
qMeteoStatevars: merges timeseries of meteo input and state variables (.csv)
satellite: merges the different satellite products (.csv)
Merge all predictors : merges Parameters, qMeteoStatevars and satellite products (.csv)

### 1_preprocess_selectRegion
Choose different regions for analysis (.csv)

### 2_preprocess_checkMissing
calculate number of missing values for the selected period

### 3_correlation_analysis
bigTable : binds all stations predictor tables allpredictors

### 4_randomForest
1. Select different variable combinations to use in the random forest
2. generate a training table that contains ~70% of all available timesteps.
3. Tune -> Uses training table from 0 to tune Random Forest hyperparameters.
4. Train / Testing -> Calculates variable importance and KGE (before and after post-processing).

### 5_visualization
Used to visualize all modelling phases:
- Map with percentage of missing data at GRDC stations.
- Tuning Random Forest parameters.
- Plot of variable importance with uncertainty averaged for all subsamples.
- KGE: boxplots of each subsample and predictor configuration.
- KGE: cumulative distribution plots averaged over de the subsamples for each configuration and location
