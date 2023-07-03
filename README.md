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


### 0_preprocess_grdc
Upscales daily discharge to monthly, merges daily and monthly if a station has both, keeps upscaled daily if available at a timestep.
Merges stations from stationLatLon_daily.csv and stationLatLon_monthly.csv into stationLatLon.csv
Calculates % missing data for the modelled years (here 1979-2019).
0_preprocess_predictors
Parameters: generates timeseries of static catchment attributes (.csv)
qMeteoStatevars: merges timeseries of meteo input and state variables (.csv)
Merge all predictors : merges Parameters and qMeteoStatevars (.csv)
1_correlation_analysis
bigTable : binds all stations predictor tables allpredictors

### 2_randomForest
Subsample -> Subsamples stationLatLon.csv to generate a training table that contains ~70% of all available timesteps.
Tune -> Uses training table from 0 to tune Random Forest hyperparameters.
Train / Testing -> Can be done per subsample (2,3) or in batch (4). Calculates variable importance and KGE (before and after post-processing).
3_visualization
Used to visualize all modelling phases:

Map with percentage of missing data at GRDC stations.
Correlation plot to explore predictor selection.
Tuning Random Forest parameters.
Plot of variable importance with uncertainty averaged for all subsamples.
KGE: boxplots of each subsample and predictor configuration.
KGE: global maps (uncalibrated vs. post-processed).
Hydrographs: can be done for selected stations or in batch for all subsamples (only for allpredictors setup).
