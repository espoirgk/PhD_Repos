import pandas as pd
import xarray as xr
from typing import List, Any

#define path

def load_DATASET(PATH: str, job_NAME: str, yr_START: int, yr_END: int):
    

#load datasets
# file path that is being used to locate and open specific netCDF files
# in the code. The netCDF files contain data that is being used for
# analysis and manipulation in the code.

    df_stomate =xr.open_dataset(f"{PATH}/SBG/Output/MO/{job_NAME}_{yr_START}_{yr_END}_1M_stomate_history.nc")
    df_stomate_4dim = xr.open_dataset(f"{PATH}/SBG/Output/MO/{job_NAME}_{yr_START}_{yr_END}_1M_stomate_history_4dim.nc")
    df_sechiba = xr.open_dataset(f"/{PATH}/SRF/Output/MO/{job_NAME}_{yr_START}_{yr_END}_1M_sechiba_history.nc")
    df_sechiba_4dim = xr.open_dataset(f"{PATH}/SRF/Output/MO/{job_NAME}_{yr_START}_{yr_END}_1M_sechiba_history_4dim.nc")
    return df_stomate,df_stomate_4dim,df_sechiba,df_sechiba_4dim

df_stomate, df_stomate_4dim, df_sechiba, df_sechiba_4dim = load_DATASET(PATH = f"/home/scratch01/egaglo/IGCM_OUT/OL2/TEST/test", 
                                                                        job_NAME = "FG2-origin-PFT3", yr_START = 1761, yr_END = 2070)

def get_VARIABLES(data):   
    varNAME = []
    varLONGNAME = []
    for i, var in enumerate(data.variables.keys()):
        try:
            varNAME.append(var)
            varLONGNAME.append(f'{data[var].long_name} ({data[var].units})')
        except AttributeError:
            varLONGNAME.append(var)
    return varNAME, varLONGNAME


# Define the column names
colsNAME = ["STOMATE", "STOMATE_MEANING", "STOMATE_4DIM", "STOMATE_4DIM_MEANING",
            "SECHIBA", "SECHIBA_MEANING", "SECHIBA_4DIM", "SECHIBA_4DIM_MEANING"]

# Define the lists to merge
lists = [get_VARIABLES(df_stomate)[0], get_VARIABLES(df_stomate)[1],
         get_VARIABLES(df_stomate_4dim)[0], get_VARIABLES(df_stomate_4dim)[1],
         get_VARIABLES(df_sechiba)[0], get_VARIABLES(df_sechiba)[1],
         get_VARIABLES(df_sechiba_4dim)[0], get_VARIABLES(df_sechiba_4dim)[1]]

# Create a dictionary with corresponding colsNAME to lists
result_dict = {colsNAME[i]: lists[i] for i in range(len(colsNAME))}

#create pandas DataFrame from dictionary
df = pd.DataFrame(dict([(key, pd.Series(value)) for key, value in result_dict.items()]))

#view DataFrame
df.to_csv("variables_ORCHIDEE.csv", index=False)
