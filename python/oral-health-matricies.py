# -*- coding: utf-8 -*-
import config
import pandas as pd
import EquiPy.Matrix as Mat
import matplotlib.pyplot as plt
import numpy as np
from textwrap import wrap
from os.path import join

import seaborn as sns
custom_params = {"axes.spines.right": False, "axes.spines.top": False}
sns.set_theme(style="ticks", rc=custom_params)

#%% Load and process data

data = pd.read_excel(
        config.data_path + "\\Oral Health Epi Survey - 2024.xlsx",
        sheet_name = "5Y data 2024"
    )

# Update Broad ethnicity label
data["Broad_Ethnicity"] = [name.split(" - ")[1] for name in data["Higher Ethnic Code"]]
data["Broad_Ethnicity"] = [name.replace(" / ", "/") for name in data["Broad_Ethnicity"]]
data["Broad_Ethnicity"] = ["\n".join(wrap(name, 15)) for name in data["Broad_Ethnicity"] ]

# Update IMD column names
data["IMD19_LA_Quintile"] = data["IMD (2019) Upper Tier LA Quintile"]
data["IMD19_National_Quintile"] = data["IMD (2019) National Quintile"]
data["IMD19_National_3+"] = np.where(
    data["IMD19_National_Quintile"] < 3,
    data["IMD19_National_Quintile"],
    "3+"
    )

# Create boolean outcomes
data["Plaque"] = data["plaque"] != "0 - Teeth appear clean"
data["Plaque"][data["plaque"] == "X - Not recorded"] = np.nan

data["Enamel_Caries"] = data["Any enamel caries"] == "1 - Yes"
data["Enamel_Caries"][data["Any enamel caries"] == "X - Not recorded"] = np.nan

data["Incisor_Caries"] = data["Incisor caries present (ECC)"] == "1 - Yes"
data["Incisor_Caries"][data["Incisor caries present (ECC)"] == "X - Not recorded"] = np.nan

data["PUFA_signs"] = data["pufa"] != "0 - No teeth with pufa signs"
data["PUFA_signs"][data["pufa"] == "X - Not recorded"] = np.nan

data["Decayed_teeth"] = data["dt"] > 0
data["Missing_teeth"] = data["mt"] > 0
data["Filled_teeth"] = data["ft"] > 0 
data["Missing_filled_decayed_teeth"] = data["dmft"] > 0
data["total_dmtf"] = data["dmft"]
    
data.to_csv(
    config.data_path + "\\processed-oral-health-data-2024.csv"
    )
    
#%% Define deprivation indicies and outcomes to be plotted
    
# Type : data column
IMD_types = {
    "National-all" : "IMD19_National_Quintile",
    "LA" : "IMD19_LA_Quintile",
    "National" : "IMD19_National_3+"
    }

# data column : Plot title
outcomes = {
    "Plaque" : "% with Plaque",
    "Enamel_Caries" : "% with 1 or more\nEnamel Caries",
    "Incisor_Caries" :"% with Incisor\nCaries",
    "PUFA_signs" : "% with 1 or more\nPUFA signs",
    "Decayed_teeth" : "% with 1 or more\ndecayed teeth",
    "Missing_teeth" : "% with 1 or more\nmissing teeth",
    "Filled_teeth" : "% with 1 or more\nfilled teeth",
    "Missing_filled_decayed_teeth" : "% with one or more\nDMFT"
    }

#%% Plot number surveyed
for IMD_type in IMD_types.keys():
    count_pivot = Mat.get_pivot(
            data, 
            eth_col = "Broad_Ethnicity", 
            IMD_col = IMD_types[IMD_type],
            mode="count"
            ).astype(int)
    
    if IMD_type == "National":
        IMD_ticks = ["1\nMost\ndeprived", "2", "3+"]
    else:
        IMD_ticks =["1\nMost\ndeprived","2","3","4","5\nLeast\ndeprived"]
    
    fig = Mat.inequality_map(count_pivot, 
                       title = "Children Surveyed",
                       supp_thresh=15,
                       supp_label= "<15",
                       CI_method = "Wilson",
                       palette="Blues",
                       IMD_ticks = IMD_ticks,
                       width = 8,
                       bar_rel_size = [0.25, 0.2])
    
    fig.axes[0].set_ylabel("IMD Quintile\n(" +IMD_type +")")
    fig.axes[0].set_xlabel("")
    
    # Save matrix
    save_name = join("..","output", "matricies/2024",
                     IMD_type,"children_surveyed.png")
    fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
plt.close("all")

#%% Plot inequality matricices

for IMD_type in IMD_types.keys():
    
    if IMD_type == "National":
        IMD_ticks = ["1\nMost\ndeprived", "2", "3+"]
    else:
        IMD_ticks =["1\nMost\ndeprived","2","3","4","5\nLeast\ndeprived"]
    
    for i, var in enumerate(outcomes.keys()):  
        data_i = data.filter(np.logical_not(data[var].isna()))
        perc_pivot = Mat.get_pivot(
                data, 
                var,
                eth_col = "Broad_Ethnicity", 
                IMD_col = IMD_types[IMD_type],
                mode="percentage"
                ).fillna(0)
    
        count_pivot = Mat.get_pivot(
                data, 
                eth_col = "Broad_Ethnicity", 
                IMD_col = IMD_types[IMD_type],
                mode="count"
                )
        
        fig = Mat.inequality_map(count_pivot, 
                           perc_pivot,
                           title = outcomes[var],
                           ttest = True,
                           supp_thresh=15,
                           IMD_ticks = IMD_ticks,
                           supp_label = "<15",
                           CI_method = "Wilson",
                           palette="Blues",
                           width = 8,
                           bar_rel_size = [0.25, 0.2]
                           )
        fig.axes[0].set_ylabel("IMD Quintile\n(" +IMD_type +")")
        fig.axes[0].set_xlabel("")
        
        # Save matrix
        save_name = join("..","output", "matricies/2024",
                     IMD_type,var+".png")
        fig.savefig(save_name, bbox_inches = "tight",
                        dpi = 300)

plt.close("all")

#%% Mean DMTF
for IMD_type in IMD_types.keys():
    
    if IMD_type == "National":
        IMD_ticks = ["1\nMost\ndeprived", "2", "3+"]
    else:
        IMD_ticks =["1\nMost\ndeprived","2","3","4","5\nLeast\ndeprived"]
    
    av_pivot = Mat.get_pivot(
            data, 
            "total_dmtf",
            eth_col = "Broad_Ethnicity", 
            IMD_col = IMD_types[IMD_type],
            mode="avg"
            ).fillna(0)
    
    count_pivot = Mat.get_pivot(
            data, 
            eth_col = "Broad_Ethnicity", 
            IMD_col = IMD_types[IMD_type],
            mode="count"
            )
    
    fig = Mat.inequality_map(count_pivot, 
                       av_pivot,
                       agg_type = "avg",
                       title = "Mean DMFT\nper child",
                       supp_thresh=15,
                       supp_label = "<15",
                       IMD_ticks = IMD_ticks,
                       magnitude = 1,
                       CI_method = "Byar",
                       palette="Blues",
                       width = 8,
                       bar_rel_size = [0.25, 0.2]
                       )
    fig.axes[0].set_ylabel("IMD Quintile\n(" +IMD_type +")")
    fig.axes[0].set_xlabel("")
    
    # Save matrix
    save_name = join("..","output", "matricies",
                 IMD_type,"dmft.png")
    fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
    
plt.close("all")