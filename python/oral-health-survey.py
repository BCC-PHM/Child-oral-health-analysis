# -*- coding: utf-8 -*-
import config
import pandas as pd
import EquiPy.Matrix as Mat
import matplotlib.pyplot as plt
from textwrap import wrap
from os.path import join

import seaborn as sns
custom_params = {"axes.spines.right": False, "axes.spines.top": False}
sns.set_theme(style="ticks", rc=custom_params)

#%% Load and process data

data = pd.read_excel(
    config.data_path + "\\Oral Health Epi Survey.xlsx"
    )

# Update Broad ethnicity label
data["Broad_Ethnicity"] = [name.split(" - ")[1] for name in data["Higher Ethnic Code"]]
data["Broad_Ethnicity"] = [name.replace(" / ", "/") for name in data["Broad_Ethnicity"]]
data["Broad_Ethnicity"] = ["\n".join(wrap(name, 15)) for name in data["Broad_Ethnicity"] ]

# Update IMD column names
data["IMD19_LA_Quintile"] = data["IMD (2019) Upper Tier LA Quintile"]
data["IMD19_National_Quintile"] = data["IMD (2019) National Quintile"]

# Create boolean outcomes
data["Plaque"] = data["plaque"] != "0 - Teeth appear clean"
data["Enamel_Caries"] = data["Any enamel caries"] == "1 - Yes"
data["Incisor_Caries"] = data["Incisor caries present (ECC)"] == "1 - Yes"
data["PUFA_signs"] = data["pufa"] != "0 - No pufa signs"

data["any_dental_issue"] = list(data["Plaque"]) or \
    list(data["Enamel_Caries"]) or \
    list(data["Incisor_Caries"]) or \
    list(data["PUFA_signs"])
    
data.to_csv(
    config.data_path + "\\processed-oral-health-data.csv"
    )
    
#%% Define deprivation indicies and outcomes to be plotted
    
# Type : data column
IMD_types = {
    "National" : "IMD19_National_Quintile",
    "LA" : "IMD19_LA_Quintile"
    }

# data column : Plot title
outcomes = {
    "Plaque" : "% with Plaque\n(<1/3 Labial Surfaces)",
    "Enamel_Caries" : "% with 1 or more\nEnamel Caries",
    "Incisor_Caries" :"% with Incisor\nCaries",
    "PUFA_signs" : "% with 1 or more\nPUFA signs",
    "any_dental_issue" : "% with 1 or more\ndental issue"
    }

#%% Plot number surveyed
for IMD_type in IMD_types.keys():
    count_pivot = Mat.get_pivot(
            data, 
            eth_col = "Broad_Ethnicity", 
            IMD_col = IMD_types[IMD_type],
            mode="count"
            ).astype(int)
    
    fig = Mat.inequality_map(count_pivot, 
                       title = "Children Surveyed",
                       supp_thresh=0,
                       CI_method = "Wilson",
                       palette="Blues",
                       bar_rel_size = [0.25, 0.25])
    
    fig.axes[0].set_ylabel("IMD Quintile\n(" +IMD_type +")")
    fig.axes[0].set_xlabel("")
    
    # Save matrix
    save_name = join("..","output", "matricies",
                     IMD_type,"children_surveyed.png")
    fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
plt.close("all")

#%% Plot inequality matricices

for IMD_type in IMD_types.keys():
    for i, var in enumerate(outcomes.keys()):    
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
        count_pivot[count_pivot == 0] = 0.1
        
        fig = Mat.inequality_map(count_pivot, 
                           perc_pivot,
                           title = outcomes[var],
                           ttest = True,
                           supp_thresh=0.2,
                           supp_label = "No data",
                           CI_method = "Wilson",
                           palette="Blues",
                           bar_rel_size = [0.25, 0.25])
        fig.axes[0].set_ylabel("IMD Quintile\n(" +IMD_type +")")
        fig.axes[0].set_xlabel("")
        
        # Save matrix
        save_name = join("..","output", "matricies",
                     IMD_type,var+".png")
        fig.savefig(save_name, bbox_inches = "tight",
                        dpi = 300)
    break
plt.close("all")