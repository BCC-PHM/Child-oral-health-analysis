# -*- coding: utf-8 -*-
import config
import pandas as pd
import EquiPy.Matrix as Mat
import matplotlib.pyplot as plt
from textwrap import wrap
from os.path import join

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
    

#%% Plot number surveyed

IMD_types = {
    "National" : "IMD19_National_Quintile",
    "LA" : "IMD19_LA_Quintile"
    }

for IMD_type in IMD_types.keys():
    count_pivot = Mat.get_pivot(
            data, 
            eth_col = "Broad_Ethnicity", 
            IMD_col = IMD_types[IMD_type],
            mode="count"
            ).astype(int)
    
    fig = Mat.inequality_map(count_pivot, 
                       title = "Children\nSurveyed",
                       supp_thresh=0,
                       CI_method = "Wilson",
                       palette="Blues")
    
    fig.axes[0].set_ylabel("IMD Quintile\n(" +IMD_type +")")
    fig.axes[0].set_xlabel("")
    save_name = join("..","output", "matricies",
                     IMD_type,"children_surveyed.png")
    fig.savefig(save_name, bbox_inches = "tight",
                    dpi = 300)
plt.close("all")
