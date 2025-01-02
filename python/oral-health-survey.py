# -*- coding: utf-8 -*-
import config
import pandas as pd



#%% Load and process data

data = pd.read_excel(
    config.data_path + "\\Oral Health Epi Survey.xlsx"
    )

# Update Broad ethnicity label
data["Broad Ethnicity"] = [name.split(" - ")[1] for name in data['Higher Ethnic Code']]

# Create boolean outcomes
data["Plaque"] = data["plaque"] != '0 - Teeth appear clean'
data["Enamel_Caries"] = data['Any enamel caries'] == '1 - Yes'
data["Incisor_Caries"] = data['Incisor caries present (ECC)'] = '1 - Yes'
data["PUFA_signs"] = data['pufa'] != '0 - No pufa signs'