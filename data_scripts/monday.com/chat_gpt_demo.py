import pandas as pd #### Had to write this myself

# Load the data file
data_path = '/mnt/data/session_survey_censored.csv'
data_df = pd.read_csv(data_path)

# Show the first few rows of the data to get an overview
data_df.head()

# Columns related to facilitator feedback scores
facilitator_feedback_columns = [
    'fac_feedback_1', 'fac_feedback_2', 'fac_feedback_3', 'fac_feedback_4', 'fac_feedback_5',
    'fac_feedback_2_1', 'fac_feedback_2_2', 'fac_feedback_2_3', 'fac_feedback_2_4', 'fac_feedback_2_5'
]

# Group the data by facilitator names and calculate the mean for each feedback column
grouped_by_facilitator = data_df.groupby('facilitator')[facilitator_feedback_columns].mean()

# Calculate the overall mean score for each facilitator
grouped_by_facilitator['overall_mean_score'] = grouped_by_facilitator.mean(axis=1)

# Sort the facilitators by their overall mean score in descending order
sorted_facilitators = grouped_by_facilitator.sort_values(by='overall_mean_score', ascending=False)

sorted_facilitators.head()

import re

# Function to extract numerical score from the string value
def extract_score(value):
    if pd.isna(value):
        return None
    match = re.search(r'\((\d+)\)', value)
    return int(match.group(1)) if match else None

# Extract numerical scores for each feedback column
for col in facilitator_feedback_columns:
    data_df[col + '_num'] = data_df[col].apply(extract_score)

# Columns with the extracted numerical scores
facilitator_feedback_columns_num = [col + '_num' for col in facilitator_feedback_columns]

# Exclude rows with missing numerical scores
filtered_data_df_num = data_df.dropna(subset=facilitator_feedback_columns_num)

# Group the filtered data by facilitator names and calculate the mean for each numerical feedback column
grouped_by_facilitator_num = filtered_data_df_num.groupby('facilitator')[facilitator_feedback_columns_num].mean()

# Calculate the overall mean score for each facilitator in the filtered data
grouped_by_facilitator_num['overall_mean_score'] = grouped_by_facilitator_num.mean(axis=1)

# Sort the facilitators by their overall mean score in descending order
sorted_facilitators_num = grouped_by_facilitator_num.sort_values(by='overall_mean_score', ascending=False)

sorted_facilitators_num.head()
