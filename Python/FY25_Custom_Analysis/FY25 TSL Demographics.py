### import necessary libraries ###
### pip or conda install libraries if not installed already ###

#dataframe imports
import pandas as pd
import numpy as np
import datetime
from datetime import date
#Qualtrics API imports
from QualtricsAPI.Setup import Credentials
from QualtricsAPI.Survey import Responses
import warnings
warnings.filterwarnings("ignore") #ignore warning from Qualtrics download

def get_survey(qcode):
        #Credentials from Teaching Lab individual account
        # use TL Qualtrics account to generate token and retrieve data center credentials
        Credentials().qualtrics_api_credentials(token= MY_TOKEN,data_center= TL_DATA_CENTER)
        #Create an instance
        r = Responses()
        #creating df from survey code, turn off recode values
        df = r.get_survey_responses(survey= qcode, useLabels=True)
        return df

### Clean survey data
def clean(data):
    """Cleans Qualtrics survey data."""
    # Drop metadata rows
    data = data.iloc[2:].copy()
    
    # Convert EndDate to datetime
    data['EndDate'] = pd.to_datetime(data['EndDate'])
    
    # Normalize email addresses
    data['email'] = data['email'].str.lower().str.replace(r"[.-]", "", regex=True)
    # Drop rows where email is NaN before filtering
    data = data.dropna(subset=['email'])
    # Filter emails that end with 'us' or 'org' - excludes gmail and typos
    data = data[data['email'].str.endswith(('us', 'org'), na=False)]
    
    # Filter completed surveys after Oct 1st, 2024, or desired date
    return data.query("EndDate >= '2024-10-01' and Finished in ['True', True]").reset_index(drop=True)


### Process racial identity data
def race_col(df):
    """Prepares racial identity data to be used in TL reporting."""
    
    # Select race-related columns
    race_cols = ['race_1', 'race_2', 'race_3', 'race_4', 'race_5', 'race_6', 
                 'race_7', 'race_7_TEXT', 'race_8', 'race_9']
    check_cols = ['race_1', 'race_2', 'race_3', 'race_4', 'race_5', 'race_6', 
                 'race_7', 'race_8', 'race_9']
    
    # Compute race count - to be used later
    df['race_count'] = df[check_cols].notna().sum(axis=1)
    # Ensure columns are treated as strings
    df[race_cols] = df[race_cols].astype(str)
    
    # Define race mapping for single selections
    race_mapping = {
        'race_1': 'Asian',
        'race_2': 'Black or African American',
        'race_3': 'Native American or Indian',
        'race_4': 'Native Hawaiian or Pacific Islander',
        'race_5': 'White',
        'race_6': 'Prefer not to say',
        'race_7': 'I prefer to self describe',
        'race_8': 'Hispanic/Latino',
        'race_9': 'Middle Eastern or North African'
    }

    def determine_race(row):
        """Assigns race category based on responses."""
        if row['race_count'] > 1:
            return 'More than one race'
        
        # Identify the first selected race
        else:
            for col, race_label in race_mapping.items():
                if race_label in row[col]:
                    return race_label
    
    # Apply function to determine race
    df['Race'] = df.apply(determine_race, axis=1)
    
    return df

### Coalesce survey variables with embedded __js variables
def var_coalesce(data):
    """Combines primary and embedded demographic variables."""
    return data.assign(
        site_co=data['site'].combine_first(data['__js_site_api']),
        gender_co=data['gender'].combine_first(data['__js_gender_api']),
        race_co=data['Race'].combine_first(data['__js_race_api']),
        role_co=data['role'].combine_first(data['__js_role_api']),
        content_co=data['content_area'].combine_first(data['__js_content_area_api'])
    )


### Extract first instance of demographic data per participant
def tsl_dems(data, emails):
    """Returns the first instance of demographic data for each TSL participant."""
    
    def get_first(series):
        """Retrieves the first non-null, non-empty value in a series."""
        return next((val for val in series if isinstance(val, str) and val.strip()), "No Data Yet")

    return pd.DataFrame([
        {
            "Participant": email,
            "Site": get_first(data.loc[data["email"] == email, "site_co"]),
            "Gender": get_first(data.loc[data["email"] == email, "gender_co"]),
            "Race": get_first(data.loc[data["email"] == email, "race_co"]),
            "Role": get_first(data.loc[data["email"] == email, "role_co"]),
            "Content Area": get_first(data.loc[data["email"] == email, "content_co"])
        }
        for email in emails
    ])


### Retrieve and process survey data
def process_survey(survey_id):
    """Fetches, cleans, and processes survey data for TSL sites."""
    
    df = var_coalesce(race_col(clean(get_survey(survey_id))))
    
    tsl_sites = [
        "WI_Milwaukee Public Schools",
        "TX_El Paso Leadership Academy",
        "MS_Kemper County School District",
        "AR_Osceola School District"
    ]
    
    # Filter only TSL sites
    tsl_df = df[df['site_co'].isin(tsl_sites)]
    
    # Extract unique participant emails
    emails = tsl_df['email'].unique().tolist()
    
    return tsl_dems(tsl_df, emails)

def merge_processing(pf, ed):
    """Combines pf and ed data frames, keeping the first valid value per column (excluding 'No Data Yet')."""
    
    # Merge both dataframes on Participant (email), keeping all unique participants
    combined_df = pd.concat([pf, ed]).drop_duplicates(subset=['Participant']).reset_index(drop=True)

    # Function to get the first valid value (excluding 'No Data Yet')
    def get_first_valid(series):
        """Returns the first non-'No Data Yet' value, otherwise 'No Data Yet'."""
        for val in series:
            if val != "No Data Yet":
                return val
        return "No Data Yet"

    # Apply function across each column for grouped emails
    final_df = combined_df.groupby("Participant").agg(lambda x: get_first_valid(x)).reset_index()

    return final_df
  
### set up distribution calculations based on desired demographic column
def distributions(demographic):
    sites = final_df.Site.unique().tolist()
    sites = sites + ['Overall Count']

    # Compute the race distribution counts by site
    dem_count_by_site = (
        final_df.groupby("Site")[demographic]
        .value_counts()
        .unstack(fill_value=0)
    )

    # Compute the overall race count across all sites
    dem_count_overall = (
        final_df[demographic]
        .value_counts()
        .to_frame(name="Overall Count")
    )

    def nsize():
        result=[]
        for site in sites:
            s = site
            if site =='Overall Count':
                n = final_df[demographic].count()
            else:
                site_df = final_df[final_df.Site==site]
                n = site_df[demographic].count()
            result.append({'Site':s,
                          'N size':n})
             # Convert results to DataFrame
        results_df = pd.DataFrame(result)
        return results_df

    n = nsize()

    # Compute the race distribution by site as percentages
    dem_distribution_by_site = (
        dem_count_by_site.div(dem_count_by_site.sum(axis=1), axis=0) * 100
    ).applymap(lambda x: f"{x:.2f}%")

    # Compute the overall race distribution across all sites as percentages
    dem_distribution_overall = (
        (dem_count_overall / dem_count_overall.sum()) * 100
    ).applymap(lambda x: f"{x:.2f}%")

    # Combine counts with percentage distributions and transpose
    final_dist = dem_distribution_overall.join(dem_distribution_by_site.T).T

    # Ensure the "Site" column is set as the index before merging with `n`
    n.set_index("Site", inplace=True)

    # Join the N-size column
    final = final_dist.join(n)

    return final

### Run cleaning and processing for both surveys
pf = process_survey('SV_djt8w6zgigaNq0C')
ed = process_survey('SV_8vrKtPDtqQFbiBM')
# Merge and process the final data frame
final_df = merge_processing(pf, ed)
# calculate distributions for necessary variables
race = distributions('Race')
gender = distributions('Gender')
