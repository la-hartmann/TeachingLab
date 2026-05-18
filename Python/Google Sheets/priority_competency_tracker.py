import gspread 
import oauth2client
import pandas as pd
from datetime import date
from datetime import datetime
import datetime
import requests
import numpy as np

# Load your credentials and authenticate
def authenticate_google_sheets(json_keyfile):
    scope = [
        "https://spreadsheets.google.com/feeds",
        "https://www.googleapis.com/auth/drive",
    ]
    credentials = ServiceAccountCredentials.from_json_keyfile_name(json_keyfile, scope)
    gc = gspread.authorize(credentials)
    return gc

# Open the Google Sheet to write to
def open_google_sheet(gc, spreadsheet_name_or_id):
    try:
        sheet = gc.open_by_key(spreadsheet_name_or_id)  # Use the Google Sheet ID
    except gspread.exceptions.SpreadsheetNotFound:
        sheet = gc.open(spreadsheet_name_or_id)  # Use the Google Sheet Name (if ID not provided)
    return sheet

# Write data to the Google Sheet
def write_to_sheet(sheet, worksheet_name, data):
    # Ensure worksheet exists
    try:
        worksheet = sheet.worksheet(worksheet_name)
    except gspread.exceptions.WorksheetNotFound:
        worksheet = sheet.add_worksheet(title=worksheet_name, rows="100", cols="20")

    # Clear existing data and update with new data
    worksheet.clear()
    worksheet.update([data.columns.values.tolist()] + data.values.tolist())

#Open google sheet to read
def read_sheet_to_dataframe(gc, spreadsheet_name_or_id, worksheet_name):
    # Open the Google Sheet
    try:
        sheet = gc.open_by_key(spreadsheet_name_or_id)  # Use ID if provided
    except gspread.exceptions.SpreadsheetNotFound:
        sheet = gc.open(spreadsheet_name_or_id)  # Use name if ID not provided

    # Open the worksheet
    worksheet = sheet.worksheet(worksheet_name)

    # Get all values from the worksheet
    data = worksheet.get_all_values()

    # Convert to Pandas DataFrame
    if data:
        df = pd.DataFrame(data[1:], columns=data[0])  # First row as header
    else:
        df = pd.DataFrame()  # Empty DataFrame if no data

    return df


def get_priorities():
  #def get_priorities():
    url = "https://api.monday.com/v2"
    headers = {
        "Authorization":MONDAY_AUTH,
        "Content-Type": "application/json"
    }

    # GraphQL query to pull coach names and email
    query = """{
      boards(ids: BOARD_ID) {
      name
      id
      description
      items_page(limit:500) {
        items {
        name
        column_values(ids: ["text60"]){
          id
          text
    } } } } }"""

    # Note for future reference once the Monday board is correctly mapped:
    #"priority9__1" - competency columns
    # once correctly mapped, I do not need to pull from the second board


    response = requests.post(url, headers=headers, json={'query': query})

    # Parse the response
    data = response.json()

    # Extract items from the response
    items = data['data']['boards'][0]['items_page']['items']
    item_names = [item['name'] for item in items]
    column_values = [{col['id']: col['text'] for col in item['column_values']} for item in items]

    # Create DataFrame
    coach = pd.DataFrame(column_values)
    coach['Item Name'] = item_names


    # Rename columns
    coach = coach.rename(columns={'text60': 'email','Item Name':'Coach'})

    # Reorder the columns 
    contact = pd.DataFrame(coach[['email', 'Coach']])
    # Zip to dictionary to map second monday board
    ref =  dict(zip(contact.email, contact.Coach))


    ### Pulling Competencies#######
    # GraphQL query to pull email and competencies
    query = """{
      boards(ids: BOARD2_ID) {
      name
      id
      description
      items_page(limit:500) {
        items {
        name
        column_values(ids: ["email__1", "multi_select__1","dropdown__1"]){
          id
          text
    } } } } }"""


    response = requests.post(url, headers=headers, json={'query': query})

    # Parse the response
    data = response.json()


    # Extract items from the response
    items = data['data']['boards'][0]['items_page']['items']
    item_names = [item['name'] for item in items]
    column_values = [{col['id']: col['text'] for col in item['column_values']} for item in items]

    # Create a pandas DataFrame
    comps = pd.DataFrame(column_values)
    comps['Item Name'] = item_names


    # Rename 
    comps = comps.rename(columns={'email__1': 'email','multi_select__1':'proj_comp','dropdown__1':'priorities'})

    # Reorder
    comps = pd.DataFrame(comps[['email', 'proj_comp', 'priorities']])
    #separate data into goal3, goal4, and goal 5 columns

    comps[['goal4', 'goal5']] = comps['priorities'].str.split(',', expand=True)
    # Drop the original 'priorities' column (optional)
    comps.drop(columns=['priorities'], inplace=True)
    comps['goal5'] = comps['goal5'].str.lstrip()
    comps['goal5'] = comps['goal5'].str.lstrip()
    # Display the DataFrame
    comps['Coach']= comps['email'].map(ref)
    comps= comps.replace({'Coach':{np.nan:'Not a coach'}})
    
    return comps

def get_survey(qcode):
    from QualtricsAPI.Setup import Credentials
    from QualtricsAPI.Survey import Responses
    #Credentials from Teaching Lab Research account
    Credentials().qualtrics_api_credentials(token='xNp32So8ETDM15Ug2jOQLLLRzbi8CWWVCeXP8MkV',data_center='teachinglab.iad1')

    #Create an instance
    r = Responses()
    #creating df from survey code, without recode values
    df = r.get_survey_responses(survey= qcode, useLabels=True)
    return df

# Clean the data
def clean(data):
    #drop the import label and question rows
    data.drop(index=data.index[:2], inplace=True)
    #change the data type to date instead of string 
    data['selected_date']=pd.to_datetime(data['selected_date'],format='mixed')
    #subset the data to only include Finished surveys completed after July 1st (new school year)
    df = pd.DataFrame(data[(data['selected_date']>='07-01-2024')])
    df = pd.DataFrame(df[(df['Finished']==True)|(df.Finished=="True")])
    df = df.reset_index()
    df = df.drop(columns=['index'])
    return df

def check_comps():
    comps = get_priorities()

    df = clean(get_survey('SV_8ldVeywLkOMgugK'))   

    column_mapping = {
        'head_1': 'Teaching Lab Knowledge',
        'head_2': 'Pedagogical Content Knowledge',
        'head_3': 'Professional Learning & Coaching Research Knowledge',
        'head_4': 'HQIM Knowledge',
        'heart_1': 'Inclusive',
        'heart_2': 'Motivation',
        'heart_3': 'Responsive',
        'heart_4': 'Authentic',
        'heart_5': 'Collaborative',
        'heart_6': 'Engaging',
        'habits_1': 'Preparation',
        'habits_2': 'Integrity',
        'habits_3': 'Reflection',
        'habits_4': 'Coaching',
        'equity_1': 'Educational Equity',
        'equity_2': 'Asset Based Language'
    }

    # Reverse the dictionary for easier lookup
    reverse_mapping = {v: k for k, v in column_mapping.items()}

    # Initialize result lists
    results = []

    # Iterate through each coach in df2
    for index, row in comps.iterrows():
        coach = row['Coach']
        coach_obs = df[df['facilitator'] == coach]  # Get all observations for the coach

       # Check proj_comp, goal4, goal5
        proj_list = []
        goal4_list = []
        goal5_list = []

         # Dictionary mapping df2 columns to their corresponding column in df1
        comps_to_qualtrics = {
            'proj_comp': reverse_mapping.get(row['proj_comp']),
            'goal4': reverse_mapping.get(row['goal4']),
            'goal5': reverse_mapping.get(row['goal5'])
        }


        # Check TL Competency (head_1, equity_2)
        tl_competency_list = []

        def is_valid(value):
            # Ensure the value is a string before processing
            if isinstance(value, str):
                return value.strip().lower() not in ["", "not applicable"]
            return False  # Non-string values are treated as invalid

        # Apply the check to head_1 and equity_2 columns
        proj_valid = coach_obs[comps_to_qualtrics['proj_comp']].apply(is_valid).any()
        goal4_valid = coach_obs[comps_to_qualtrics['goal4']].apply(is_valid).any()
        goal5_valid = coach_obs[comps_to_qualtrics['goal5']].apply(is_valid).any()
        head_1_valid = coach_obs['head_1'].apply(is_valid).any()
        equity_2_valid = coach_obs['equity_2'].apply(is_valid).any()

        if proj_valid:
            proj_list = 'Yes'
        elif not proj_valid:
            proj_list = 'No'
        
        if goal4_valid:
            goal4_list = 'Yes'
        elif not goal4_valid:
            goal4_list = 'No'
            
        if goal5_valid:
            goal5_list = 'Yes'
        elif not goal5_valid:
            goal5_list = 'No'
        
        # Determine TL competency status
        if head_1_valid and equity_2_valid:
            tl_competency_list = "Both observed"
        elif not head_1_valid and not equity_2_valid:
            tl_competency_list = "Missing both"
        elif not head_1_valid:
            tl_competency_list = "Missing TL Knowledge"
        elif not equity_2_valid:
            tl_competency_list = "Missing Asset Based"

        # Append results for this coach
        results.append({
            'Coach': coach,
            'TL Competencies': tl_competency_list,
            'Project Competency': row['proj_comp'],
            'Project Competency Rated': proj_list,
            'Competency 4': row['goal4'],
            'Competency 4 Rated': goal4_list,
            'Competency 5': row['goal5'],
            'Competency 5 Rated': goal5_list
        })

    # Convert results to a DataFrame
    results_df = pd.DataFrame(results)

    def needs_new_baseline(row):
        # Check if proj_comp_list, goal4_list, and goal5_list are all True
        all_lists_true = (
        row['Project Competency Rated'] == "Yes" and
        row['Competency 4 Rated'] == "Yes" and
        row['Competency 5 Rated'] == "Yes")
        
        # Check if TL Competency is "Both observed"
        tl_competency_observed = row['TL Competencies'] == "Both observed"
        # Determine Needs New Baseline value
        if all_lists_true and tl_competency_observed:
            return "No"
        else:
            return "Yes"

    # Apply the function to each row in the DataFrame
    results_df['Needs New Baseline'] = results_df.apply(needs_new_baseline, axis=1)
    results_df = results_df[['Coach','Needs New Baseline' ,'TL Competencies', 'Project Competency',
       'Project Competency Rated', 'Competency 4', 'Competency 4 Rated',
       'Competency 5', 'Competency 5 Rated']]
    results_df = results_df.sort_values(by=['Needs New Baseline','Coach'])
    update_time = datetime.datetime.now() + datetime.timedelta(hours=1)
    update_row = pd.DataFrame({
        "Coach": ["Last updated: " + update_time.strftime("%m-%d-%Y %I:%M %p")+" ET"],
        "TL Competencies": [""],
        "Project Competency": [""],
        "Project Competency Rated": [""],
        "Competency 4": [""],
        "Competency 4 Rated": [""],
        "Competency 5": [""],
        "Competency 5 Rated": [""],
        "Needs New Baseline": [""]
        })

    final = pd.concat([results_df, update_row], ignore_index=False)

    # Display the results
    return final

# Main code to authenticate, prepare data, and write to sheet
def main():
    
    #Set up writing to sheet    
    json_keyfile = json_keyfile_path
    spreadsheet_write = SPREADSHEET_ID
    worksheet_write = "Baseline Data Check"

    # Authenticate and open the sheets to read and write
    gc = authenticate_google_sheets(json_keyfile)
    sheet = open_google_sheet(gc, spreadsheet_write)

    # Example data (replace with your dynamic data)
    checks = check_comps()


    # Write to the worksheet
    write_to_sheet(sheet, worksheet_write, checks)

if __name__ == "__main__":
    main()
