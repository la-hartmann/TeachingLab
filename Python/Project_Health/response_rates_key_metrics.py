## imports
import os.path
import gspread 
import oauth2client
from oauth2client.service_account import ServiceAccountCredentials
from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from QualtricsAPI.Setup import Credentials
from QualtricsAPI.Survey import Responses
import boto3
from io import BytesIO
import pandas as pd
from datetime import date
from datetime import datetime
import numpy as np
import math 
from zoneinfo import ZoneInfo
from config import (
    AWS_ACCESS_KEY_ID,
    AWS_SECRET_ACCESS_KEY,
    AWS_DEFAULT_REGION,
    RRKO_SHEET_ID,
    GOOGLE_SHEETS_CREDS,
    QUALTRICS_SW_ID,
    QUALTRICS_CLIENT,
    QUALTRICS_TOKEN,
)
#### data load / data write ####
### AWS parquet files
def read_parquet_from_s3(bucket_name, key, aws_access_key_id=None, aws_secret_access_key=None, region_name=None):
    # Initialize an S3 session
    session = boto3.Session(
        aws_access_key_id=aws_access_key_id,
        aws_secret_access_key=aws_secret_access_key,
        region_name=region_name
    )
    s3_client = session.client('s3')
    
    # Retrieve the parquet file from S3
    response = s3_client.get_object(Bucket=bucket_name, Key=key)
    data = response['Body'].read()
    
    # Convert the binary data into a pandas DataFrame
    df = pd.read_parquet(BytesIO(data), engine='pyarrow')  # You can switch to 'fastparquet' if you prefer
    
    return df

def get_survey(qcode):
        #Credentials TL Qualtrics
        Credentials().qualtrics_api_credentials(token=QUALTRICS_TOKEN,data_center=QUALTRICS_CLIENT)
        #Create an instance
        r = Responses()
        #creating df from survey code, without recode values
        df = r.get_survey_responses(survey= qcode, useLabels=True)
        df.drop(index=df.index[:2],inplace=True)
        df['RecordedDate']=pd.to_datetime(df.RecordedDate)
        df = pd.DataFrame(df[(df.RecordedDate>='07-01-2025')])
        df = pd.DataFrame(df[(df.Finished==True)|(df.Finished=='True')])
        df = df.reset_index()
        df = df.drop(columns =['index'])
        return df

### Google Sheets

# Load credentials and authenticate
def authenticate_google_sheets(creds_dict=None):
    scope = [
        "https://spreadsheets.google.com/feeds",
        "https://www.googleapis.com/auth/drive",
    ]
    
    # default to config.GOOGLE_SHEETS_CREDS
    if creds_dict is None:
        creds_dict = GOOGLE_SHEETS_CREDS
    
    credentials = ServiceAccountCredentials.from_json_keyfile_dict(creds_dict, scope)
    gc = gspread.authorize(credentials)
    return gc

# Open the Google Sheet to write to
def open_google_sheet(gc, spreadsheet_name_or_id):
    try:
        sheet = gc.open_by_key(spreadsheet_name_or_id)  # Use the Google Sheet ID
    except gspread.exceptions.SpreadsheetNotFound:
        sheet = gc.open(spreadsheet_name_or_id)  # Use the Google Sheet Name 
    return sheet

# Write data to the Google Sheet
def write_to_sheet(sheet, worksheet_name, data):
    # Ensure worksheet exists
    try:
        worksheet = sheet.worksheet(worksheet_name)
    except gspread.exceptions.WorksheetNotFound:
        worksheet = sheet.add_worksheet(title=worksheet_name, rows="100", cols="35")

    # Clear existing data and update with new data
    worksheet.clear()
    worksheet.update([data.columns.values.tolist()] + data.values.tolist())

#### Site reassign for NY - source dependent ####
def site_assign(df: pd.DataFrame) -> pd.Series:
    # NYC districts and Pre-K subset
    nyc = {"NY_D9", "NY_D11", "NY_D12", "NY_D13", "NY_D25", "NY_D75"}
    prek_dists = {"NY_D12", "NY_D13", "NY_D25"}

    site = df["site"].astype(str)
    grade = df["grade_level"].fillna("").astype(str)
    content = df["content_area"].fillna("").astype(str)

    is_nyc = site.isin(nyc)
    is_prek_dist = site.isin(prek_dists)

    is_prek_grade = grade == "Pre-K"
    is_prek_lit = content.str.contains("Pre-K Early Literacy", na=False)
    is_math = content.str.contains("Math", na=False)
    is_elaish = (
        content.str.contains("ELA", na=False) |
        content.str.contains("K-2 Early Literacy", na=False)
    )

    # start with default: just the site (non-NYC or unmatched cases)
    new = site.copy()

    # NYC + Pre-K districts
    mask_prek_ela = is_nyc & is_prek_dist & (is_prek_grade | is_prek_lit)
    new[mask_prek_ela] = site[mask_prek_ela] + " Pre-K Early Literacy"

    mask_prek_math = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &          # not already classified as ELA Pre-K
        is_math &
        (site != "NY_D25")
    )
    new[mask_prek_math] = site[mask_prek_math] + " Math"

    mask_prek_else = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &
        (~is_math | (site=="NY_D25"))
    )
    new[mask_prek_else] = site[mask_prek_else] + " ELA"

    # NYC non-Pre-K districts
    mask_nyc_math = is_nyc & ~is_prek_dist & is_math
    new[mask_nyc_math] = site[mask_nyc_math] + " Math"

    mask_nyc_else = is_nyc & ~is_prek_dist & ~is_math
    new[mask_nyc_else] = site[mask_nyc_else] + " ELA"

    return new

def site_assign_swg(df: pd.DataFrame) -> pd.Series:
    # NYC districts and Pre-K subset
    nyc = {"NY_D9", "NY_D11", "NY_D12", "NY_D13", "NY_D25", "NY_D75"}
    prek_dists = {"NY_D12", "NY_D13", "NY_D25"}

    site = df["Site"].astype(str)
    grade = df["Grade Band"].fillna("").astype(str)
    content = df["Subject Area"].fillna("").astype(str)

    is_nyc = site.isin(nyc)
    is_prek_dist = site.isin(prek_dists)

    is_prek_grade = grade == "Pre-K"
    is_prek_lit = content.str.contains("Pre-K Early Literacy", na=False)
    is_math = content.str.contains("Math", na=False)
    is_elaish = (
        content.str.contains("ELA", na=False) |
        content.str.contains("K-2 Early Literacy", na=False)
    )

    # start with default: just the site (non-NYC or unmatched cases)
    new = site.copy()

    # NYC + Pre-K districts
    mask_prek_ela = is_nyc & is_prek_dist & (is_prek_grade | is_prek_lit)
    new[mask_prek_ela] = site[mask_prek_ela] + " Pre-K Early Literacy"

    mask_prek_math = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &          # not already classified as ELA Pre-K
        is_math &
        (site != "NY_D25")
    )
    new[mask_prek_math] = site[mask_prek_math] + " Math"

    mask_prek_else = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &
        (~is_math | (site=="NY_D25"))
    )
    new[mask_prek_else] = site[mask_prek_else] + " ELA"

    # NYC non-Pre-K districts
    mask_nyc_math = is_nyc & ~is_prek_dist & is_math
    new[mask_nyc_math] = site[mask_nyc_math] + " Math"

    mask_nyc_else = is_nyc & ~is_prek_dist & ~is_math
    new[mask_nyc_else] = site[mask_nyc_else] + " ELA"

    return new

def sw_site(df: pd.DataFrame) -> pd.Series:
    # NYC districts and Pre-K subset
    nyc = {"NY_D9", "NY_D11", "NY_D12", "NY_D13", "NY_D25", "NY_D75"}
    prek_dists = {"NY_D12", "NY_D13", "NY_D25"}

    site = df["site"].astype(str)
    grade = df["grade_level_1"].fillna("").astype(str)
    content = df["content_area"].fillna("").astype(str)

    is_nyc = site.isin(nyc)
    is_prek_dist = site.isin(prek_dists)

    is_prek_grade = grade == "Pre-K"
    is_prek_lit = content.str.contains("Pre-K Early Literacy", na=False)
    is_math = content.str.contains("Math", na=False)
    is_elaish = (
        content.str.contains("ELA", na=False) |
        content.str.contains("K-2 Early Literacy", na=False)
    )

    # start with default: just the site (non-NYC or unmatched cases)
    new = site.copy()

    # NYC + Pre-K districts
    mask_prek_ela = is_nyc & is_prek_dist & (is_prek_grade | is_prek_lit)
    new[mask_prek_ela] = site[mask_prek_ela] + " Pre-K Early Literacy"

    mask_prek_math = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &          # not already classified as ELA Pre-K
        is_math &
        (site != "NY_D25")
    )
    new[mask_prek_math] = site[mask_prek_math] + " Math"

    mask_prek_else = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &
        (~is_math | (site=="NY_D25"))
    )
    new[mask_prek_else] = site[mask_prek_else] + " ELA"

    # NYC non-Pre-K districts
    mask_nyc_math = is_nyc & ~is_prek_dist & is_math
    new[mask_nyc_math] = site[mask_nyc_math] + " Math"

    mask_nyc_else = is_nyc & ~is_prek_dist & ~is_math
    new[mask_nyc_else] = site[mask_nyc_else] + " ELA"

    return new

def site_assign_cl(df: pd.DataFrame) -> pd.Series:
    # NYC districts and Pre-K subset
    nyc = {"NY_D9", "NY_D11", "NY_D12", "NY_D13", "NY_D25", "NY_D75"}
    prek_dists = {"NY_D12", "NY_D13", "NY_D25"}

    site = df["site"].astype(str)
    content = df["content_area"].fillna("").astype(str)

    is_nyc = site.isin(nyc)
    is_prek_dist = site.isin(prek_dists)

    is_prek_lit = content.str.contains("Pre-K Early Literacy", na=False)
    is_math = content.str.contains("Math", na=False)
    is_elaish = (
        content.str.contains("ELA", na=False) |
        content.str.contains("K-2 Early Literacy", na=False)
    )

    # start with default: just the site (non-NYC or unmatched cases)
    new = site.copy()

    # NYC + Pre-K districts
    mask_prek_ela = is_nyc & is_prek_dist & (is_prek_lit)
    new[mask_prek_ela] = site[mask_prek_ela] + " Pre-K Early Literacy"

    mask_prek_math = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &          # not already classified as ELA Pre-K
        is_math &
        (site != "NY_D25")
    )
    new[mask_prek_math] = site[mask_prek_math] + " Math"

    mask_prek_else = (
        is_nyc & is_prek_dist &
        ~mask_prek_ela &
        (~is_math | (site=="NY_D25"))
    )
    new[mask_prek_else] = site[mask_prek_else] + " ELA"

    # NYC non-Pre-K districts
    mask_nyc_math = is_nyc & ~is_prek_dist & is_math
    new[mask_nyc_math] = site[mask_nyc_math] + " Math"

    mask_nyc_else = is_nyc & ~is_prek_dist & ~is_math
    new[mask_nyc_else] = site[mask_nyc_else] + " ELA"

    return new

#### Scoring & processing key metrics ####
def ipg_scoring(df: pd.DataFrame, timing: str) -> pd.DataFrame:
    df = df.copy()

    # Keep one unique teacher record
    # per site based on timing
    
    df["selected_date"] = pd.to_datetime(df["selected_date"], errors="coerce")

    # Drop rows missing needed fields for selection
    df = df.dropna(subset=["teacher_select", "new_site", "selected_date"]).copy()

    if timing == "diagnostic":
        # earliest scored submission per teacher per site
        df = (
            df.sort_values("selected_date", ascending=True)
              .drop_duplicates(subset=["new_site", "teacher_select"], keep="first")
              .copy()
        )
    elif timing == "second_round":
        # latest scored submission per teacher per site
        df = (
            df.sort_values("selected_date", ascending=True)
              .drop_duplicates(subset=["new_site", "teacher_select"], keep="last")
              .copy()
        )
    else:
        raise ValueError("timing must be 'diagnostic' or 'second_round'")

    # ---- Column definitions ----
    columns_pos = [
        'k12_m_ca1a', 'k12_m_ca1b', 'k12_m_ca1c', 'k12_m_ca2a',
        'k12_m_ca2b', 'k12_m_ca2c', 'k12_m_ca2d',
        'k12_m_ca3a', 'k12_m_ca3b', 'k12_m_ca3c', 'k12_m_ca3d', 'k12_m_ca3e',
        'k12_ela_ca1a', 'k12_ela_ca1b', 'k12_ela_ca1c',
        'k12_ela_ca2a', 'k12_ela_ca2b', 'k12_ela_ca2c', 'k12_ela_ca2d',
        'k12_ela_ca3a', 'k12_ela_ca3b', 'k12_ela_ca3c',
        'k12_ela_ca3d', 'k12_ela_ca3e', 'k12_ela_ca3f',
        'sci_ca1a', 'sci_ca1b', 'sci_ca1c',
        'sci_ca2a', 'sci_ca2b', 'sci_ca2c', 'sci_ca2d',
        'sci_ca3a', 'sci_ca3b', 'sci_ca3c', 'sci_ca3d',
        'ss_ca1a', 'ss_ca1b', 'ss_ca1c',
        'ss_ca2a', 'ss_ca2b', 'ss_ca2c', 'ss_ca2d',
        'ss_ca3a', 'ss_ca3b', 'ss_ca3c', 'ss_ca3d',
        'fsot_ac1','fsot_ac2','fsot_td1', 'fsot_td2', 'fsot_td3', 'fsot_td4',
        'fsot_sp1', 'fsot_sp2', 'fsot_sp3', 'fsot_sp4'
    ]

    columns_2 = ['fsot_ad1', 'fsot_ad2']

    columns_prek = [
        'prek_lang_1', 'prek_lang_2', 'prek_lang_3', 'prek_lang_4', 'prek_lang_5',
        'prek_lang_6', 'prek_lang_7', 'prek_lang_8', 'prek_lang_9',
        'prek_lang_10','prek_lit_1', 'prek_lit_2',
        'prek_lit_3', 'prek_lit_4', 'prek_lit_5', 'prek_lit_6', 'prek_lit_7',
        'prek_lit_8', 'prek_lit_9'
    ]

    columns = columns_pos + columns_2 + columns_prek

    rubric_columns = {
        'K-12: Mathematics IPG': [
            'k12_m_ca1a', 'k12_m_ca1b', 'k12_m_ca1c', 'k12_m_ca2a',
            'k12_m_ca2b', 'k12_m_ca2c', 'k12_m_ca2d',
            'k12_m_ca3a', 'k12_m_ca3b', 'k12_m_ca3c', 'k12_m_ca3d', 'k12_m_ca3e'
        ],
        'K-12: ELA/Literacy IPG (please use this tool for K-2 observations that are not focused on foundational skills)': [
            'k12_ela_ca1a', 'k12_ela_ca1b', 'k12_ela_ca1c',
            'k12_ela_ca2a', 'k12_ela_ca2b', 'k12_ela_ca2c', 'k12_ela_ca2d',
            'k12_ela_ca3a', 'k12_ela_ca3b', 'k12_ela_ca3c',
            'k12_ela_ca3d', 'k12_ela_ca3e', 'k12_ela_ca3f'
        ],
        'Foundational Skills Observational Tool - FSOT': [
            'fsot_ac1','fsot_ac2',
            'fsot_td1', 'fsot_td2', 'fsot_td3', 'fsot_td4',
            'fsot_sp1', 'fsot_sp2', 'fsot_sp3', 'fsot_sp4',
            'fsot_ad1', 'fsot_ad2'
        ],
        '6-12: Science & Technical Subjects IPG': [
            'sci_ca1a', 'sci_ca1b', 'sci_ca1c',
            'sci_ca2a', 'sci_ca2b', 'sci_ca2c', 'sci_ca2d',
            'sci_ca3a', 'sci_ca3b', 'sci_ca3c', 'sci_ca3d'
        ],
        '6-12: History/Social Studies IPG': [
            'ss_ca1a', 'ss_ca1b', 'ss_ca1c',
            'ss_ca2a', 'ss_ca2b', 'ss_ca2c', 'ss_ca2d',
            'ss_ca3a', 'ss_ca3b', 'ss_ca3c', 'ss_ca3d'
        ],
        'Pre-K MTSS Language and Literacy Classroom Walkthrough Tool': [
            'prek_lang_1', 'prek_lang_2', 'prek_lang_3', 'prek_lang_4', 'prek_lang_5',
            'prek_lang_6', 'prek_lang_7', 'prek_lang_8', 'prek_lang_9',
            'prek_lang_10','prek_lit_1', 'prek_lit_2',
            'prek_lit_3', 'prek_lit_4', 'prek_lit_5', 'prek_lit_6', 'prek_lit_7',
            'prek_lit_8', 'prek_lit_9'
        ]
    }

    # Ensure all scoring columns exist
    existing_cols_pos = [c for c in columns_pos if c in df.columns]
    existing_cols_2 = [c for c in columns_2 if c in df.columns]
    existing_cols_prek = [c for c in columns_prek if c in df.columns]
    existing_cols_all = [c for c in columns if c in df.columns]

    block_all = df[existing_cols_all].astype("string")

    # ---- count_yes_3_4 ----
    block_pos = block_all[existing_cols_pos]
    mask_yes = block_pos.apply(lambda col: col.str.contains("Yes", na=False))
    mask_3_4 = block_pos.apply(lambda col: col.str.startswith(("3", "4"), na=False))
    df["count_yes_3_4"] = (mask_yes | mask_3_4).sum(axis=1)

    # ---- count_2 ----
    if existing_cols_2:
        block_2 = block_all[existing_cols_2]
        mask_2 = block_2.apply(lambda col: col.str.startswith("2", na=False))
        df["count_2"] = mask_2.sum(axis=1)
    else:
        df["count_2"] = 0

    # ---- count_prek ----
    if existing_cols_prek:
        block_prek = block_all[existing_cols_prek]
        mask_prek = block_prek.apply(
            lambda col: col.str.startswith(("Always", "Often"), na=False)
        )
        df["count_prek"] = mask_prek.sum(axis=1)
    else:
        df["count_prek"] = 0

    df["pos_ind"] = df["count_yes_3_4"] + df["count_2"] + df["count_prek"]

    # ---- count_NA ----
    if existing_cols_all:
        na_mask = block_all.apply(
            lambda col: col.str.contains("Not Applicable|Not Observed", na=False, regex=True)
        )
        df["count_NA"] = na_mask.sum(axis=1)
    else:
        df["count_NA"] = 0

    # ---- non_null: per rubric ----
    df["non_null"] = 0

    for rubric, cols in rubric_columns.items():
        cols_existing = [c for c in cols if c in df.columns]
        if not cols_existing:
            continue
        mask_rubric = df["ipg_rubric"] == rubric
        df.loc[mask_rubric, "non_null"] = (
            df.loc[mask_rubric, cols_existing].notna().sum(axis=1)
        )

    # ---- nsize & PPI ----
    df["nsize"] = df["non_null"] - df["count_NA"]
    df["ppi"] = np.where(
        df["nsize"] > 0,
        100 * df["pos_ind"] / df["nsize"],
        np.nan
    )

    # ---- Aggregate by site ----
    summary = (
        df.groupby("new_site", as_index=False)
          .agg(
              Submissions=("ppi", "size"),
              PPI=("ppi", lambda s: round(s.mean(), 2))
          )
          .rename(columns={"new_site": "Site"})
    )

    return summary



def pf_scoring(df):
    results = []
    scores = {
    np.nan: np.nan, '4 - Agree': 4, '5 - Strongly agree': 5,
    '3 - Neither agree nor disagree': 3, '2 - Disagree': 2,
    '1 - Strongly disagree': 1}

    facilitation_cols = [
    'coach_ongoing_feed_1', 'coach_ongoing_feed_2', 'coach_ongoing_feed_3', 
    'coach_ongoing_feed_4', 'coach_ongoing_feed_2_1', 
    'coach_ongoing_feed_2_2', 'coach_ongoing_feed_2_3', 'coach_ongoing_feed_2_4']
    
    for col in facilitation_cols:
        if col in df.columns:
            df[col] = df[col].replace(scores).astype(float)
    
    df['fac1']= round(100*(df[['coach_ongoing_feed_1', 'coach_ongoing_feed_2_1']].isin([4,5]).sum(axis=1))/(df[['coach_ongoing_feed_1', 'coach_ongoing_feed_2_1']].notna().sum(axis=1)),2)
    df['fac2']= round(100*(df[['coach_ongoing_feed_2', 'coach_ongoing_feed_2_2']].isin([4,5]).sum(axis=1))/(df[['coach_ongoing_feed_2', 'coach_ongoing_feed_2_2']].notna().sum(axis=1)),2)
    df['fac3']= round(100*(df[['coach_ongoing_feed_3', 'coach_ongoing_feed_2_3']].isin([4,5]).sum(axis=1))/(df[['coach_ongoing_feed_3', 'coach_ongoing_feed_2_3']].notna().sum(axis=1)),2)
    df['fac4']= round(100*(df[['coach_ongoing_feed_4', 'coach_ongoing_feed_2_4']].isin([4,5]).sum(axis=1))/(df[['coach_ongoing_feed_4', 'coach_ongoing_feed_2_4']].notna().sum(axis=1)),2)
    df['fac_score'] = df[facilitation_cols].mean(axis=1)
    
    session_cols = ['coach_end_feed_13','coach_end_feed_15','coach_end_feed_17','coach_end_feed_4']
    
    for col in session_cols:
        if col in df.columns:
            df[col]=df[col].replace(scores).astype(float)
            
    df['ses1']= round(100*(df[['coach_end_feed_13']].isin([4,5]).sum(axis=1))/(df[['coach_end_feed_13']].notna().sum(axis=1)),2)
    df['ses2']= round(100*(df[['coach_end_feed_17']].isin([4,5]).sum(axis=1))/(df[['coach_end_feed_17']].notna().sum(axis=1)),2)
    df['ses3']= round(100*(df[['coach_end_feed_4']].isin([4,5]).sum(axis=1))/(df[['coach_end_feed_4']].notna().sum(axis=1)),2)
    df['ses4']= round(100*(df[['coach_end_feed_15']].isin([4,5]).sum(axis=1))/(df[['coach_end_feed_15']].notna().sum(axis=1)),2)
    df['ses_score'] = df[session_cols].mean(axis=1)
    
    for site in df['new_site'].unique():
        df_site = df[df.new_site==site]
        n = len(df_site)
        
        teacher = df_site[df_site.role=='Teacher or specialist']
        tn = teacher['coach_nps_NPS_GROUP'].count()
        other = df_site[df_site.role!='Teacher or specialist']
        on = other['coach_nps_NPS_GROUP'].count()
        
        tprom = (teacher['coach_nps_NPS_GROUP'] == 'Promoter').sum()
        tdet = (teacher['coach_nps_NPS_GROUP'] == 'Detractor').sum()
        tnps = round(100 * (tprom - tdet) / tn, 2)
        
        oprom = (other['coach_nps_NPS_GROUP'] == 'Promoter').sum()
        odet = (other['coach_nps_NPS_GROUP'] == 'Detractor').sum()
        onps = round(100 * (oprom - odet) / on, 2)
        
        
        fac_avg = round(df_site['fac_score'].mean(),2)
        fac_1 = round(df_site['fac1'].mean(),2)
        fac_2 = round(df_site['fac2'].mean(),2)
        fac_3 = round(df_site['fac3'].mean(),2)
        fac_4 = round(df_site['fac4'].mean(),2)
        
        ses_avg = round(df_site['ses_score'].mean(),2)
        ses_1 = round(df_site['ses1'].mean(),2)
        ses_2 = round(df_site['ses2'].mean(),2)
        ses_3 = round(df_site['ses3'].mean(),2)
        ses_4 = round(df_site['ses4'].mean(),2)
        
        results.append({'Site':site,
                        'Submissions':n,
                       'Teacher NPS':tnps,
                       'Leader NPS':onps,
                       'Fac Avg':fac_avg,
                       'Clearly':fac_1,
                       'Encourage':fac_2,
                       'Foster':fac_3,
                       'Responsive':fac_4,
                       'Ses Avg':ses_avg,
                       'Quality':ses_1,
                       'rel T':ses_2,
                       'rel L':ses_3,
                       'Apply':ses_4})
    
        
    return pd.DataFrame(results)
    

def ed_scoring(df):
    columns = ['mindsets_ts_1_1','mindsets_ts_1_2','mindsets_ts_1_3',
     'mindsets_ts_1_4','mindsets_ts_1_5',
     'non_ts_mindsets_1','non_ts_mindsets_2','non_ts_mindsets_3',
              'non_ts_mindsets_4','non_ts_mindsets_5']

    reverse = ['mindsets_ts_1_1','mindsets_ts_1_2','mindsets_ts_1_3',
               'non_ts_mindsets_1','non_ts_mindsets_2','non_ts_mindsets_3']

    #dictionary to reassign values
    num = {'5 - Strongly agree':5, '4 - Agree':4, '3 - Neither agree nor disagree':3, '2 - Disagree':2, '1 - Strongly disagree':1 }

    #change text to numeric
    for col in columns:
        df = df.replace({col: num})
        df[col]=df[col].apply(pd.to_numeric)

    #reverse score required columns
    for i in reverse:  
        high = 6
        df[i]= (high-df[i]).mod(6)

    ### Creating Scored Dataframe ###
    #ed survey creating quintile scores

    scoring = {5:1, 4:0.75, 3:0.5, 2:0.25, 1:0, 0:0}
    qscores = []
    for i in columns:
        column = df[i]
        qscore = column.map(scoring)
        qscores.append(qscore)
    qscores = pd.DataFrame(qscores).transpose()

    # rename scored columns to add to df
    rename = {'mindsets_ts_1_1':'m1', 'mindsets_ts_1_2':'m2', 'mindsets_ts_1_3':'m3',
            'mindsets_ts_1_4':'m4', 'mindsets_ts_1_5':'m5','non_ts_mindsets_1':'nm1','non_ts_mindsets_2':'nm2',
              'non_ts_mindsets_3':'nm3','non_ts_mindsets_4':'nm4','non_ts_mindsets_5':'nm5'}

    qscores.rename(columns=rename, inplace=True)

    # add quintile scores to df
    df = pd.concat([df,qscores], axis=1)

    ### Calculating Mindset Scores ###

    # calculate the score for each individual overall and by construct category
    overall = (100*(df[['m1','m2','m3','m4','m5','nm1','nm2','nm3','nm4','nm5']].sum(axis=1))/5) 
    
    df['overall_score']=overall
    
    results=[]
    for site in df['new_site'].unique():
        df_site = df[df.new_site==site]
        n = len(df_site)
        
        t_mindset = round(df_site[(df_site.role=='Teacher/Specialist')|(df_site.role=='Paraprofessional')]['overall_score'].mean(),2)
        o_mindset = round(df_site[(df_site.role!= 'Teacher/Specialist')&(df_site.role!='Paraprofessional')]['overall_score'].mean(),2)
        
        mat1 = round(100*len(df_site[(df_site.materials_1=='Use often (once or twice weekly)')|(df_site.materials_1=='Use everyday')])/df_site.materials_1.value_counts().sum(),2)
        mat2 = round(100*len(df_site[(df_site.materials_2=='Use often (once or twice weekly)')|(df_site.materials_2=='Use everyday')])/df_site.materials_2.value_counts().sum(),2)
        mat3 = round(100*len(df_site[(df_site.materials_3=='Use often (once or twice weekly)')|(df_site.materials_3=='Use everyday')])/df_site.materials_3.value_counts().sum(),2)
        
        per1 = round(100*len(df_site[(df_site.curriculum_sch_dist_1=='Agree')|(df_site.curriculum_sch_dist_1=='Strongly agree')])/df_site.curriculum_sch_dist_1.value_counts().sum(),2)
        per2 = round(100*len(df_site[(df_site.curriculum_sch_dist_2=='Agree')|(df_site.curriculum_sch_dist_2=='Strongly agree')])/df_site.curriculum_sch_dist_2.value_counts().sum(),2)
        per3 = round(100*len(df_site[(df_site.curriculum_sch_dist_3=='Agree')|(df_site.curriculum_sch_dist_3=='Strongly agree')])/df_site.curriculum_sch_dist_3.value_counts().sum(),2)
        
        mast1 = round(100*len(df_site[df_site.curr_mastery =="I am just starting to become familiar with or try out my school or district’s adopted curriculum."])/df_site.curr_mastery.value_counts().sum(),2)
        mast2 = round(100*len(df_site[df_site.curr_mastery =="I’m building my capacity to use my school or district’s adopted curriculum materials, with some level of success."])/df_site.curr_mastery.value_counts().sum(),2)
        mast3 = round(100*len(df_site[df_site.curr_mastery =="I’m using my school or district’s materials with facility."])/df_site.curr_mastery.value_counts().sum(),2)
        mast4 = round(100*len(df_site[df_site.curr_mastery =="I’m using the curricular materials with mastery."])/df_site.curr_mastery.value_counts().sum(),2)
                              
        results.append({'Site':site,
                        'Submissions':n,
                       'Teacher Mindset Score':t_mindset,
                        'Leader Mindset Score':o_mindset,
                       'Materials1':mat1,
                       'Materials2':mat2,
                       'Materials3':mat3,
                       'Percep1':per1,
                       'Percep2':per2,
                       'Percep3':per3,
                       'Mastery1':mast1,
                        'Mastery2':mast2,
                       'Mastery3':mast3,
                       'Mastery4':mast4})  
                              
    return pd.DataFrame(results)

def sw_counts(df: pd.DataFrame) -> pd.DataFrame:
    summary = (
        df.groupby("new_site", as_index=False)
          .agg(
              Submissions=("teacher_name", "size"))
          .rename(columns={"new_site": "Site"})
    )

    return summary    


def sw_scoring(df: pd.DataFrame, timing: str) -> pd.DataFrame:
    df = df.copy()
    
    df["Date of Submission"] = pd.to_datetime(df["Date of Submission"], errors="coerce")
    
    # Drop rows missing needed fields for selection
    df = df.dropna(subset=["Teacher Name", "new_site", "Date of Submission"]).copy()

    if timing == "diagnostic":
        # earliest scored submission per teacher per site
        df = (
            df.sort_values("Date of Submission", ascending=True)
              .drop_duplicates(subset=["new_site", "Teacher Name"], keep="first")
              .copy()
        )
    elif timing == "second_round":
        # latest scored submission per teacher per site
        df = (
            df.sort_values("Date of Submission", ascending=True)
              .drop_duplicates(subset=["new_site", "Teacher Name"], keep="last")
              .copy()
        )
    else:
        raise ValueError("timing must be 'diagnostic' or 'second_round'")
    
    def count_occurrences(cell, target):
        if isinstance(cell, str):
            return cell.split(', ').count(target)
        return 0

    def count_total(cell):
        if isinstance(cell, str) and 'Duplicate' not in cell:
            return cell.count(',') + 1
        return 0

    # Apply all counts
    df['count_prof'] = df['Submitted Grade/s'].apply(lambda x: count_occurrences(x, '2'))
    df['count_ngl'] = df['Submitted Grade/s'].apply(lambda x: count_occurrences(x, 'Not on grade level'))
    df['count_nl'] = df['Submitted Grade/s'].apply(lambda x: count_occurrences(x, 'Not legible'))
    df['count_tda'] = df['Submitted Grade/s'].apply(lambda x: count_occurrences(x, 'Task does NOT ask students to respond or write from evidence'))
    df['count_total'] = df['Submitted Grade/s'].apply(count_total)
    df['duplicate'] = df['Submitted Grade/s'].apply(lambda x: count_occurrences(x, 'Duplicate'))
    df['skipped'] = df['Submitted Grade/s'].apply(lambda x:count_occurrences(x,'Skipped'))

    # Compute proficiency and on-grade level
    ct_on_gl = []
    ct_pro = []
    proficiency = []
    on_grade_level = []

    for i, row in df.iterrows():
        if pd.isna(row['Submitted Grade/s']) or row['duplicate'] > 0 or row['skipped'] > 0:
            ctgl = ctpro = gl = pro = np.nan
        else:
            ngl = row['count_ngl']
            n = row['count_total']
            ntda = row['count_tda']
            nnl = row['count_nl']

            if ngl == n:
                ctgl = 0
                ctpro = gl = pro = np.nan
            else:
                ctgl = n - ngl
                ctpro = row['count_prof']
                gl = ctgl / n if n else np.nan
                pro = ctpro / (n - ngl - ntda - nnl) if (n - ngl - ntda - nnl) else np.nan

        ct_on_gl.append(ctgl)
        ct_pro.append(ctpro)
        on_grade_level.append(gl)
        proficiency.append(pro)

    df['ct_on_gl'] = ct_on_gl
    df['ct_pro'] = ct_pro
    df['on_grade_level'] = on_grade_level
    df['proficiency'] = proficiency
                                                  
  # ---- Aggregate by site ----
    summary = (
        df.groupby("new_site", as_index=False)
          .agg(
              Proficiency=("proficiency", lambda s: round(s.mean(), 2))
          )
          .rename(columns={"new_site": "Site"})
    )

    return summary

def cl_scoring(df):
    results = []
    for site in df['new_site'].unique():
        df_site = df[df.new_site==site]
        
        n = df_site['nps_NPS_GROUP'].count()
        prom = (df_site['nps_NPS_GROUP'] == 'Promoter').sum()
        det = (df_site['nps_NPS_GROUP'] == 'Detractor').sum()
        nps = round(100 * (prom - det) / n, 2)
        
        n_size = df_site['mid_year_likert_qs_5'].count()
        if n_size == 0:
                continue
        advance_rate = round(100 * (
            (df_site['mid_year_likert_qs_5'] == '4- Agree') |
            (df_site['mid_year_likert_qs_5'] == '5- Strongly agree')).sum() / n_size, 2)
        
        results.append({'Site':site,
                       'Submissions':n,
                       'NPS':nps,
                       'Advance':advance_rate})
        
    return pd.DataFrame(results)
        

#### Set up final merged df - wide; merging on processed site ####
def merge_site_wide(pf_results, diag_results, diag_obs_results,mid_obs_results, diag_sw_count,diag_grades, mid_sw_count,mid_grades, cl_results, fu_results, key="Site", fill_blank=False):
    # Make copies and add suffixes to every column except the key
    pf  = pf_results.copy()
    ed  = diag_results.copy()
    ed2 = fu_results.copy()
    obs1 = diag_obs_results.copy()
    obs2 = mid_obs_results.copy()
    diag_swc = diag_sw_count.copy()
    mid_swc = mid_sw_count.copy()
    diag_g = diag_grades.copy()
    mid_g = mid_grades.copy()
    cl = cl_results.copy()

    pf.rename(columns={c: f"{c}_pf"  for c in pf.columns  if c != key},  inplace=True)
    ed.rename(columns={c: f"{c}_ed"  for c in ed.columns  if c != key},  inplace=True)
    ed2.rename(columns={c: f"{c}_ed2"  for c in ed2.columns  if c != key},  inplace=True)
    obs1.rename(columns={c: f"{c}_diag_obs" for c in obs1.columns if c != key}, inplace=True)
    obs2.rename(columns={c: f"{c}_mid_obs" for c in obs2.columns if c != key}, inplace=True)
    diag_swc.rename(columns = {c: f"{c}_diag_sw" for c in diag_swc.columns if c != key}, inplace=True)
    mid_swc.rename(columns = {c: f"{c}_mid_sw" for c in mid_swc.columns if c!=key}, inplace=True)
    diag_g.rename(columns = {c: f"{c}_diag_grades" for c in diag_g.columns if c != key}, inplace=True)
    mid_g.rename(columns = {c: f"{c}_mid_grades" for c in mid_g.columns if c != key}, inplace=True)
    cl.rename(columns = {c: f"{c}_cl" for c in cl.columns if c != key}, inplace=True)

    # Full outer merges on the key - reassigned site
    out = pf.merge(ed, on=key, how="outer").merge(obs1, on=key, how="outer").merge(obs2, on=key, how='outer').merge(diag_swc,on=key,how="outer").merge(mid_swc, on=key, how="outer").merge(diag_g,on=key,how="outer").merge(cl,on=key,how='outer').merge(ed2,on=key,how='outer').merge(mid_g,on=key,how="outer")

  
    if fill_blank:
        out = out.fillna("")

    
    other_cols = [c for c in out.columns if c != key]
    out = out[[key] + other_cols]

    return out


#### Main function - pull, process, score, and write data to Google Sheets ####
def main():
    # Data pull credentials - AWS & Qualtrics survey ID
    bucket_name = 'tl-surveys-26'
    aws_access_key_id = AWS_ACCESS_KEY_ID
    aws_secret_access_key = AWS_SECRET_ACCESS_KEY
    SW_QCODE = QUALTRICS_SW_ID
    
    # Pull data from AWS
    obs = read_parquet_from_s3(bucket_name, 'classroom_observations.parquet', aws_access_key_id, aws_secret_access_key)
    diag = read_parquet_from_s3(bucket_name, 'diagnostic_survey.parquet', aws_access_key_id, aws_secret_access_key)
    follow = read_parquet_from_s3(bucket_name, 'followup_educator.parquet', aws_access_key_id, aws_secret_access_key)
    pf = read_parquet_from_s3(bucket_name, 'participant_feedback.parquet', aws_access_key_id, aws_secret_access_key)
    sw = get_survey(SW_QCODE)
    sw_grades = read_parquet_from_s3(bucket_name, 'student_work_grades.parquet', aws_access_key_id, aws_secret_access_key)
    cl = read_parquet_from_s3(bucket_name, 'contact_lead.parquet',aws_access_key_id, aws_secret_access_key)

    cl['date']=pd.to_datetime(cl['date'])
    cl = cl[cl['date']>'12-01-2025'].copy()

    # Reassign site based on project work
    diag['new_site']=site_assign(diag)
    follow['new_site']=site_assign(follow)
    pf['new_site'] = site_assign(pf)
    obs['new_site'] = site_assign(obs)
    diag_obs = obs[obs.direct_to_ts_obs=='Baseline (first observation of the year)'].copy()
    mid_obs = obs[(obs.direct_to_ts_obs!='Baseline (first observation of the year)')&(obs.direct_to_ts_obs!='Ongoing')].copy()
    sw['new_site']=sw_site(sw)
    diag_sw = sw[sw['round']=='Baseline (first submission of the year)'].copy()
    mid_sw = sw[(sw['round']!='Baseline (first submission of the year)')&(sw['round']!='Other')].copy()
    sw_grades['new_site']=site_assign_swg(sw_grades)
    diag_swg = sw_grades[sw_grades.Prepost=='Baseline (first submission of the year)'].copy()
    mid_swg = sw_grades[(sw_grades.Prepost!='Baseline (first submission of the year)')&(sw_grades.Prepost!='Other')].copy()
    cl['new_site']=site_assign_cl(cl)

    # Apply scoring & reporting metrics to each data
    diag_results = ed_scoring(diag)
    fu_results = ed_scoring(follow)
    pf_results = pf_scoring(pf)
    diag_obs_results = ipg_scoring(diag_obs.dropna(subset=['content_area']),timing="diagnostic")
    mid_obs_results = ipg_scoring(mid_obs.dropna(subset=['content_area']),timing="second_round")
    diag_sw_count = sw_counts(diag_sw)
    mid_sw_count = sw_counts(mid_sw)
    diag_grades = sw_scoring(diag_swg, timing='diagnostic')
    mid_grades = sw_scoring(mid_swg, timing = 'second_round')
    cl_results = cl_scoring(cl)

    # merge all data on Site
    final = merge_site_wide(pf_results, diag_results, diag_obs_results, mid_obs_results,diag_sw_count,diag_grades,mid_sw_count,mid_grades,cl_results,fu_results, key="Site", fill_blank=False)

    #Set up writing to sheet    
    
    spreadsheet_write = RRKO_SHEET_ID
    worksheet1 = "raw"

    # Authenticate and open the sheets to read and write
    gc = authenticate_google_sheets()
    sheet = open_google_sheet(gc, spreadsheet_write)


    # Write to the worksheets
    write_to_sheet(sheet, worksheet1, final.reset_index().fillna(""))

if __name__ == "__main__":
    main()

