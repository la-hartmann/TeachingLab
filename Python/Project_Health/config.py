# config.py
import os
import json

# --- AWS credentials from environment variables ---
AWS_ACCESS_KEY_ID = os.environ["AWS_ACCESS_KEY_ID"]
AWS_SECRET_ACCESS_KEY = os.environ["AWS_SECRET_ACCESS_KEY"]
AWS_DEFAULT_REGION = os.environ.get("AWS_DEFAULT_REGION", "us-east-1")

# --- Google Sheets service account JSON from env ---
GOOGLE_SHEETS_CREDS_RAW = os.environ.get("GOOGLE_SHEETS_CREDS")

if GOOGLE_SHEETS_CREDS_RAW:
    GOOGLE_SHEETS_CREDS = json.loads(GOOGLE_SHEETS_CREDS_RAW)
else:
    GOOGLE_SHEETS_CREDS = None

# --- Google Sheets IDs - stored as Posit env variable, override with exact sheet ID if necessary ---
RRKO_SHEET_ID = os.environ.get("RRKO_SHEET_ID", "1m8K4gpf5jctyMWpo7Qt3Aa0CQh9-wwGeVTyDuEtkrlk")


# -- Qualtrics credentials for environment variables --
QUALTRICS_SW_ID = os.environ["QUALTRICS_SW_ID"]
QUALTRICS_CLIENT = os.environ["QUALTRICS_CLIENT"]
QUALTRICS_TOKEN = os.environ["QUALTRICS_TOKEN"]
