from __future__ import print_function

from googleapiclient import discovery
from httplib2 import Http
from oauth2client import client, file, tools

SCOPES = "https://www.googleapis.com/auth/forms.body.readonly"
DISCOVERY_DOC = "https://forms.googleapis.com/$discovery/rest?version=v1"

store = file.Storage('token.json')
creds = None
if not creds or creds.invalid:
    flow = client.flow_from_clientsecrets('client_secrets.json', SCOPES)
    creds = tools.run_flow(flow, store)
service = discovery.build('forms', 'v1', http=creds.authorize(
    Http()), discoveryServiceUrl=DISCOVERY_DOC, static_discovery=False)

# Prints the title of the sample form:
form_id = '11XtABIeV0GxEP6X5ONrnMbR2aSczvmeOVmIBSDHMrEc'
result = service.forms().get(formId=form_id).execute()

print(result)

with open('data/google_forms/fy24_attendance.json', 'w') as f:
    json.dump(result.json(), f, indent=4, sort_keys=True)
