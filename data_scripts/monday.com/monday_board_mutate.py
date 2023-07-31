import requests
import os
import json

apiKey = "eyJhbGciOiJIUzI1NiJ9.eyJ0aWQiOjE2MDA3NzI5NCwidWlkIjoyMjAzOTU3NSwiaWFkIjoiMjAyMi0wNS0xMVQxODowMjo0MS4wMDBaIiwicGVyIjoibWU6d3JpdGUiLCJhY3RpZCI6ODg4NDgxOSwicmduIjoidXNlMSJ9.t1s_JYzM1xPn4qyOyOmDJ_aR03AL02rWnFmJQSPAQuw"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey}

# Make query with dynamically populating arguments
query4 = 'mutation ($myItemName: String! $columnVals: JSON!) { create_item (board_id:2208860812, item_name:$myItemName, column_values:$columnVals) { id } }'
# Define Facilitator Scoring variables
vars = {
 'myItemName' : 'A fake facilitator',
 'columnVals' : json.dumps({
   'numbers2' : {'Score1' : 5},
   'numbers6' : {'Score2' : 5}
 })
}
# Pass variables into data for json object
data = {'query' : query4, 'variables' : vars}

r = requests.post(url=apiUrl, json=data, headers=headers) # make request
print(r.json()) # Print request
