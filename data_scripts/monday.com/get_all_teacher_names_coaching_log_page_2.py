import requests
import json

apiKey = "eyJhbGciOiJIUzI1NiJ9.eyJ0aWQiOjE2MDA3NzI5NCwidWlkIjoyMjAzOTU3NSwiaWFkIjoiMjAyMi0wNS0xMVQxODowMjo0MS4wMDBaIiwicGVyIjoibWU6d3JpdGUiLCJhY3RpZCI6ODg4NDgxOSwicmduIjoidXNlMSJ9.t1s_JYzM1xPn4qyOyOmDJ_aR03AL02rWnFmJQSPAQuw"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey,
           'API-Version' : '2023-10'}

query3 = 'query { next_items_page( limit: 500 cursor: "' + r.cursor_obj + '" ) { cursor items { column_values(ids: ["short_text_2"]) { text } } } }'
data = {'query' : query3}

req = requests.post(url=apiUrl, json=data, headers=headers) # make request

with open('data/Monday/fy24_teachers_coaching_log_page_2.json', 'w') as f:
    json.dump(req.json(), f, indent=4, sort_keys=True)
