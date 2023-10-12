import requests
import json

apiKey = "eyJhbGciOiJIUzI1NiJ9.eyJ0aWQiOjE2MDA3NzI5NCwidWlkIjoyMjAzOTU3NSwiaWFkIjoiMjAyMi0wNS0xMVQxODowMjo0MS4wMDBaIiwicGVyIjoibWU6d3JpdGUiLCJhY3RpZCI6ODg4NDgxOSwicmduIjoidXNlMSJ9.t1s_JYzM1xPn4qyOyOmDJ_aR03AL02rWnFmJQSPAQuw"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey,
           'API-Version' : '2023-10'}

query2 = '{ boards(ids: 4987315255) { items_page (limit: 500, query_params: {rules: [{column_id: "short_text_2", compare_value: [""], operator:contains_text}]}) { items { column_values (ids: ["short_text_2"]) { text } } } } }'
data = {'query' : query2}

r = requests.post(url=apiUrl, json=data, headers=headers) # make request

with open('data/Monday/fy24_teachers_coaching_log.json', 'w') as f:
    json.dump(r.json(), f, indent=4, sort_keys=True)
