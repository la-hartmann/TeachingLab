import requests
import json

apiKey = "eyJhbGciOiJIUzI1NiJ9.eyJ0aWQiOjE2MDA3NzI5NCwidWlkIjoyMjAzOTU3NSwiaWFkIjoiMjAyMi0wNS0xMVQxODowMjo0MS4wMDBaIiwicGVyIjoibWU6d3JpdGUiLCJhY3RpZCI6ODg4NDgxOSwicmduIjoidXNlMSJ9.t1s_JYzM1xPn4qyOyOmDJ_aR03AL02rWnFmJQSPAQuw"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey}

query2 = '{ boards(ids: 4830833150) { groups { title items (limit: 200) { column_values (ids: ["text8", "text"]) { title text } } } } }'
data = {'query' : query2}

r = requests.post(url=apiUrl, json=data, headers=headers) # make request
print(r.json())


with open('data/Monday/fy24_sites_courses.json', 'w') as f:
    json.dump(r.json(), f, indent=4, sort_keys=True)


