import requests
import json

apiKey = "eyJhbGciOiJIUzI1NiJ9.eyJ0aWQiOjE2MDA3NzI5NCwidWlkIjoyMjAzOTU3NSwiaWFkIjoiMjAyMi0wNS0xMVQxODowMjo0MS4wMDBaIiwicGVyIjoibWU6d3JpdGUiLCJhY3RpZCI6ODg4NDgxOSwicmduIjoidXNlMSJ9.t1s_JYzM1xPn4qyOyOmDJ_aR03AL02rWnFmJQSPAQuw"
apiUrl = "https://api.monday.com/v2"
headers = {"Authorization" : apiKey,
           'API-Version' : '2023-10'}

query2 = '{ boards(ids: 4987315255) { items_page (limit: 500, query_params: {rules: [{column_id: "short_text_2", compare_value: [""], operator:contains_text}]}) { items { column_values (ids: ["name", "date", "people3", "short_text_2", "email7", "single_select9", "date6", "single_select73", "short_text09", "multi_select_1", "multi_select", "short_text_161", "coaching_partners", "status", "single_select4", "short_text06", "blytheville_school", "faa_school", "hope_school", "osceola_schools", "cps_n4_school", "cps_n7_school", "cps_n12_school", "pt_coupee_school", "ascend_school", "d6_schools", "number", "number0", "number9", "number03", "number8", "rochester_school", "short_text38"]) { column { title id } text } } } } }'
data = {'query' : query2}

r = requests.post(url=apiUrl, json=data, headers=headers) # make request

with open('data/monday/fy24_teachers_coaching_log_full.json', 'w') as f:
    json.dump(r.json(), f, indent=4, sort_keys=True)
