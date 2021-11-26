import json
import requests

url = 'https://graphql.anilist.co'

query = '''
query ($search: String) {
  Page(page: 5000) {
    pageInfo {
      total,
      perPage,
      currentPage,
      lastPage
    }


    media(search: $search, type: MANGA) {
      title {
        english
      }
    }
  }
}
'''

response = requests.post(url, json={'query': query})

last_page = json.loads(response.text)['data']['Page']['pageInfo']['lastPage']

query = '''
query ($page: Int, $search: String) {
  Page(page: $page) {
    media(search: $search, type: MANGA, sort: FAVOURITES_DESC) {
      title {
        english
      }
      startDate {
        year
        month
      }
      endDate {
        year
        month
      }
      chapters
      volumes
      isAdult
      genres
      tags {
        name
        category
        rank
        isAdult
      }
      averageScore
      popularity
      isLicensed
    }
  }
}
'''

# Make the HTTP Api request
response = requests.post(url, json={'query': query, 'variables': {'page': 1}})
data = list(json.loads(response.text)['data']['Page']['media'])

# This will take a while
for i in range(1, last_page + 1):
  print(str(i/last_page))
  response = requests.post(url, json={'query': query, 'variables': {'page': i}})
  data = data + list(json.loads(response.text)['data']['Page']['media'])

with open('data/data.txt', 'w') as fp:
  json.dump(data, fp)