#!/usr/bin/env python

import sys
import os
import http.client
import urllib.request
import json
import hashlib

# see https://devcenter.heroku.com/articles/platform-api-deploying-slugs

# requires 1 arg for the slug file and a 2nd for the process_types.json
slug = open(sys.argv[1], 'rb')
with open(sys.argv[2], 'r') as proc_types:
    heroku_proc_types = json.load(proc_types)

slug_data = slug.read()

digest = hashlib.sha256()
digest.update(slug_data)
slug_checksum = digest.hexdigest()

del digest

# requires these custom environmnet variables
heroku_api_key = os.environ['HEROKU_API_KEY']
heroku_url_base = os.environ['HEROKU_URL_BASE']
heroku_app_name = os.environ['HEROKU_APP_NAME']

heroku_api_con = http.client.HTTPSConnection(heroku_url_base)
heroku_headers = {
    'Content-Type': 'application/json',
    'Accept': 'application/vnd.heroku+json; version=3',
    'Authorization': f'Bearer {heroku_api_key}'
    }


slug_create = {
    'process_types': heroku_proc_types,
    'checksum': f'SHA256:{slug_checksum}',
    'commit': os.environ['CI_COMMIT_SHA'],
    'commit_description': os.environ['CI_COMMIT_TITLE']
}

heroku_api_con.request(
    'POST',
    f'/apps/{heroku_app_name}/slugs',
    headers=heroku_headers,
    body=json.dumps(slug_create)
)

slug_upload_info = json.loads(heroku_api_con.getresponse().read())

print('---')
print('slug provision')
print(json.dumps(slug_upload_info, indent=4))
print()


slug_upload_data_response = urllib.request.urlopen(
    urllib.request.Request(
        slug_upload_info['blob']['url'],
        method='PUT',
        headers={'Content-Type': ''},
        data=slug_data
    )
)

slug.close()

print('slug upload')
print(f'HTTP code: {slug_upload_data_response.getcode()}')
print()

release_create = {
    'slug': slug_upload_info['id']
}

heroku_api_con.request(
    'POST',
    f'/apps/{heroku_app_name}/releases',
    headers=heroku_headers,
    body=json.dumps(release_create)
)

print('release provision')
print(json.dumps(json.loads(heroku_api_con.getresponse().read()), indent=4))
