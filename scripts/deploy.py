#!/usr/bin/env python

import sys
import os
import subprocess
import json

# see https://devcenter.heroku.com/articles/platform-api-deploying-slugs

# requires 1 arg for the slug file and a 2nd for the process_types.json
with open(sys.argv[2], 'r') as f:
    heroku_proc_types = json.load(f)

# requires these environmnet variables
heroku_api_key = os.environ['HEROKU_API_KEY']
heroku_url_base = os.environ['HEROKU_URL_BASE']

print('---')
print(
    f""" curl -sSX POST \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/vnd.heroku+json; version=3' \
    -H 'Authorization: Bearer {{heroku_api_key}}' \
    -d '{{"process_types":{json.dumps(heroku_proc_types)}}}' \
    {heroku_url_base}/slugs
    """)

upload_info_str = subprocess.check_output(
    f""" curl -sSX POST \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/vnd.heroku+json; version=3' \
    -H 'Authorization: Bearer {heroku_api_key}' \
    -d '{{"process_types":{json.dumps(heroku_proc_types)}}}' \
    {heroku_url_base}/slugs
    """,
    shell=True)
print()

upload_info = json.loads(upload_info_str)


print(f"slug id: {upload_info['id']}")
print()

subprocess.run(
    f""" curl -sSX PUT \
    -H 'Content-Type:' \
    --data-binary @"{sys.argv[1]}" \
    "{upload_info['blob']['url']}"
    """,
    shell=True)

print(
    json.dumps(
        json.loads(
            subprocess.check_output(
                f""" curl -sSX POST \
                -H "Content-Type: application/json" \
                -H "Accept: application/vnd.heroku+json; version=3" \
                -H 'Authorization: Bearer {heroku_api_key}' \
                -d '{{"slug":"{upload_info['id']}"}}' \
                "{heroku_url_base}/releases"
                """,
                shell=True
            )
        ),
        indent=4
    )
)
print('---')
