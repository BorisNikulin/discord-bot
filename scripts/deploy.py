#!/usr/bin/env python

import sys
import os
import subprocess
import json
import hashlib

# see https://devcenter.heroku.com/articles/platform-api-deploying-slugs

# requires 1 arg for the slug file and a 2nd for the process_types.json
slug_file_path = sys.argv[1]
with open(sys.argv[2], 'rb') as slug:
    heroku_proc_types = json.load(slug)

    slug_data = slug.read()
    digest = hashlib.sha256()
    digest.update(slug_data)
    slug_checksum = digest.hexdigest()

    del slug_data
    del digest

# requires these custom environmnet variables
heroku_api_key = os.environ['HEROKU_API_KEY']
heroku_url_base = os.environ['HEROKU_URL_BASE']

slug_create = {
    'process_types': heroku_proc_types,
    'checksum': f'SHA256:{slug_checksum}',
    'commit': os.environ['CI_COMMIT_SHA'],
    'commit_description': os.environ['CI_COMMIT_TITLE']
}

print('---')
print(
    f""" curl -sSX POST \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/vnd.heroku+json; version=3' \
    -H 'Authorization: Bearer {{heroku_api_key}}' \
    -d '{json.dumps(slug_create, indent=4)}' \
    {heroku_url_base}/slugs
    """)

upload_info = json.loads(subprocess.check_output(
    f""" curl -sSX POST \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/vnd.heroku+json; version=3' \
    -H 'Authorization: Bearer {heroku_api_key}' \
    -d '{json.dumps(heroku_proc_types)}' \
    {heroku_url_base}/slugs
    """,
    shell=True))
print()

print(f"slug id: {upload_info['id']}")
print()

subprocess.run(
    f""" curl -sSX PUT \
    -H 'Content-Type:' \
    --data-binary @"{slug_file_path}" \
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
