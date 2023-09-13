# lftp command template: lftp -c "open ftp://io.storage.com:21; set ftp:ssl-protect-data on; cd DIRECTORY; mirror --use-pget-n=5 URL"

import os
import sys
import re
import time
import json
import logging
import argparse
import subprocess
from datetime import datetime
from logging.handlers import RotatingFileHandler

# Global variables
# --------------------------------------------------
# Log file
LOG_FILE = 'upload2ERDA.log'
# Log level
LOG_LEVEL = logging.INFO
# Log format
LOG_FORMAT = '%(asctime)s %(levelname)s %(message)s'
# Log max size
LOG_MAX_SIZE = 10 * 1024 * 1024
# Log backup count
LOG_BACKUP_COUNT = 5
# Log handler
LOG_HANDLER = RotatingFileHandler(LOG_FILE, mode='a', maxBytes=LOG_MAX_SIZE, backupCount=LOG_BACKUP_COUNT)
# Log formatter
LOG_FORMATTER = logging.Formatter(LOG_FORMAT)
# Log handler set formatter
LOG_HANDLER.setFormatter(LOG_FORMATTER)
# Log handler set level
LOG_HANDLER.setLevel(LOG_LEVEL)
# Logger
LOGGER = logging.getLogger(__name__)
# Logger add handler
LOGGER.addHandler(LOG_HANDLER)
# Logger set level
LOGGER.setLevel(LOG_LEVEL)

# List of urls to files to be uploaded
image_csv = "MothEU_barebones.csv"
# Structure of csv file:
# 1st row: "match;gbifID;identifier;acceptedScientificName;genus;family" (header)
# other rows: "string;integer;url;string;string;string"

# List of urls to files to be uploaded
image_urls = []
image_ids = []
image_family = []
image_genus = []
image_species = []

# Function: clean strings for directory names with regex "re"
def clean_string(string):
    string = re.sub(r'[^a-zA-Z0-9_ ]', '', string)
    string = re.sub(r'\s+', '_', string)
    return string

with open(image_csv, 'r') as f:
    for i, line in enumerate(f):
        if i == 0:
            continue
        if i > 10:
            break
        row = line.split(';')
        image_urls.append(row[2].rstrip())
        image_ids.append(row[1])
        # The family, genus and species, should be cleaned such that they can be used as directory names on linux.
        # This means that all spaces should be replaced with underscores, and all non-alphanumeric characters should be removed.
        image_family.append(clean_string(row[5]))
        image_genus.append(clean_string(row[4]))
        image_species.append(clean_string(row[3]))
# --------------------------------------------------

# Function: get current time
def get_current_time():
    return datetime.now().strftime('%Y-%m-%d %H:%M:%S')

# Function: get current date
def get_current_date():
    return datetime.now().strftime('%Y-%m-%d')

# Function: get current timestamp
def get_current_timestamp():
    return datetime.now().strftime('%Y%m%d%H%M%S')

# Function create lftp command
def create_lftp_cmd(url, directory, file_name, connection_limit=9999):
    original_name = url.split('/')[-1]  # Extracting the original file name from the URL
    return f'lftp -e "set ftp:ssl-protect-data on; set net:connection-limit {connection_limit}; mkdir -p {directory}; cd {directory}; mirror --use-pget-n=5 {url}; mv {original_name} {file_name}" -p 21 io.erda.au.dk'

# Function: upload file to ERDA
def upload_file_to_erda(url, directory, file_name):
    cmd = create_lftp_cmd(url, directory, file_name)
    LOGGER.info(f'[{get_current_time()}] Executing command: {cmd}')
    
    result = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    
    if result.returncode == 0:
        LOGGER.info(f'[{get_current_time()}] Successfully uploaded {file_name} to {directory}')
    else:
        LOGGER.error(f'[{get_current_time()}] Failed to upload {file_name} to {directory}. Error: {result.stderr.decode()}')

# Main function
def main():
    # Try a single file first
    LOGGER.info(f'[{get_current_time()}] Start uploading files to ERDA')
    for i, (url, species, genus, family) in enumerate(zip(image_urls, image_species, image_genus, image_family)):
        LOGGER.info(f'[{get_current_time()}] Uploading file {i+1}/{len(image_urls)}')
        upload_file_to_erda(url, f'AMI_GBIF_Pretraining_Data/{family}/{genus}/{species}/', f'{image_ids[i]}.jpg')
        break
    LOGGER.info(f'[{get_current_time()}] Finished uploading files to ERDA')


if __name__ == '__main__':
    main()