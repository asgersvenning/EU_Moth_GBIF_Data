# lftp command template: lftp -c "open ftp://io.storage.com:21; set ftp:ssl-protect-data on; cd DIRECTORY; mirror --use-pget-n=5 URL"

import re
import time
import logging
import subprocess
from datetime import datetime
from logging.handlers import RotatingFileHandler
import threading
from concurrent.futures import ThreadPoolExecutor
from collections import defaultdict

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

# Lock for shared variables
lock = threading.Lock()

# List of urls to files to be uploaded
image_csv = "MothEU_barebones.csv"
# Structure of csv file:
# 1st row: "match;gbifID;identifier;acceptedScientificName;genus;family" (header)
# other rows: "string;integer;url;string;string;string" (data)

# Function: clean strings for directory names with regex "re"
def clean_string(string):
    string = re.sub(r'[^a-zA-Z0-9_ ]', '', string)
    string = re.sub(r'\s+', '_', string)
    return string

# Initialize a dictionary to hold the information by directory
dir_dict = defaultdict(list)

with open(image_csv, 'r') as f:
    for i, line in enumerate(f):
        if i == 0:
            continue
        row = line.split(';')
        url = row[2].rstrip()
        image_id = row[1]
        family = clean_string(row[5])
        genus = clean_string(row[4])
        species = clean_string(row[3])
        
        directory = f'AMI_GBIF_Pretraining_Data/root/{family}/{genus}/{species}/'
        dir_dict[directory].append({'url': url, 'image_id': image_id})

# Initialize shared variables
processed_files = 0
total_files = sum(len(file_list) for file_list in dir_dict.values())

start_time = time.time()

print(f'Uploading {total_files} files to ERDA...')

# --------------------------------------------------
### UNUSED CODE ###
# # Function create lftp command
# def create_lftp_cmd(url, directory, file_name, connection_limit=-1):
#     original_name = url.split('/')[-1]  # Extracting the original file name from the URL
#     file_ext = original_name.split('.')[-1]  # Extracting the file extension from the original file name
#     base_cmd = f'lftp -c "open sftp://io.erda.au.dk; set ftp:ssl-protect-data on; set net:connection {connection_limit};'
#     return f'{base_cmd} mkdir -p {directory}; cd {directory}; put {url}; mv {original_name} {file_name}.{file_ext}"'
#
# # Function: upload file to ERDA
# def upload_file_to_erda(url, directory, file_name):
#     cmd = create_lftp_cmd(url, directory, file_name)
#     LOGGER.info(f'[{get_current_time()}] Executing command: {cmd}')
#    
#     result = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
#    
#     if result.returncode == 0:
#         LOGGER.info(f'[{get_current_time()}] Successfully uploaded {file_name} to {directory}')
#     else:
#         LOGGER.error(f'[{get_current_time()}] Failed to upload {file_name} to {directory}. Error: {result.stderr.decode()}')
#
# def wrapper_upload(i, url, species, genus, family, image_id):
#     LOGGER.info(f'[{get_current_time()}] Uploading file {i+1}')
#     upload_file_to_erda(url, f'AMI_GBIF_Pretraining_Data/root/{family}/{genus}/{species}/', f'{image_id}')

# Function: get current time
def get_current_time():
    return datetime.now().strftime('%Y-%m-%d %H:%M:%S')

# Function: get current date
def get_current_date():
    return datetime.now().strftime('%Y-%m-%d')

# Function: get current timestamp
def get_current_timestamp():
    return datetime.now().strftime('%Y%m%d%H%M%S')

# Function: create lftp batch command
def create_lftp_batch_cmd(file_batch, directory):
    base_cmd = f'lftp -c "open sftp://io.erda.au.dk; set ftp:ssl-protect-data on; set net:connection-limit 0; set net:limit-rate 0; set ssl:verify-certificate off; mkdir -p {directory}; cd {directory}; '
    get_and_rename_cmds = []
    
    for file_info in file_batch:
        url = file_info['url']
        image_id = file_info['image_id']
        original_name = url.split('/')[-1]
        file_ext = original_name.split('.')[-1]
        get_and_rename_cmds.append(f'put {url}; mv {original_name} {image_id}.{file_ext}')

    cmd = base_cmd + "; ".join(get_and_rename_cmds) + '"'  # Joining the commands with a semicolon
    return cmd

def upload_files_for_directory(directory, file_list):
    global start_time, processed_files, total_files

    # Calculate ETA
    total_time_taken = time.time() - start_time

    avg_time_per_file = total_time_taken / processed_files if processed_files > 0 else 0
    remaining_files = total_files - processed_files
    remaining_time = remaining_files * avg_time_per_file  # in seconds
                
    hours, remainder = divmod(int(remaining_time), 3600)
    minutes, seconds = divmod(remainder, 60)

    # Log before uploading directory
    LOGGER.info(f'[{get_current_time()} | ETA: {hours}h {minutes}m {seconds}s] Uploading {len(file_list)} files to {directory}')
    
    batch_size = 50
    num_batches, remainder = divmod(len(file_list), batch_size)
    if remainder > 0:
        num_batches += 1

    end_idx = 0
    
    for i in range(num_batches):
        try:
            start_idx = i * batch_size
            end_idx = min(start_idx + batch_size, len(file_list))
            file_batch = file_list[start_idx:end_idx]
            batch_file_names = ", ".join(file_info['image_id'] for file_info in file_batch)

            cmd = create_lftp_batch_cmd(file_batch, directory)
            
            lftp_result = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            
            total_time_taken = time.time() - start_time
            try:
                with lock:  # Ensure that only one thread updates the shared variables at a time
                    processed_files += len(file_batch)
            except Exception as e:
                print("LOG ERROR:", e)
                
            # Calculate ETA
            avg_time_per_file = total_time_taken / processed_files if processed_files > 0 else 0
            remaining_files = total_files - processed_files
            remaining_time = remaining_files * avg_time_per_file  # in seconds
                        
            hours, remainder = divmod(int(remaining_time), 3600)
            minutes, seconds = divmod(remainder, 60)
            
            # Log result
            if lftp_result.returncode == 0:
                LOGGER.info(f'[{get_current_time()} | ETA: {hours}h {minutes}m {seconds}s] Successfully uploaded ({batch_file_names}) to {directory}')
            else:
                LOGGER.error(f'[{get_current_time()} | ETA: {hours}h {minutes}m {seconds}s] Failed to upload ({batch_file_names}) to {directory}. Error: {lftp_result.stderr.decode()}')
        except Exception as e:
            LOGGER.info("BATCH LOOP ERROR:", e)
    else:
        # check if all files have been uploaded
        if end_idx != len(file_list):
            LOGGER.info(f'ERROR: {end_idx} != {len(file_list)}')


def main():
    max_workers = 10
    LOGGER.info(f'[{get_current_time()} | ETA: ?h ?m ?s] Starting upload to ERDA with {max_workers} workers')

    futures = []
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        for directory, file_list in dir_dict.items():
            # The actual function to execute in a new thread will be wrapped by the executor
            future = executor.submit(upload_files_for_directory, directory, file_list)
            futures.append(future)
        
    for future in futures:
        try:
            future.result()  # This will raise an exception if the function failed
        except Exception as e:
            LOGGER.error(f"Thread failed with error: {e}")
            

if __name__ == '__main__':
    main()