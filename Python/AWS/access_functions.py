## Imports

import boto3
import os
import pandas as pd
import rpy2
from rpy2 import robjects
from rpy2.robjects import r, pandas2ri
from rpy2.robjects.conversion import localconverter
from io import BytesIO

# Enable the automatic conversion of R data to pandas dataframes
pandas2ri.activate()

## RDS files
def retrieve_rds(bucket, file, name):
    # AWS credentials and bucket details
    AWS_ACCESS_KEY_ID = ID
    AWS_SECRET_ACCESS_KEY = key
    BUCKET_NAME = bucket
    FILE_KEY = file

    # Get the current working directory
    current_directory = os.getcwd()
    local_file_path = os.path.join(current_directory, name)

    def download_file_from_s3(bucket_name, file_key, local_file_path):
        # Initialize a session using Amazon S3
        s3 = boto3.client(
            's3',
            aws_access_key_id=AWS_ACCESS_KEY_ID,
            aws_secret_access_key=AWS_SECRET_ACCESS_KEY
        )

        # Download the file from S3
        s3.download_file(bucket_name, file_key, local_file_path)
        print(f'File downloaded to {local_file_path}')

    def read_rds_file(file_path):
        # Read the .rds file into an R object
        readRDS = r['readRDS']
        r_object = readRDS(file_path)

        # Convert R object to pandas DataFrame if it is a data frame
        with localconverter(robjects.default_converter + pandas2ri.converter):
            py_data = robjects.conversion.rpy2py(r_object)

        return py_data

    # Download and read the .rds file
    download_file_from_s3(BUCKET_NAME, FILE_KEY, local_file_path)
    r_data = read_rds_file(local_file_path)

    # If it's a dataframe, display it
    if isinstance(r_data, pd.DataFrame):
        return r_data
    else:
        print(f'Read R object of type: {type(r_data)}')


## Parquet file

def retrieve_parquet(bucket_name, key):

    # Initialize an S3 session
    session = boto3.Session(
        aws_access_key_id=ID,
        aws_secret_access_key=key,
        region_name=region
    )
    s3_client = session.client('s3')
    
    # Retrieve the parquet file from S3
    response = s3_client.get_object(Bucket=bucket_name, Key=key)
    data = response['Body'].read()
    
    # Convert the binary data into a pandas DataFrame
    df = pd.read_parquet(BytesIO(data), engine='pyarrow')  # You can switch to 'fastparquet' if you prefer
    
    return df



## View buckets and objects
s3 = boto3.resource(
    service_name='s3',
    region_name=region,
    aws_access_key_id=ID,
    aws_secret_access_key=key)

#check available buckets in s3 session 
for bucket in s3.buckets.all():
    print(bucket.name)

for obj in s3.Bucket(bucketname).objects.all():
    print(obj)
