#!/usr/bin/env python

# requires:
# yum install gcc gcc-c++ freetype-devel libpng-devel python-devel python-pip
# or
# apt-get install python-dev build-essential libfreetype6-dev libpng12-dev pkg-config
# pip install numpy pandas matplotlib

import pandas as pd
import argparse
import math
import sys

def combine_status(start_status, end_status):
    if start_status == 'start' and end_status == 'end':
        return 'success'
    elif start_status == 'start' and type(end_status) == float and math.isnan(end_status):
        return 'timeout'
    elif start_status == 'start':
        return end_status
    else:
        return 'not_started'

parser = argparse.ArgumentParser(description='Merges starts and ends/errors.')
parser.add_argument('data_file', help='data csv file, with header')
args = parser.parse_args()

init_df = pd.read_csv(args.data_file, na_values=['na'])
[status_col, cat_col, id_col, ts_col] = init_df.columns

started_df = init_df[init_df[status_col] == 'start']
not_started_df = init_df[init_df[status_col] != 'start']
consolidated_df = pd.merge(started_df, not_started_df, how='outer', on=[cat_col, id_col], suffixes=['', '_end'])
consolidated_df['delta'] = consolidated_df[ts_col+'_end'] - consolidated_df[ts_col]
consolidated_df[status_col] = consolidated_df[status_col].combine(consolidated_df[status_col+'_end'], combine_status)
del consolidated_df[status_col+'_end']
del consolidated_df[ts_col+'_end']

print consolidated_df.to_csv(index=False, float_format='%.0f')
