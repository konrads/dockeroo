#!/usr/bin/env python

# requires:
# yum install gcc gcc-c++ freetype-devel libpng-devel python-devel python-pip
# or
# apt-get install python-dev build-essential libfreetype6-dev libpng12-dev pkg-config
# pip install numpy pandas matplotlib

from subprocess import call
import argparse
import jinja2
import os
import numpy as np
import pandas as pd
import pprint as pp
import sys

plot_csv_exec = "%s/plot_csv.py" % os.path.dirname(os.path.realpath(__file__))

root_colour = '#00006E'
status_colour = '#600020'
cat_colour = '#004400'

def filter_nans(df):
    val_col = df.columns[-1]
    return df[np.isfinite(df[val_col])]

def get_stats(col):
    return {
        'mean':   col.mean(),
        'median': col.median(),
        'std':    col.std(),
        'cnt':    len(col)
    }

def run(File, Cmd):
    print "Plotting %s" % File
    sys.stdout.flush()
    call(Cmd, shell=True)

def get_imgs(data_file, prefix, colour, **filters):
    filters = ' '.join(['%s=%s' % (k,v) for k,v in filters.iteritems()])
    return {
        'scatter_file':   'img/%s.scatter.png' % prefix,
        'scatter_cmd':    'python %s --name=img/%s --colour=%s %s ts delta %s' % (plot_csv_exec, prefix, colour, data_file, filters),
        'histogram_file': 'img/%s.histogram.png' % prefix,
        'histogram_cmd':  'python %s --name=img/%s --colour=%s %s ts delta %s' % (plot_csv_exec, prefix, colour, data_file, filters)
    }

def gen_imgs(root):
    if 'imgs' in root:
        run(root['imgs']['scatter_file'], root['imgs']['scatter_cmd'])
        run(root['imgs']['histogram_file'], root['imgs']['histogram_cmd'])
    for status in root['status_children']:
        if 'imgs' in status:
            run(status['imgs']['scatter_file'], status['imgs']['scatter_cmd'])
            run(status['imgs']['histogram_file'], status['imgs']['histogram_cmd'])
        for cat in status['cat_children']:
            if 'imgs' in cat:
                run(cat['imgs']['scatter_file'], cat['imgs']['scatter_cmd'])
                run(cat['imgs']['histogram_file'], cat['imgs']['histogram_cmd'])

def gen_report(root):
    # Generate html report
    templateEnv = jinja2.Environment(loader=jinja2.FileSystemLoader(''))
    report_template = templateEnv.get_template('template/report_template.html')
    with open('report.html', 'w') as report_file:
        report_cnts = report_template.render(root)
        report_file.write(report_cnts)

def calc_stats(df, data_file):
    [status_col, cat_col, id_col, ts_col, delta_col] = df.columns
    statuses = sorted(set(df[status_col]))
    cats = sorted(set(df[cat_col]))
    status_children = []
    root_ns = 'root'
    root_info = {
        'name': root_ns,
        'stats': get_stats(df[delta_col]),
        'status_children': status_children
    }
    if not filter_nans(df).empty:
        root_info['imgs'] = get_imgs(data_file, root_ns, root_colour)
    for status in statuses:
        status_df = df[df[status_col] == status]
        if not status_df.empty:
            cat_children = []
            status_ns = status
            status_info = {
                'name': status,
                'stats': get_stats(status_df[delta_col]),
                'cat_children': cat_children
            }
            # plots must have at least 1 finite val
            if not filter_nans(status_df).empty:
                status_info['imgs'] = get_imgs(data_file, status_ns, status_colour, status=status)              
            for cat in cats:
                cat_df = status_df[status_df[cat_col] == cat]
                if not cat_df.empty:
                    cat_ns = '%s_%s' % (status, cat)
                    cat_info = {
                        'name': cat,
                        'stats': get_stats(cat_df[delta_col])
                    }
                    # plots must have at least 1 finite val
                    if not filter_nans(cat_df).empty:
                        cat_info['imgs'] = get_imgs(data_file, cat_ns, cat_colour, status=status, client=cat)
                    cat_children.append(cat_info)
            status_children.append(status_info)
    return root_info

def main():
    parser = argparse.ArgumentParser(description='Orchestrate the report: analyse, plot graphs, create html report.')
    parser.add_argument('data_file', help='data csv file, with header')
    parser.add_argument('report_name', help='report name')
    args = parser.parse_args()
    df = pd.read_csv(args.data_file, na_values=['na'])
    
    stats = calc_stats(df, args.data_file)
    stats['report_name'] = args.report_name
    gen_imgs(stats)
    print "\nFinal stats:"
    pp.pprint(stats, indent=2)
    print "\nGenerating reports:"
    gen_report(stats)

if __name__ == "__main__":
    main()
