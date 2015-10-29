#!/usr/bin/env python

# requires:
# yum install gcc gcc-c++ freetype-devel libpng-devel python-devel python-pip
# or
# apt-get install python-dev build-essential libfreetype6-dev libpng12-dev pkg-config
# pip install numpy pandas matplotlib

from collections import OrderedDict
from subprocess import call
import argparse
import jinja2
import pandas as pd
import sys

def marker_colour_gen():
    min_colour = 0x110000
    max_colour = 0xee0000
    diff = max_colour - min_colour
    curr_colour = 0x123456
    while True:
        yield '#' + hex(curr_colour+min_colour)[2:]
        curr_colour = (curr_colour + 0x6543456) % diff

marker_colour = marker_colour_gen()

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

def get_imgs(data_file, prefix, **filters):
    filters = ' '.join(['%s=%s' % (k,v) for k,v in filters.iteritems()])
    colour = marker_colour.next()
    return {
        'scatter_file':   'img/%s.scatter.png' % prefix,
        'scatter_cmd':    'python plot_csv.py --name=img/%s.scatter.png --colour=%s %s ts delta %s' % (prefix, colour, data_file, filters),
        'histogram_file': 'img/%s.histogram.png' % prefix,
        'histogram_cmd':  'python plot_csv.py --name=img/%s.histogram.png --colour=%s %s ts delta %s' % (prefix, colour, data_file, filters)
    }

def gen_imgs(root):
    run(root['imgs']['scatter_file'], root['imgs']['scatter_cmd'])
    run(root['imgs']['histogram_file'], root['imgs']['histogram_cmd'])
    for status in root['status_children']:
        run(status['imgs']['scatter_file'], status['imgs']['scatter_cmd'])
        run(status['imgs']['histogram_file'], status['imgs']['histogram_cmd'])
        for cat in status['cat_children']:
            run(cat['imgs']['scatter_file'], cat['imgs']['scatter_cmd'])
            run(cat['imgs']['histogram_file'], cat['imgs']['histogram_cmd'])

def gen_report(root):
    # Generate html report
    templateEnv = jinja2.Environment(loader=jinja2.FileSystemLoader(''))
    report_template = templateEnv.get_template('report_template/report_template.html')
    with open('report.html', 'w') as report_file:
        report_cnts = report_template.render(root)
        report_file.write(report_cnts)

def calc_stats(df, data_file):
    [status_col, cat_col, id_col, ts_col, delta_col] = df.columns
    statuses = sorted(set(df[status_col]))
    cats = sorted(set(df[cat_col]))
    root_info = OrderedDict()
    root_ns = 'root'
    root_info['name'] = root_ns
    root_info['stats'] = get_stats(df[delta_col])
    root_info['imgs'] = get_imgs(data_file, root_ns)
    status_children = []
    root_info['status_children'] = status_children
    for status in statuses:
        status_df = df[df[status_col] == status]
        if not status_df.empty:
            status_info = OrderedDict()
            status_ns = status
            status_info['name'] = status_ns
            status_info['stats'] = get_stats(status_df[delta_col])
            status_info['imgs'] = get_imgs(data_file, status_ns, status=status)
            cat_children = []
            status_info['cat_children'] = cat_children
            for cat in cats:
                cat_df = status_df[status_df[cat_col] == cat]
                if not cat_df.empty:
                    cat_info = OrderedDict()
                    cat_ns = '%s_%s' % (status, cat)
                    cat_info['name'] = cat
                    cat_info['stats'] = get_stats(cat_df[delta_col])
                    cat_info['imgs'] = get_imgs(data_file, cat_ns, status=status, client=cat)
                    cat_children.append(cat_info)
            status_children.append(status_info)
    return root_info

def main():
    parser = argparse.ArgumentParser(description='Orchestrate the report: analyse, plot graphs, create html report.')
    parser.add_argument('data_file', help='data csv file, with header')
    args = parser.parse_args()
    df = pd.read_csv(args.data_file, na_values=['na'])
    
    stats = calc_stats(df, args.data_file)
    gen_imgs(stats)
    gen_report(stats)
    print "\nFinal stats:" ; import json; print json.dumps(stats, indent=2)

if __name__ == "__main__":
    main()
