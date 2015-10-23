#!/usr/bin/env python

# requires:
# yum install gcc gcc-c++ freetype-devel libpng-devel python-devel python-pip
# pip install numpy pandas matplotlib

import pandas as pd
import matplotlib.pylab as plt
import matplotlib
import argparse
import re
import StringIO

TYPES = ['scatter', 'line', 'histogram']
choice_types = TYPES[:]
choice_types.append('all')

# read in the args
parser = argparse.ArgumentParser(description='Generate report charts.')
parser.add_argument('data_file', help='data csv file, with header')
parser.add_argument('xaxis', help='x axis')
parser.add_argument('yaxis', help='y axis')
parser.add_argument('--xsize', type=int, default=10, help='graph X size')
parser.add_argument('--ysize', type=int, default=5, help='graph Y size')
parser.add_argument('--colour', type=str, default='#000066', help='graph colour')
parser.add_argument('--norm_x', type=bool, default=True, help='normalize x axis')
parser.add_argument('--marker', type=str, default='.', choices=[',', '+', '.', 'o', '*'])
parser.add_argument('--markersize', type=float, default=5.0, help='scatter plot marker size')
parser.add_argument('--linestyle', type=str, default='-', help='line plot line style')
parser.add_argument('--type', default='all', choices=choice_types)
parser.add_argument('--exclude', type=str, default='(Summary.*|summary.*|Elapsed.*)')
parser.add_argument('--filter-additional-headers', type=bool, default=True, help='filter out multiple headers (first line)')
args = parser.parse_args()

# choose plot type, or all
if args.type == 'all':
    types = TYPES
else:
    types = [args.type]

# read in the data
data = None
first_line = None
with open(args.data_file) as f:
    for line in f:
        if not first_line:
            first_line = line
            data = first_line
        elif args.filter_additional_headers and line == first_line:
            pass
        elif not re.match(args.exclude, line):
            data += line

df = pd.read_csv(StringIO.StringIO(data), na_values=['na'], skiprows=-2)

if args.norm_x:
    df[args.xaxis] = df[args.xaxis] - min(df[args.xaxis])

# validate axis exist in csv
if args.xaxis not in df:
    print 'Error: X axis %s is not a valid CSV column!' % args.xaxis 
    exit(1)
if args.yaxis not in df:
    print 'Error: Y axis %s is not a valid CSV column!' % args.yaxis 
    exit(1)

# start plotting
matplotlib.rc('font', size=8)
matplotlib.rcParams['legend.fontsize'] = 8
matplotlib.rcParams['figure.figsize'] = args.xsize, args.ysize
for t in types:
    print 'Generating %s...' % t
    if t == 'histogram':
        plt.hist(df[args.yaxis], color=args.colour)
        plt.xlabel(args.xaxis)
        plt.ylabel(args.yaxis)
        plt.tight_layout()
        plt.savefig(args.data_file+'.histogram.png', transparent=True)
        plt.clf()    
    else:
        if t == 'scatter':
            marker = args.marker
            linestyle = ''
            chart_file = args.data_file+'.scatter.png'
        else:
            marker = ''
            linestyle = args.linestyle
            chart_file = args.data_file+'.line.png'
        plt.plot(df[args.xaxis], df[args.yaxis], color=args.colour, markeredgecolor=args.colour, marker=marker, linestyle=linestyle, markersize=args.markersize)
        plt.xlabel(args.xaxis)
        plt.ylabel(args.yaxis)
        plt.tight_layout()
        plt.savefig(chart_file, transparent=True)
        plt.clf()