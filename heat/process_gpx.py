import gpxpy
import gpxpy.gpx
import json
import re
import lxml    
from lxml import etree
from os import listdir
from os.path import isfile, join
from geojson import LineString, Feature, FeatureCollection, dump
import numpy as np
import pandas as pd
from datetime import datetime

import pdb

def process_gpx(file):
    gpx_search = re.compile("gpx$")
    
    if gpx_search.search(file):

        pattern = re.compile(r"export_4778598/")
        file_filter = pattern.sub("", file)
        print(file)

        gpx_file = open(file, 'r')

        try:
            gpx = gpxpy.parse(gpx_file)
        except:
            print("Error opening file")
            return None
        
        for track in gpx.tracks:
            dict[track.name] = {}

            dict[track.name]['geo'] = []
            for segment in track.segments:
                for point in segment.points:
                    dict[track.name]['geo'].append([point.longitude,point.latitude])
                    #time_secs = ((point.time.hour * 360) + (point.time.minute * 60) + point.time.second) / 12239
                    #dict[track.name]['timestamp'].append([time_secs])

    tcx_search = re.compile("tcx$")
    if tcx_search.search(file):

        pattern = re.compile(r"export_4778598/")
        file_filter = pattern.sub("", file)
        
        print(file)
        
        ## the xml declaration of every tcx file is indented, which breaks things.
        ## fix this by reading in the file and replacing the first line to remove indent
        ## only really have to run once per data pull
        
        with open(file) as f:
            lines = f.readlines()
            
        lines[0] = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'

        ## now write file back out

        with open(file, "w") as f:
            f.writelines(lines)

        ## h/t to https://github.com/cast42/vpower/blob/master/vpower.py
        ## for helping me figure out how to parse tcx files
        
        ns1 = 'http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2'
        ns2 = 'http://www.garmin.com/xmlschemas/ActivityExtension/v2'
        
        tree = etree.parse(file)
        root = tree.getroot()
        
        tracks = []
        dict[file_filter] = {}

        dict[file_filter]['geo'] = []
        dict[file_filter]['timestamp'] = []
        
        for element in root.iter():
                if element.tag == '{%s}Track'%ns1:
                    tracks.append(element)

        lat = []
        lon = []           
        
        for element in tracks:
            for child in element:
                for elem in child.iter():
                    if elem.tag == '{%s}LatitudeDegrees'%ns1:
                        for node in elem.iter():
                            lat.append(node.text)
                    if elem.tag == '{%s}LongitudeDegrees'%ns1:    
                        for node in elem.iter():
                            lon.append(node.text)
                    if elem.tag == '{%s}Time'%ns1:
                        for node in elem.iter():
                            node.text = re.sub('-0[0-9]:00', '', node.text)
                            node.text = re.sub('\+0[0-9]:00', '', node.text)
                            new_time = datetime.strptime(node.text, '%Y-%m-%dT%H:%M:%S.%f')
                            time_secs = ((new_time.hour * 360) + (new_time.minute * 60) + new_time.second) / 12239
                            dict[file_filter]['timestamp'].append(time_secs)

        
        for i in range(0,len(lat),2):
            dict[file_filter]['geo'].append([float(lon[i]),float(lat[i])])


    ## now convert dict to geojson file

    features = []

    for key in dict.keys():
        features.append(Feature(geometry=LineString(dict[key]['geo']),properties={"Name": key,"Commute" : dict[key]['commute']}))

    return(dict)