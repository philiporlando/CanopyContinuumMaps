# pull 3rd party purple air data from api

import json
import urllib
import requests
import time
from datetime import datetime
import calendar
import sys
import itertools
import os
import pandas as pd

downDay = int(60*60*24)
downHour = int(60*60)
row = 0
t_start = time.time()
start_day = datetime.fromtimestamp(t_start).strftime("%Y-%m-%d")
#file_name = str(start_day) + "_online_purpleair_sensors.txt"
file_name = "existing_purple_air.txt"
dir_name = "/data/existing_purple_air"
full_path = os.getcwd() + "//" + dir_name

d = datetime.utcnow()
unixtime = calendar.timegm(d.utctimetuple())

if not os.path.isdir(full_path):
	os.mkdir(full_path)

os.chdir(full_path)

df = pd.DataFrame(columns=['datetime', 'site_id', 'site_name', 'lat', 'lon'])

print

## assigning PurpleAir API to url
url = "https://www.purpleair.com/json"

## GET request from PurpleAir API
try:
	r = requests.get(url)
	print '[*] Connecting to API...'
	print '[*] GET Status: ', r.status_code

except Exception as e:
	print '[*] Unable to connect to API...'
	print 'GET Status: ', r.status_code
	print e
print

try:
	## parse the JSON returned from the request
	j = r.json()

except Exception as e:
	print '[*] Unable to parse JSON'
	print e

try:
	##  iterate through entire dictionary
	for sensor in j['results']:

		if (unixtime - int(sensor['LastSeen'])) > downHour:
			#print '[*] Sensor', sensor['Label'], 'went offline at', datetime.fromtimestamp(sensor['LastSeen'])
			pass
		else:
			#print '[*]', str(sensor['Label']) + ':', sensor['ID'], 'lastSeen', datetime.fromtimestamp(sensor['LastSeen'])
			df.loc[row] = pd.Series(dict(datetime = datetime.fromtimestamp(sensor['LastSeen']), site_id = sensor['ID'],site_name = sensor['Label'], lat = sensor['Lat'], lon = sensor['Lon']))
			print df.loc[[row]]
			row += 1
			df.to_csv(file_name, sep = ',', index = False, encoding = 'utf-8')

except Exception as e:
	print '[*] Error, no data was written to file'
	print e
