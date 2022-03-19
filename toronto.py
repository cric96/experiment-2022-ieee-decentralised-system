import pandas as pd
import requests
from os.path import exists
from gpx_converter import Converter
import gpxpy
import os
import shutil

tmp_folder = "./tmp/"
target_folder = "./src/main/resources/toronto/"
toronto_data = "./toronto.csv"
toronto_reduced_data = "./src/main/resources/toronto.csv"
startingSeconds = 21575000
endingSeconds = 21620000
precision = 5
## remove prev folder
shutil.rmtree(tmp_folder, ignore_errors=True)
os.mkdir(tmp_folder)
# Get the dataset metadata by passing package_id to the package_search endpoint
def load_from_remote():
  url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3/action/package_show"
  params = { "id": "f2933501-0373-4734-b50c-4e4f39646180"}
  package = requests.get(url, params = params).json()
  # Take only the data produced for 2021
  csv2021 = package["result"]["resources"][0] ## prefer oldest data
  # Load csv as pandas
  print("download data...")
  return pd.read_csv(csv2021["url"])

def load_locally():
  print("load locally")
  return pd.read_csv(toronto_data)

data = load_locally() if exists(toronto_data) else load_from_remote()
correct_position = data.round(precision)
# Store csv
correct_position.to_csv(toronto_data)
latlong = correct_position.drop_duplicates(subset = ["latitude", "longitude"])
# Date to second
date = pd.to_datetime(data.date, format="%Y-%m-%dT%H:%M:%S")
date = pd.to_numeric(date)
date = date - date.head(1)[0] ## normalise
date = date / 10 ** 9
data['seconds'] = date ## add second
focus_on = data[data.seconds > startingSeconds][data.seconds < endingSeconds]
focus_on[["id","name","date","rainfall","longitude","latitude","seconds"]].to_csv(toronto_reduced_data)
# Store a gpx track for each element
for id in latlong["id"].drop_duplicates():
  storeDf = latlong[latlong["id"] == id]

  print("store {0}".format(id))
  Converter.dataframe_to_gpx(
    input_df=storeDf,
    lats_colname='latitude',
    longs_colname='longitude',
    times_colname='date',
    output_file=tmp_folder + 'toronto-rain-{0}.gpx'.format(id)
  )
# Merge in one file
merged = gpxpy.gpx.GPX()
for root, _, files in os.walk(tmp_folder):
    for file in files:
        if file.endswith(".gpx"):
            gpx_file = open("/".join([root,file]), 'r')
            gpx = gpxpy.parse(gpx_file)
            for track in gpx.tracks:
                track.name = file
                merged.tracks.append(track)
            for route in gpx.routes:
                route.name = file
                merged.routes.append(route)
            for wp in gpx.waypoints:
                merged.waypoints.append(wp)

with open(target_folder + "merge.gpx", "w") as out:
  out.write(merged.to_xml())