import pandas as pd
import requests
from gpx_converter import Converter
# Get the dataset metadata by passing package_id to the package_search endpoint
# For example, to retrieve the metadata for this dataset:
url = "https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3/action/package_show"
params = { "id": "f2933501-0373-4734-b50c-4e4f39646180"}
package = requests.get(url, params = params).json()
# Take only the data produced for 2021
csv2021 = package["result"]["resources"][0]
# Load csv as pandas
print("download data...")
data = pd.read_csv(csv2021["url"])
# Store a gpx track for each element
for id in data["id"].drop_duplicates():
  storeDf = data[data["id"] == id]
  print("store {0}".format(id))
  Converter.dataframe_to_gpx(input_df=storeDf,
    lats_colname='latitude',
    longs_colname='longitude',
    times_colname='date',
    output_file='toronto-rain-{0}.gpx'.format(id)
  )