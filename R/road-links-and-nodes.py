##
## This script will generate supporting files for calculating road distances to points of interest
## (such as pharmacies or sports centres)
##
## Before running this code, you'll need to download the OS Open Roads dataset from https://www.ordnancesurvey.co.uk/opendatadownload/products.html
## ... and unzip it into this folder: `resilience-index\data\boundaries\OS Open Roads`
##
## This script creates two output files in `resilience-index\data\boundaries`:
## - OS Open Road Nodes.csv
## - OS Open Road Links.csv
##
## These files will be used by the .py scripts that calculate road distances to places.
##

# ---- Import libraries ----
import numpy as np
import pandas as pd
import geopandas as gpd
import os
from scipy.spatial import cKDTree
import networkx as nx
import pandana as pdna

# ----  Globals & Functions ----
# Set datat dir
data_dir = "../data"
boundaries_dir = os.path.join(data_dir, "boundaries")

# Set CRS
GBcrs = {"init":"epsg:27700"}

# ---- Generate the OS Open Road Nodes and Links data ----
# OS Open Roads accessed from: https://www.ordnancesurvey.co.uk/opendatadownload/products.html
open_roads_dir = os.path.join(boundaries_dir, "OS Open Roads/data")

road_node_files = [i for i in os.listdir(open_roads_dir) if "RoadNode.shp" in i]
gdfRoadNodes = gpd.GeoDataFrame()
for f in road_node_files:
    gdf = gpd.read_file(os.path.join(open_roads_dir, f))
    gdf['file'] = f
    gdfRoadNodes = pd.concat([gdfRoadNodes, gdf])
gdfRoadNodes = gdfRoadNodes.to_crs(GBcrs)
gdfRoadNodes.to_file(os.path.join(data_dir, "OS Open Road Nodes.shp"))

road_link_files = [i for i in os.listdir(open_roads_dir) if "RoadLink.shp" in i]

gdfRoadLinks = gpd.GeoDataFrame()
for f in road_link_files:
    gdf = gpd.read_file(os.path.join(open_roads_dir, f))
    gdfRoadLinks = pd.concat([gdfRoadLinks, gdf])
gdfRoadLinks = gdfRoadLinks.to_crs(GBcrs)
gdfRoadLinks.to_file(os.path.join(data_dir,"OS Open Road Links.shp"))

gdfRoadNodes = gpd.read_file(os.path.join(data_dir, "OS Open Road Nodes.shp"))
gdfRoadLinks = gpd.read_file(os.path.join(data_dir,"OS Open Road Links.shp"))
dfNodes = pd.DataFrame({"identifier":gdfRoadNodes.identifier, "x": gdfRoadNodes.geometry.x, "y":gdfRoadNodes.geometry.y})
dfNodes.set_index("identifier", inplace=True)
dfLinks = gdfRoadLinks.loc[:, ['startNode', 'endNode', 'length']]
dfNodes.to_csv(os.path.join(data_dir, "OS Open Road Nodes.csv"))
dfLinks.to_csv(os.path.join(data_dir, "OS Open Road Links.csv"))
gdfRoadNodes = None
gdfRoadLinks = None
