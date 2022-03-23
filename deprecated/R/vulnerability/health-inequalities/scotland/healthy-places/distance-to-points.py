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
data_dir = "../../../../../data"
boundaries_dir = os.path.join(data_dir, "boundaries")
scotland_dir = os.path.join(data_dir, "vulnerability/health-inequalities/scotland/healthy-places")

# Set CRS
GBcrs = {"init":"epsg:27700"}

# LSOA 2011 Pop Weighted Centroids: https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-population-weighted-centroids
# lsoa_pop_weighted_centroids_file = "Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids.csv"

# ---- Prepare Data ----
# Uncomment this section to generate the ward_centroids.shp data used below. 
"""
# Wards 2019 boundaries
ward_boundaries = "Wards_December_2019_Boundaries_UK_BGC.shp"

# Island wards to exclude - Isle of Scily and Wight wards
island_ward_boundaries = "Wards_December_2019_Boundaries_Scily_Wight_BGC.shp"

# Load ward boundaries and get ward centroids
gdfWards = gpd.read_file(os.path.join(data_dir, "boundaries", ward_boundaries))
gdfWards = gdfWards.to_crs(GBcrs)

# Load island wards - selected manually using QGIS
gdfWardsIsland = gpd.read_file(os.path.join(data_dir, "boundaries", island_ward_boundaries))
gdfWardsIsland = gdfWardsIsland.to_crs(GBcrs)

# Only work with England and Wales wards for now
gdfWards = gdfWards.loc[ (gdfWards['WD19CD'].map(lambda s: s[0] =='E')) | (gdfWards['WD19CD'].map(lambda s: s[0] =='W'))]

# Exclude Isles of Scily and Isle of Whight for now - need to add in ferry crossings to connect with road network
gdfWards = gdfWards.loc[~gdfWards['WD19CD'].isin(gdfWardsIsland['WD19CD'])]

gdfWardsCent = gdfWards.copy()
gdfWardsCent['geometry'] = gpd.points_from_xy(gdfWardsCent.LONG, gdfWardsCent.LAT)
gdfWardsCent.crs = {"init":'epsg:4326'}
gdfWardsCent = gdfWardsCent.to_crs(GBcrs)
gdfWardsCent.to_file(os.path.join(data_dir, "boundaries", "ward_centroids.shp"))
"""

# Uncomment this section to generate the OS Open Road Nodes and Links data used below. 
""" 
# OS Open Roads accessed from: https://www.ordnancesurvey.co.uk/opendatadownload/products.html
open_roads_dir = os.path.join(data_dir, "OS Open Roads/data")

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
"""

# Uncomment this section to generate the OS Open Road Nodes and Links data used below. 
"""
gdfRoadNodes = gpd.read_file(os.path.join(data_dir, "OS Open Road Nodes.shp"))
gdfRoadLinks = gpd.read_file(os.path.join(data_dir,"OS Open Road Links.shp"))
dfNodes = pd.DataFrame({"identifier":gdfRoadNodes.identifier, "x": gdfRoadNodes.geometry.x, "y":gdfRoadNodes.geometry.y})
dfNodes.set_index("identifier", inplace=True)
dfLinks = gdfRoadLinks.loc[:, ['startNode', 'endNode', 'length']]
dfNodes.to_csv(os.path.join(data_dir, "OS Open Road Nodes.csv"))
dfLinks.to_csv(os.path.join(data_dir, "OS Open Road Links.csv"))
gdfRoadNodes = None
gdfRoadLinks = None
"""

# ---- Load Data ----
# LSOA
# dfLSOACent = pd.read_csv(os.path.join(boundaries_dir, lsoa_pop_weighted_centroids_file))
dfLSOACent = gpd.read_file(os.path.join(boundaries_dir, "SG_DataZone_Cent_2011.shp"))
dfLSOACent = dfLSOACent.loc[:, ['DataZone', 'Easting', 'Northing']]
dfLSOACent.columns = ['lsoa11cd', 'X', 'Y']

"""
# Combine England/Wales LSOAs and Scotland Data Zones into one
dfLSOACent2 = dfLSOACent.loc[:, ['lsoa11cd', 'X', 'Y']]
dfDZCent2 = dfDZCent.loc[:, ['DataZone', 'Easting', 'Northing']]
dfDZCent2.columns = ['lsoa11cd', 'X', 'Y']
dfLSOACent = pd.concat([dfLSOACent2, dfDZCent2])
"""

# Load points of interest + change crs
scotlandGeoJSON = gpd.read_file(os.path.join(scotland_dir, "points.geojson"))
scotlandGeoJSON = scotlandGeoJSON.to_crs(GBcrs)

# OS Open Road Nodes and Links
dfNodes = pd.read_csv(os.path.join(boundaries_dir, "OS Open Road Nodes.csv"))
dfLinks = pd.read_csv(os.path.join(boundaries_dir, "OS Open Road Links.csv"))
dfNodes.drop_duplicates(inplace=True)
dfNodes.set_index('identifier', inplace=True)

# ---- Compute Distances ----
# Get largest connected component
edges = dfLinks.loc[:,['startNode','endNode','length']].values
G = nx.Graph()
G.add_weighted_edges_from(edges)
largest_connected_component = sorted(nx.connected_components(G), key = len, reverse=True)[0]

# Clean up to save memory
G = None
edges = None

# Create pandana network. It's much faster for nearest point-of-interest analysis
# Filter nodes and edges to just include those in the largest connected componet
dfLinksLCC = dfLinks.loc[(dfLinks['startNode'].isin(largest_connected_component)) & (dfLinks['endNode'].isin(largest_connected_component))]
dfNodesLCC = dfNodes.loc[largest_connected_component]
net=pdna.Network(dfNodesLCC["x"], dfNodesLCC["y"], dfLinksLCC["startNode"], dfLinksLCC["endNode"], dfLinksLCC[["length"]])

# Get the nearest three points of interest
search_distance = 200000
net.set_pois("points", search_distance, 3, scotlandGeoJSON.geometry.x, scotlandGeoJSON.geometry.y)
dfNear = net.nearest_pois(search_distance, "points", num_pois=3, include_poi_ids=True)

# ---- Ward Computations ----
# Select only the Wards centroids and their nearest foodbanks rather than every road node
"""
ward_road_nodes = net.get_node_ids(gdfWardsCent.geometry.x, gdfWardsCent.geometry.y)
wardComps = dfNear.loc[ward_road_nodes]

# Include Ward codes
gdfWardsCent['ward_road_node'] = ward_road_nodes

# Merge network distances and wards data
wardComps.reset_index(inplace=True)
wardComps = pd.merge(wardComps, gdfWardsCent, left_on = 'identifier', right_on = 'ward_road_node', how = 'outer', indicator = True)

# Calculate mean distance of three food banks
wardComps['mean_distance_nearest_three_foodbanks'] = wardComps.loc[:,[1,2,3]].mean(axis = 1)

# Write to csv
wardComps.to_csv(os.path.join(data_dir, "foodbanks/nearest-foodbanks-wards.csv"), index=False)
"""

# ---- LSOA Computations ----
# Get just the LSOA centroids and their nearest points
lsoa_nodes = net.get_node_ids(dfLSOACent.X, dfLSOACent.Y)
lsoaComps = dfNear.loc[lsoa_nodes]

# Include LSOA codes
dfLSOACent['lsoa_nodes'] = lsoa_nodes

# Merge network distances and LSOA data
lsoaComps.reset_index(inplace=True)
lsoaComps = pd.merge(lsoaComps, dfLSOACent, left_on = 'identifier', right_on = 'lsoa_nodes', how = 'outer', indicator = True)

# Wrangle
lsoaComps['mean_distance_nearest_three_points'] = lsoaComps.loc[:,[1,2,3]].mean(axis = 1)

# Write
lsoaComps.to_csv(os.path.join(scotland_dir, "points-lsoa.csv"), index=False)
