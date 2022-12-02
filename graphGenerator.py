import osmnx as ox
ox.settings.log_console = False

import numpy as np
import networkx as nx
import pandas as ps
import geopandas as gpd
import matplotlib.pyplot as plt
import os, pdb


def geocode_address(address, crs=4326):
    geocode = gpd.tools.geocode(address, provider='nominatim', user_agent="drive time demo").to_crs(crs)
    return (geocode.iloc[0].geometry.y, geocode.iloc[0].geometry.x)

def create_new_graph(loc, dist):
    # not very accurate
    # location_point = geocode_address(address)
    # G = ox.graph_from_point(location_point, dist=r, dist_type="bbox", network_type="walk")
    # G = ox.graph_from_point(loc, dist=dist, dist_type="network", network_type="walk")

    G = ox.graph_from_place('Columbia University', network_type='walk')
    G = ox.add_edge_speeds(G, hwy_speeds = 5.1)
    G = ox.add_edge_travel_times(G)
    #ox.save_graphml(G, filepath="./GraphML/manhattan.graphml", gephi=True)
    #fig, ax = ox.plot_graph(G, show=False, save=True, close=True, node_color="r", edge_color = 'g', filepath="./GraphML/manhattan.png")

    size = G.number_of_nodes()
    gdf_nodes, gdf_edges = ox.graph_to_gdfs(G)
    adj_list = []

    # work on this
    idx = 0
    for u, outNode in gdf_edges.groupby(level=0):
        ux = G.nodes[u]['x']
        uy = G.nodes[u]['y']
        uName = str(uy) + "&" + str(ux)
        data = str(idx) + ":" + uName + "***"
        tmp = ""
        for v in outNode.iterrows():
            vx = G.nodes[v[0][1]]['x']
            vy = G.nodes[v[0][1]]['y']
            vName = str(vy) + "&" + str(vx)
            weight = v[1]['travel_time']
            tmp += vName + "," + str(weight) +";"
        adj_list.append(data+tmp)
        idx += 1

    with open("columbia.txt", "w") as fp:
        for line in adj_list:
            fp.write(line + "\n")

    print("graph saved!")

def load_graph(filepath):
    G = ox.load_graphml(filepath)
    return G

def main():
    # not very accruate
    # addr = "Columbia University, New York, 10027"
    # location_point = geocode_address(addr)
    # G = ox.graph_from_point(loc, dist=350, dist_type="network", network_type="walk")
    # fig, ax = ox.plot_graph(G, node_color="r", edge_color = 'g')
    # G = ox.graph_from_place('Columbia University', network_type='walk')
    # G = load_graph("./GraphML/columbia.graphml")

    create_new_graph(0, 0)
    #G = ox.graph_from_place('Columbia University', network_type='walk')
    # ox.save_graphml(G, filepath="./GraphML/manhattan.graphml", gephi=True)
    # G = load_graph("./GraphML/manhattan.graphml")
    #fig, ax = ox.plot_graph(G, show=True, save=False, close=False, node_color="r", node_size = 2.5, edge_color = 'g', filepath="./GraphML/manhattan.png")
    pdb.set_trace()


    # columbia (211, 562)
    # manhattan (42366, 145774)





if __name__ == '__main__':
    main()
