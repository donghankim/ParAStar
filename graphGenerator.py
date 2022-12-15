import osmnx as ox
ox.settings.log_console = False
ox.settings.use_cache = True

import numpy as np
import networkx as nx
import pandas as ps
import geopandas as gpd
import matplotlib.pyplot as plt
import os, math, argparse, pdb

# for sanity check
def haversine(coord1, coord2):
    lon1, lat1 = coord1
    lon2, lat2 = coord2
    R = 6371000  # radius of Earth in meters
    phi_1 = math.radians(lat1)
    phi_2 = math.radians(lat2)
    
    delta_phi=math.radians(lat2-lat1)
    delta_lambda=math.radians(lon2-lon1)

    a=math.sin(delta_phi/2.0)**2+\
            math.cos(phi_1)*math.cos(phi_2)*\
            math.sin(delta_lambda/2.0)**2
    c=2*math.atan2(math.sqrt(a),math.sqrt(1-a))

    meters = R*c
    print(meters)


def createTxt(G, TXT_PATH):
    fp = open(TXT_PATH, "w")
    size = G.number_of_nodes()
    gdf_nodes, gdf_edges = ox.graph_to_gdfs(G)
    adj_list = []

    idx = 0    
    for u, outNode in gdf_edges.groupby(level=0):
        ux = G.nodes[u]['x']
        uy = G.nodes[u]['y']
        uName = str(uy) + "&" + str(ux)
        data = str(idx) + ":" + uName + "*"
        tmp = ""
        for v in outNode.iterrows():
            pdb.set_trace()
            vx = G.nodes[v[0][1]]['x']
            vy = G.nodes[v[0][1]]['y']
            vName = str(vy) + "&" + str(vx)
            weight = v[1]['travel_time']
            tmp += vName + "," + str(weight) +";"
        adj_list.append(data+tmp)
        idx += 1

    for line in adj_list:
        fp.write(line + "\n")

    fp.close()
    print("graph .txt saved!")

def saveGraph(G, place):
    DIR_PATH = os.path.join('./graphData/', place.replace(" ", "").strip())
    ML_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".graphml")
    PNG_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".png")
    TXT_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".txt")

    if not os.path.isdir(DIR_PATH):
        os.mkdir(DIR_PATH)
    else:
        print(place + " graph data already exists...")
        return
    
    print("creating graph data for " + place + "...") 
    ox.save_graphml(G, filepath = ML_PATH, gephi = True)
    print(".graphml saved!")
    fig, ax = ox.plot_graph(G, show = False, save = True, close = True, node_color = "r", edge_color = 'g', filepath = PNG_PATH)
    print(".png saved!")
    # createTxt(G, TXT_PATH)
       



def create_new_graph(place):
    G = ox.graph_from_place(place, network_type = "all", simplify = True)
    G = ox.add_edge_speeds(G, hwy_speeds = 5.1) # for generating desired edges
    G = ox.add_edge_travel_times(G)
    saveGraph(G, place)
    
        

def load_graph(place):
    DIR_PATH = os.path.join('./graphData/', place.replace(" ", "").strip())
    ML_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".graphml")
    G = ox.load_graphml(ML_PATH)
    

    
    pdb.set_trace()



if __name__ == '__main__':
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--generate", type = str, help = "enter the name of the place you want to download")
    parser.add_argument("--load",  type = str, help = "load an existing graph (folder name)")
    args = parser.parse_args()

    if args.generate:
        create_new_graph(args.generate)
    elif args.generate:
        load_graph(args.load)
    else:
        print("*** You have to either generate or load a graph ***")
        


