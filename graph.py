#!/usr/bin/env python3

import osmnx as ox
ox.settings.log_console = False
ox.settings.use_cache = True

import geopandas as gpd
import matplotlib.pyplot as plt
import os, shutil, argparse
import pickle, math, tqdm, pdb


DIR_PATH = "./astar/data/"

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


def createAdj(G, place):
    PNG_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".png")
    TXT_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".txt")
    PKL_PATH =  os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".pickle")
    gdf_nodes, gdf_edges = ox.graph_to_gdfs(G)
    
    # osmid hashmap
    size = G.number_of_nodes()
    osmid = gdf_nodes.index.values
    idx = [i for i in range(size)]
    osm_hm = dict(zip(osmid, idx))
    
    # create adjacency list
    adj_list = []
    for uid, edges in tqdm.tqdm(gdf_edges.groupby(level=0), desc = "processing nodes..."):
        uNode = gdf_nodes.loc[uid]
        ux = uNode.x; uy = uNode.y
        node = str(osm_hm[uid]) + ":" + str(uy) + "&" + str(ux) + "*"
       
        data  = ""
        for edge in edges.iterrows():
            vid = edge[0][1]
            edge_weight = edge[1]['travel_time']
            data += str(osm_hm[vid]) + ";" + str(edge_weight) + ","
        
        # remove trailing comma
        if data[-1] == ",":
            data = data[:-1]
        adj_list.append(node+data)
    
    
    # save graph .png
    fig, ax = ox.plot_graph(G, figsize = (20,20), show = False, save = False, close = False, node_color = "r", edge_color = 'g', filepath = PNG_PATH)    
    for node in tqdm.tqdm(gdf_nodes.iterrows(), desc = "adding node labels to .png"):
        text = osm_hm[node[0]]
        c = node[1]["geometry"].centroid
        ax.annotate(text, (c.x, c.y), size = 12, c="white")
    
    # plt.show()
    plt.savefig(f'{PNG_PATH}', dpi = 600)
    
    # save adjacency list
    fp = open(TXT_PATH, "w")
    for line in adj_list:
        fp.write(line + "\n")
    fp.close()

    # save osmid hashmap
    fp = open(PKL_PATH, "wb")    
    pickle.dump(dict(zip(idx, osmid)), fp, protocol=pickle.HIGHEST_PROTOCOL)
    fp.close()

def saveGraph(G, place):
    ML_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".graphml")
      
    try:
        print("creating graph data for " + place + "...") 
        ox.save_graphml(G, filepath = ML_PATH, gephi = False)
        createAdj(G, place)
        print("adjacency graph created!")
    except Exception as e:
        print(e)

def create_new_graph(place):
    print("Retrieving graph from OpenStreetMap...")
    G = ox.graph_from_place(place, network_type = "all", simplify = True)
    G = ox.add_edge_speeds(G, hwy_speeds = 5.1) # for generating desired edges
    G = ox.add_edge_travel_times(G)
    saveGraph(G, place)
    
        
def plot_path(place):
    ML_PATH = os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".graphml")
    PKL_PATH =  os.path.join(DIR_PATH, place.replace(" ", "").strip() + ".pickle")
    
    # load graph
    G = ox.load_graphml(ML_PATH)
    gdf_nodes, _ = ox.graph_to_gdfs(G)
    
    # load idx -> osmid hm
    fp = open(PKL_PATH, 'rb')
    osm_hm = pickle.load(fp)
    fp.close()
    
    # load path
    path = []
    fp = open('./astar/res.txt', 'r')
    lines = fp.read().splitlines()
    path = [osm_hm[int(idx)] for idx in lines]    
    
    # plot path
    fig, ax = ox.plot_graph_route(G, path, show = False, save = False, close = False, node_size = 5, node_color = "r", edge_color = 'g', route_color='b')    
    plt.show()



if __name__ == '__main__':
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-g", type = str, help = "enter place name to download and generate graph data for")
    parser.add_argument("-p",  type = str, help = "enter graph to load (place name)")
    parser.add_argument("--debug", action = "store_true")
    args = parser.parse_args()
    
    if args.g:
        create_new_graph(args.g)
    elif args.p:
        plot_path(args.p) 
    elif args.debug:
        pdb.set_trace()
    else:
        print("*** You have to either generate or load a graph ***")
        


