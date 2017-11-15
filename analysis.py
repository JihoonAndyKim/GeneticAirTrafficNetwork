import numpy as np
import snap
import matplotlib.pyplot as plt
import random
from geopy.distance import great_circle

traffic = snap.LoadEdgeList(snap.PUNGraph, "data/june_01_2017.txt", 0, 1)
num_nodes =  traffic.GetNodes()
num_edges = traffic.GetEdges()
nodes = [node.GetId() for node in traffic.Nodes()]

PLOTS = False
PRINT = False
#################################
##     Degree Distribution     ##
#################################
histogram = []
DegToCntV = snap.TIntPrV()
snap.GetDegCnt(traffic, DegToCntV)
for item in DegToCntV:
    histogram.append((item.GetVal1(), item.GetVal2()))

x, y = zip(*histogram)

##################################
##    MLLE Estimate of alpha    ##
##################################
def density_function(alpha, xmin, x):
    return ((alpha - 1)/xmin) * ((x/xmin) ** -alpha)

dist = []
for item in histogram:
    dist = dist + [item[0]] * item[1]

n = num_nodes
alpha_mlle = n/sum(np.log(dist)) + 1

if PRINT:
    print "Alpha estimated by the MLLE: ", alpha_mlle

scale_y = [val/float(max(y) * 2.5) for val in y]

#Note the power-law with exponential cutoff
if PLOTS:
    plt.figure()
    plt.title("Log-log plot of empirical distribution")
    plt.xlabel("Degree of nodes")
    plt.ylabel("Number of nodes")
    true_dist = [density_function(alpha_mlle, 1.7, val) for val in x]
    plt.plot(np.log10(x), np.log10(true_dist), color = "r")
    plt.scatter(np.log10(x), np.log10(scale_y))
    plt.show()

######################################
##    Various Network Statistics    ##
######################################
#Centrality measure
centrality = []
for node in traffic.Nodes():
    centrality.append((node.GetId(), snap.GetDegreeCentr(traffic, node.GetId())))

centrality = sorted(centrality, key=lambda x: x[1])
top_5 = list(reversed(centrality[-5:]))
#Atlanta, Chicago O'Hare, Denver, Minneapolis-Saint Paul, Detroit
if PRINT:
    print "Centrality of top 5: ", top_5


#PageRank
page_rank = []
PRankH = snap.TIntFltH()
snap.GetPageRank(traffic, PRankH)
for item in PRankH:
    page_rank.append((item, PRankH[item]))

page_rank = sorted(page_rank, key=lambda x: x[1])
#Atlanta, Chicago O'Hare, Denver, Minneapolis-Saint Paul, Detroit, same cities
top_5_page_rank = list(reversed(page_rank[-5:]))

if PRINT:
    print "PageRank of top 5: ", top_5_page_rank


#Degree
degree = []
for node in traffic.Nodes():
    degree.append((node.GetId(), node.GetDeg()))
degree = sorted(degree, key=lambda x: x[1])
#Atlanta, Chicago O'Hare, Denver, Minneapolis-Saint Paul, Detroit, same cities
top_5_deg = list(reversed(degree[-5:]))

if PRINT:
    print "Degrees of top 5: ", top_5_deg



#Eccentricity
ecc = [snap.GetNodeEcc(traffic, node.GetId(), True)for node in traffic.Nodes()]
ecc_hist, val = np.histogram(ecc)
if PRINT:
    print "Histogram of Eccentricity: ", ecc_hist, val
mean_ecc = sum(ecc)/float(len(ecc))
if PRINT:
    print "Average Eccentricity: ", mean_ecc

###############################
##    Network Robustnesss    ##
###############################
def copy_graph(Graph):
    temp = snap.TUNGraph.New()
    for node in Graph.Nodes():
        temp.AddNode(node.GetId())

    for edge in Graph.Edges():
        temp.AddEdge(edge.GetSrcNId(), edge.GetDstNId())
    return temp

def fraction_scc(Graph, nodes):
    copy = copy_graph(Graph)
    fraction = [snap.GetMxScc(copy).GetNodes()/float(num_nodes)]
    for remove in nodes:
        copy.DelNode(remove)
        fraction.append(snap.GetMxScc(copy).GetNodes()/float(num_nodes))
    return fraction

def robust(x, y):
    prob = [x[i] * y[i] for i, e in enumerate(x)]
    return sum(prob)/len(prob)

def initialize_lookup(path):
    lookup = {}
    f = open(path, 'r')
    for line in f:
        temp = line.replace("\n", "").split('\t')
        lookup[int(temp[0])] = (float(temp[1]), float(temp[2]))
    return lookup

def compute_distance(path, lookup):
    dist = 0
    for i in range(len(path) - 1):
        dist = dist + great_circle(reversed(lookup[path[i+1]]), reversed(lookup[path[i]])).miles
    return dist

def shortest_flight_dist(Graph, lookup):

    average_short_flight = []

    for i in range(100):
        picked = np.random.choice(nodes, 2, replace = False)
        if picked[0] != picked[1]:
            node = picked[0]
            target = picked[1]
            BfsTree = snap.GetBfsTree(Graph, node, True, False)
            search = target
            path = [target]
            edge_src = [edge.GetSrcNId() for edge in BfsTree.Edges()]
            edge_dst = [edge.GetDstNId() for edge in BfsTree.Edges()]

            while search != node:
                ind = edge_dst.index(search)
                search = edge_src[ind]
                path.append(search)

            dist = compute_distance(path, lookup)
            average_short_flight.append(dist)

    return sum(average_short_flight)/len(average_short_flight)


#Targeting centrality, PageRank and degree. These nodes will be removed in order
centrality_nodes = zip(*list(reversed(centrality)))[0]
page_rank_nodes = zip(*list(reversed(page_rank)))[0]
degree_nodes = zip(*list(reversed(degree)))[0]

centrality_robust = fraction_scc(traffic, centrality_nodes)
page_rank_robust = fraction_scc(traffic, page_rank_nodes)
degree_robust = fraction_scc(traffic, degree_nodes)

x_ = [val/float(num_nodes) for val in range(len(centrality_robust))]

if PLOTS:
    plt.figure()
    axes = plt.gca()
    axes.set_xlim([0,0.2])
    plt.plot(x_, centrality_robust)
    plt.plot(x_, page_rank_robust)
    plt.plot(x_, degree_robust)
    plt.title("Size of largest SCC vs. percentage of network removed")
    plt.xlabel("Percentage of Network Removed")
    plt.ylabel("Size of Largest SCC")
    plt.legend(["Centrality", "PageRank", "Degree"])
    plt.show()

#Moving forward, we will use PageRank for robustness
print robust(x_, page_rank_robust)

lookup = initialize_lookup("data/lat_lon_lookup.txt")
print shortest_flight_dist(traffic, lookup)
