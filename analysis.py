import numpy as np
import snap
import matplotlib.pyplot as plt
import random
from geopy.distance import great_circle
from scipy.stats import bernoulli
from joblib import Parallel, delayed
from multiprocessing import Pool

traffic = snap.LoadEdgeList(snap.PUNGraph, "data/june_01_2017.txt", 0, 1)
num_nodes =  traffic.GetNodes()
num_edges = traffic.GetEdges()
nodes = [node.GetId() for node in traffic.Nodes()]

PLOTS = False
PRINT = True
PRELIM_STATS = False

if PRELIM_STATS:
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
##    Network Robustness    ##
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
        id_air = int(temp[0].replace('"', ''))
        lon = temp[1].replace('"', '')
        lat = temp[2].replace('"', '')
        lookup[id_air] = (float(lon), float(lat))
    return lookup

def compute_distance(path, lookup):
    dist = 0
    try:
        for i in range(len(path) - 1):
            dist = dist + great_circle(reversed(lookup[path[i+1]]), reversed(lookup[path[i]])).miles
    except:
        return dist
    return dist

def compute_distance_hazard(hazard, query, lookup):
    return great_circle(reversed(hazard), reversed(lookup[query])).miles

def compute_shortest_distance(Graph, node_list, lookup):
    picked = np.random.choice(node_list, 2, replace = False)
    if picked[0] != picked[1]:
        node = picked[0]
        target = picked[1]
        BfsTree = snap.GetBfsTree(Graph, node, True, False)
        search = target
        path = [target]
        edge_src = [edge.GetSrcNId() for edge in BfsTree.Edges()]
        edge_dst = [edge.GetDstNId() for edge in BfsTree.Edges()]

        while search != node:
            try:
                ind = edge_dst.index(search)
                search = edge_src[ind]
                path.append(search)
            except:
                break

        return compute_distance(path, lookup)

def shortest_flight_dist(Graph, lookup, sig = False):

    average_short_flight = []
    node_list = [node.GetId() for node in Graph.Nodes()]
    if sig:
        for i in range(1000):
            average_short_flight.append(compute_shortest_distance(Graph, node_list, lookup))
    else:
        for i in range(100):
            average_short_flight.append(compute_shortest_distance(Graph, node_list, lookup))

    if sig:
        print np.std(average_short_flight)
    return sum(average_short_flight)/len(average_short_flight)

def compute_robust(network):
    page_rank = []
    PRankH = snap.TIntFltH()
    snap.GetPageRank(network, PRankH)
    for item in PRankH:
        page_rank.append((item, PRankH[item]))

    page_rank = sorted(page_rank, key=lambda x: x[1])
    page_rank_nodes = zip(*list(reversed(page_rank)))[0]
    page_rank_robust = fraction_scc(network, page_rank_nodes)

    x_ = [val/float(network.GetNodes()) for val in range(len(page_rank_robust))]
    return robust(x_, page_rank_robust)

def robustness_plot(network):
    page_rank = []
    PRankH = snap.TIntFltH()
    snap.GetPageRank(network, PRankH)
    for item in PRankH:
        page_rank.append((item, PRankH[item]))

    page_rank = sorted(page_rank, key=lambda x: x[1])
    page_rank_nodes = zip(*list(reversed(page_rank)))[0]
    page_rank_robust = fraction_scc(network, page_rank_nodes)

    x_ = [val/float(network.GetNodes()) for val in range(len(page_rank_robust))]
    return (x_, page_rank_robust)

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
#print robust(x_, page_rank_robust)

#Average Shortest Flight Distance Metric
lookup = initialize_lookup("data/lat_lon_lookup.txt")
#print shortest_flight_dist(traffic, lookup)

#############################
##    Genetic Algorithm    ##
#############################
N = num_nodes
E = num_edges

ind_to_id = {}
f = open("data/lat_lon_lookup.txt", 'r')
for i, line in enumerate(f):
    temp = line.replace("\n", "").split('\t')
    ind_to_id[i] = int(temp[0].replace('"', ''))


ind = np.triu_indices(num_nodes)
mapping = []
for i in range(len(ind[0])):
    if ind[0][i] != ind[1][i]:
        mapping.append((ind_to_id[ind[0][i]], ind_to_id[ind[1][i]]))

def graph_to_chromosome(Graph, mapping):

    def find_element_in_list(element, list_element):
        try:
            index_element = list_element.index(element)
            return index_element
        except ValueError:
            return None

    chromosome = np.zeros(Graph.GetNodes() * (Graph.GetNodes() - 1) / 2)

    for edge in Graph.Edges():
        ind =  find_element_in_list((edge.GetSrcNId(), edge.GetDstNId()), mapping)
        if not ind:
            ind = find_element_in_list((edge.GetDstNId(), edge.GetSrcNId()), mapping)

        chromosome[ind] = 1

    return chromosome

def chromosome_to_graph(chrome, mapping):
    temp = snap.TUNGraph.New()

    already_added = set()
    for i, val in enumerate(chrome):
        if mapping[i][0] not in already_added:
            temp.AddNode(mapping[i][0])
            already_added.add(mapping[i][0])
        if mapping[i][1] not in already_added:
            temp.AddNode(mapping[i][1])
            already_added.add(mapping[i][1])
        if val:
            temp.AddEdge(mapping[i][0], mapping[i][1])

    return temp

def simulation(N, E, Graph, mapping, lookup):

    def evolve(generation, mapping, lookup, iterations, constraint):

        def split(a, n):
            k, m = divmod(len(a), n)
            return (a[i * k + min(i, m):(i + 1) * k + min(i + 1, m)] for i in xrange(n))

        for k in range(iterations):
            print "iteration: ", k
            n = 4
            groups = [generation[i:i + n] for i in xrange(0, len(generation), n)]
            indices = [0]*n
            for i, group in enumerate(groups):
                score = 10000
                for j, chromosome in enumerate(group):
                    graph = chromosome_to_graph(chromosome, mapping)
                    #Normalize with respect to robustness metric
                    val = shortest_flight_dist(graph, lookup)/float(200 * constraint) + compute_robust(graph)
                    if val < score:
                        indices[i] = i*n + j
                        score = val

            parents = [generation[i] for i in indices]
            pairs = list(split(parents, 2))

            new_chromosomes = []
            for i in range(len(pairs[0])):
                new_chromosomes = new_chromosomes + breed(pairs[0][i], pairs[1][i], len(generation)/2, constraint)

            generation = new_chromosomes

        score = 10000
        index = 0
        for j, chromosome in enumerate(generation):
            graph = chromosome_to_graph(chromosome, mapping)
            val = shortest_flight_dist(graph, lookup)
            if val < score:
                index = j
                score = val

        return generation[index]

    def breed(chromosome1, chromosome2, num_child, constraint):
        children = []
        for i in range(num_child):
            cut = random.randrange(0,len(chromosome1))
            child = np.concatenate((chromosome1[:cut], chromosome2[cut:]))
            child = correct_diff(child)
            children.append(mutation(child, constraint))
        return children

    def correct_diff(chromosome):
        flights_diff = sum(chromosome) - constraint
        if flights_diff > 0:
            indices = [i for i, x in enumerate(chromosome) if x == 1]
            change = np.random.choice(indices, flights_diff, replace = False)
            for item in change:
                chromosome[change] = 0
        else:
            indices = [i for i, x in enumerate(chromosome) if x == 0]
            change = np.random.choice(indices, -flights_diff, replace = False)
            for item in change:
                chromosome[change] = 1

        return chromosome

    def mutation(chromosome, constraint):
        mutation_rate = len(chromosome)
        mutate = chromosome

        for i, bit in enumerate(chromosome):
            if np.random.uniform() < 1/float(mutation_rate):
                random_ind = int(random.randrange(0,mutation_rate-1))
                temp = chromosome[random_ind]
                mutate[random_ind] = chromosome[i]
                mutate[i] = temp

        return mutate

    def initialize_chromosomes(Graph, constraint):
        gen = []

        chromosome = graph_to_chromosome(Graph, mapping)
        for i in range(16):
            mutate = mutation(chromosome, constraint)
            gen.append(mutate)

        return gen

    constraint = E
    first_gen = initialize_chromosomes(Graph, constraint)

    winning_chromosome = evolve(first_gen, mapping, lookup, 100, constraint)

    return chromosome_to_graph(winning_chromosome, mapping)

#graph = simulation(N, E, traffic, mapping, lookup)
#snap.SaveEdgeList(graph, 'mygraph.txt')

#print shortest_flight_dist(traffic, lookup, True)
#print shortest_flight_dist(graph, lookup, True)
#LoadedUGraph = snap.LoadEdgeList(snap.PUNGraph, "good.txt", 0, 1, '\t')
LoadedUGraph = snap.LoadEdgeList(snap.PUNGraph, "mygraph.txt", 0, 1, '\t')

x1, y1 = robustness_plot(traffic)
x2, y2 = robustness_plot(LoadedUGraph)
frac = len(x1)/5
print "Robustness", robust(x1[0:frac], y1[0:frac])
print "Robustness optimized", robust(x2[0:frac], y2[0:frac])
plt.figure()
plt.plot(x1[0:frac], y1[0:frac])
plt.plot(x2[0:frac], y2[0:frac])
plt.legend(["Original", "Optimized"])
plt.show()
#####################################
#### Modeling the Spatial Hazard ####
#####################################
#Simulating an eruption at Yellowstone
yellowstone = (-110.5885, 44.4280)
hazard_distance = 500
hazardGraph = snap.LoadEdgeList(snap.PUNGraph, "data/june_01_2017.txt", 0, 1)
nodes_removed = []
for node in hazardGraph.Nodes():
    if compute_distance_hazard(yellowstone, node.GetId(), lookup) <= hazard_distance:
        nodes_removed.append(node.GetId())
        hazardGraph.DelNode(node.GetId())
#print shortest_flight_dist(hazardGraph, lookup, True)

N = hazardGraph.GetNodes()
E = hazardGraph.GetEdges()

hazard_id = {}
new_nodes = []
for node in nodes:
    if node not in nodes_removed:
        new_nodes.append(node)

for i, node in enumerate(new_nodes):
    hazard_id[i] = node

ind = np.triu_indices(N)
hazard_mapping = []
for i in range(len(ind[0])):
    if ind[0][i] != ind[1][i]:
        hazard_mapping.append((hazard_id[ind[0][i]], hazard_id[ind[1][i]]))


#graph = simulation(N, E, hazardGraph, hazard_mapping, lookup)
#snap.SaveEdgeList(graph, 'hazard.txt')
hazard = snap.LoadEdgeList(snap.PUNGraph, "hazard.txt", 0, 1, '\t')
print shortest_flight_dist(hazard, lookup, True)

x1, y1 = robustness_plot(hazardGraph)
x2, y2 = robustness_plot(hazard)
frac = len(x1)/5
print "Robustness", robust(x1[0:frac], y1[0:frac])
print "Robustness optimized", robust(x2[0:frac], y2[0:frac])
plt.figure()
plt.plot(x1[0:frac], y1[0:frac])
plt.plot(x2[0:frac], y2[0:frac])
plt.legend(["Original", "Optimized"])
plt.show()

#snap.SaveEdgeList(hazardGraph, 'hazard_original.txt')


#PageRank for new spatial model
page_rank = []
PRankH = snap.TIntFltH()
snap.GetPageRank(hazard, PRankH)
for item in PRankH:
    page_rank.append((item, PRankH[item]))

page_rank = sorted(page_rank, key=lambda x: x[1])
#Atlanta, Chicago O'Hare, Denver, Minneapolis-Saint Paul, Detroit, same cities
top_5_page_rank = list(reversed(page_rank[-5:]))

#10397, 13930, 11433, 13487, 12266
#ATL, ORD, DTW, MSP, IAH
if PRINT:
    print "PageRank of top 5, hazard: ", top_5_page_rank




####################################
#### Modeling a Targeted Attack ####
####################################
targeted_attack = zip(*top_5_page_rank)[0]
targetedGraph = snap.LoadEdgeList(snap.PUNGraph, "data/june_01_2017.txt", 0, 1)
targeted_removed = []
for node in targeted_attack:
    targeted_removed.append(node)
    targetedGraph.DelNode(node)

print targeted_removed
print shortest_flight_dist(targetedGraph, lookup, True)
snap.SaveEdgeList(targetedGraph, 'targeted_original.txt')

N = targetedGraph.GetNodes()
E = targetedGraph.GetEdges()

targeted_id = {}
new_nodes = []
for node in nodes:
    if node not in targeted_removed:
        new_nodes.append(node)

for i, node in enumerate(new_nodes):
    targeted_id[i] = node

ind = np.triu_indices(N)
targeted_mapping = []
for i in range(len(ind[0])):
    if ind[0][i] != ind[1][i]:
        targeted_mapping.append((targeted_id[ind[0][i]], targeted_id[ind[1][i]]))

#graph = simulation(N, 1884, targetedGraph, targeted_mapping, lookup)
#snap.SaveEdgeList(graph, 'targeted.txt')
targeted = snap.LoadEdgeList(snap.PUNGraph, "targeted.txt", 0, 1, '\t')
print shortest_flight_dist(targeted, lookup, True)

x1, y1 = robustness_plot(targetedGraph)
x2, y2 = robustness_plot(targeted)
frac = len(x1)/5
print "Robustness", robust(x1[0:frac], y1[0:frac])
print "Robustness optimized", robust(x2[0:frac], y2[0:frac])
plt.figure()
plt.plot(x1[0:frac], y1[0:frac])
plt.plot(x2[0:frac], y2[0:frac])
plt.legend(["Original", "Optimized"])
plt.show()



#PageRank for new targeted model
page_rank = []
PRankH = snap.TIntFltH()
snap.GetPageRank(targeted, PRankH)
for item in PRankH:
    page_rank.append((item, PRankH[item]))

page_rank = sorted(page_rank, key=lambda x: x[1])
#Atlanta, Chicago O'Hare, Denver, Minneapolis-Saint Paul, Detroit, same cities
top_5_page_rank = list(reversed(page_rank[-5:]))

#14869, 12266, 11298, 14107, 11618
#SLC, IAH, DFW, PHX, EWR
if PRINT:
    print "PageRank of top 5, hazard: ", top_5_page_rank
