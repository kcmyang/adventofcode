import collections
import operator
import re
import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()


def parse():
    """
    Build the graph as a list of neighbour lists, but index with integers instead of strings.
    Only so many valves can be opened, so group those indices together at the front (see later DP section).
    """
    graph_dict = dict()
    flows_dict = dict()

    for line in lines:
        valves = re.compile(r'[A-Z]{2}').findall(line)
        v, neighbours = valves[0], valves[1:]
        graph_dict[v] = neighbours
        flows_dict[v] = int(re.search(r'\d+', line)[0])

    flows_dict = dict(
        sorted(flows_dict.items(), key=operator.itemgetter(1), reverse=True))

    vertex_map = dict([(v, i) for i, (v, _) in enumerate(flows_dict.items())])
    vertices = list(vertex_map.keys())

    flows = list(flows_dict.values())

    graph = [[] for _ in vertex_map]
    for v, nv in graph_dict.items():
        for u in nv:
            graph[vertex_map[v]].append(vertex_map[u])

    return graph, flows, vertex_map, vertices


graph, flows, vertex_map, vertices = parse()

NUM_VALVES = len(graph)
NUM_OPENABLE_VALVES = flows.index(0)
START_VERTEX = vertex_map['AA']
assert flows[START_VERTEX] == 0
TIME_LIMIT = 26

print(graph)
print(flows)
print(vertices)
print(vertex_map)

# dp[k][v][w][s] = maximum total flow achieved if we are at vertices v and w after k minutes with open valve set s,
# OR s is not in dp[k][v][w] if there is no way to reach vertices v and w after k minutes with open valve set s.
# s is a bitstring where s & 2**v == 1 iff v is in the set of open valves.
# Note the openable valves are the indices in 0..(NUM_OPENABLE_VALVES - 1).
dp = [[[dict() for _ in graph] for _ in graph] for _ in range(TIME_LIMIT + 1)]
empty_set = 0


def update_dp(k, v, w, s, total_flow):
    dp[k][v][w][s] = max(dp[k][v][w].get(s, 0), total_flow)


def put_valve_set(s: int, valve: int) -> int:
    return s | 2**valve


def union_valve_set(s1: int, s2: int) -> int:
    return s1 | s2


def in_valve_set(s: int, valve: int) -> bool:
    return s & 2**valve != 0


def compute_flow(s: int) -> int:
    return sum(flows[v] for v in range(NUM_OPENABLE_VALVES)
               if in_valve_set(s, v))


# fill base case
dp[0][START_VERTEX][START_VERTEX][empty_set] = 0

# fill rest
for k in range(1, TIME_LIMIT + 1):
    for v in range(NUM_VALVES):
        for w in range(NUM_VALVES):
            # current frontier
            for s, total_flow in dp[k - 1][v][w].items():
                total_flow += compute_flow(s)

                # lists of pairs (vertex, valve set)
                v_next = []
                w_next = []

                # handle v

                # if v is openable and not yet open, try opening it
                if flows[v] > 0 and not in_valve_set(s, v):
                    v_next.append((v, put_valve_set(s, v)))

                # try not moving
                v_next.append((v, s))

                # try moving to a neighbour
                v_next.extend((u, s) for u in graph[v])

                # handle w

                # if w is openable and not yet open, try opening it
                if flows[w] > 0 and not in_valve_set(s, w):
                    w_next.append((w, put_valve_set(s, w)))

                # try not moving
                w_next.append((w, s))

                # try moving to a neighbour
                w_next.extend((u, s) for u in graph[w])

                # do all combinations of updates
                for v_, sv in v_next:
                    for w_, sw in w_next:
                        update_dp(k, v_, w_, union_valve_set(sv, sw),
                                  total_flow)

best = max(
    max(dp[TIME_LIMIT][v][w].values()) for v in range(NUM_VALVES)
    for w in range(NUM_VALVES))
print(best)
