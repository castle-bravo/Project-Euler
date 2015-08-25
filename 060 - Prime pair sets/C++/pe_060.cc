/*
 * file:    pe_060.cc
 * title:   Prime pair sets
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    August 24, 2015
 * 
 * link:    https://projecteuler.net/problem=60
 */


#include <algorithm>
#include <iostream>
#include <set>
#include <vector>

#include "miller_rabin.h"
#include "node.h"

const int SET_SIZE = 5; // size of the prime pair set to return

std::vector<int> primes({2, 3});

int next_prime() {
  for (int n = primes.back() + 2; ; n += 2) {
    for (auto p : primes) {
      if (n % p == 0) break;
      if (p*p > n) {
        primes.push_back(n);
        return n;
      }
    }
  }
}

bool are_pair(int a, int b) {
  std::string as = std::to_string(a);
  std::string bs = std::to_string(b);
  return miller_rabin(stoi(as + bs)) && miller_rabin(stoi(bs + as));
}

int graph_sum(std::set<Node<int>*> &graph) {
  int sum = 0;
  for (auto node : graph) {
    sum += node->data;
  }
  return sum;
}

// find a clique in a graph recursively
std::set<Node<int>*> find_clique(std::set<Node<int>*> &graph,
                                 size_t size) {
  if (graph.size() < size) { // failed to find clique
    return std::set<Node<int>*>();
  } 
  if (graph.size() == 1) {   // graph is a clique
    return graph;
  }
  
  for (auto node = graph.begin(); node != std::prev(graph.end(), size); 
       ++node) {
    std::set<Node<int>*> mutual;
    std::set_intersection(
        (*node)->edges.begin(), (*node)->edges.end(),
        graph.begin(), graph.end(),
        std::inserter(mutual, mutual.begin()));
    if (mutual.size() < size - 1) continue;
    std::set<Node<int>*> clique = find_clique(mutual, size - 1);
    if (clique.size() > 0) {  // a clique has been found
      clique.insert(*node);
      return clique;
    }
  }
  
  return std::set<Node<int>*>();
}

int main() {
  
  // initialize prime pair graph
  std::vector<Node<int>*> graph({new Node<int>(primes.back())});
  
  while (true) {
    Node<int>* node = new Node<int>(next_prime());
    // build set of edges for new node
    for (auto n : graph) {
      if (are_pair(node->data, n->data)) {
        node->add_edge(n);
      }
    }
    
    // 
    if (node->edges.size() >= SET_SIZE - 1) {
      std::set<Node<int>*> neighborhood(node->edges);
      neighborhood.insert(node);
      std::set<Node<int>*> clique = find_clique(neighborhood, 
                                                SET_SIZE - 1);
      if (clique.size() >= SET_SIZE) {
        for (auto n : clique) {
          std::cout << n->data << ", ";
        } std::cout << std::endl;
        std::cout << "sum: " << graph_sum(clique) << std::endl;
        break; 
      }
    }
    graph.push_back(node);
  }
  
  /*for (auto node : graph) {
    delete node;
  }*/
  
  return 0;
}
