/*
 * file:    node.h
 * title:   Node class
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 *          alexander.gosselin@alumni.ubc.ca
 * date:    August 23, 2015
 * 
 * license:
 *    GNU General Public License (v3)
 *    <http://www.gnu.org/licenses/gpl-3.0.en.html>
 */

#ifndef _NODE_H
#define _NODE_H

#include <set>


template<typename T>
class Node {
public:
  T data;
  std::set<Node<T>*> edges;
  
  // constructor
  Node(T);
  
  // destructor
  ~Node();
  
  void add_edge(Node<T>*);
  void remove_edge(Node<T>*);
};

template<typename T>
Node<T>::Node(T t) {
  data = t;
}
/*
template<typename T>
Node<T>::Node(Node<T> node) {
  data = node.data;
  edges = node.edges;
}*/

template<typename T>
Node<T>::~Node() {
  // erase all references to Node
  for (auto edge : edges) {
    edge->edges.erase(edge->edges.find(this));
  }
}

template<typename T>
void Node<T>::add_edge(Node<T>* edge) {
  edges.insert(edge);
  edge->edges.insert(this);
}

template<typename T>
void Node<T>::remove_edge(Node<T>* edge)  {
  edges.erase(edges.find(edge));
  edge->edges.erase(edge->edges.find(this));
}

#endif /*_NODE_H*/
