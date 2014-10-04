/*
 * file:    pe_054.cc
 * title:   Poker hands
 * author:  Alexander Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 3, 2014
 */

#include <algorithm>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>
using namespace std;

typedef pair<int, char> card_t; // int value, char suit
typedef vector<card_t> hand_t; // a poker hand will always hold 5 cards
typedef pair<int, int> rank_t; // first is a numerical value indicating
                               // the rank of the hand, second is the 
                               // value of the cards in the hand

rank_t rank_hand(hand_t &hand);
bool player_1_wins(hand_t &player_1, hand_t &player_2);;

int main () {
  int player_1_win_count = 0;
  ifstream ifs("../poker.txt");
  for (string line; getline(ifs, line); ) {
    hand_t player_1_hand;
    hand_t player_2_hand;
    istringstream iss(line);
    for (struct { size_t i; string card_string; } token = { 0, "" };
         getline(iss, token.card_string, ' '); token.i++) {
      int card_value;
      switch ( token.card_string[0] ) {
        case 'A':
          card_value = 14;
          break;
        case 'K':
          card_value = 13;
          break;
        case 'Q':
          card_value = 12;
          break;
        case 'J':
          card_value = 11;
          break;
        case 'T':
          card_value = 10;
          break;
        default: 
          card_value = token.card_string[0] - '0';
          break;
      }
      card_t card (card_value, token.card_string[1]);
      if (token.i < 5) {
        player_1_hand.push_back(card);
      } else {
        player_2_hand.push_back(card);
      }
    }
    if (player_1_wins(player_1_hand, player_2_hand)) {
      player_1_win_count++;
    }
  }
  cout << player_1_win_count << endl;
  return 0;
}

/*
 * rank_hand assigns the following ranks to each hand:
 * 0 - High Card        no cards have same value
 * 1 - One Pair         two cards have same value
 * 2 - Two Pairs        two sets of two cards have same value
 * 3 - Three of a Kind  three cards have same value
 * 4 - Straight         all cards are consecutive values
 * 5 - Flush            all cards are of the same suit
 * 6 - Full House       three of a kind and a pair
 * 7 - Four of a Kind   four cards have the same value
 * 8 - Straight Flush   all cards are consecutive values of same suit
 * 9 - Royal Flush      Ten, Jack, Queen, King, and Ace in same suit
 */
rank_t rank_hand(hand_t &hand) {
  rank_t rank = { 0, 0 };
  set<char> suits;
  set<int> values;
  sort(hand.begin(), hand.end());
  for (card_t card : hand) {
    values.insert(card.first);
    suits.insert(card.second);
  }
  if (suits.size() == 1) {
    rank.second = hand[4].first; // some kind of flush
    if ((hand[0].first == 10) && (hand[4].first == 14)) { 
      rank.first = 9; // Royal Flush
    } else if (hand[4].first - hand[0].first == 4) {
      rank.first = 8; // Straight Flush
    } else { 
      rank.first = 5; // Flush
    }
    return rank;
  } else if (values.size() == 2) { // Four of a Kind or Full House
    for (int value : values) {
      int count = count_if(hand.begin(), hand.end(), 
          [value](card_t card) { return (card.first == value); });
      if (count == 4) {
        rank.first = 7; // Four of a Kind
        rank.second = value;
        return rank;
      } else if (count == 3) {
        rank.first = 6; // Full House
        rank.second = value;
        return rank;
      }
    }
  } else if (values.size() == 5) { // Straight or High Card
    if (hand[4].first - hand[0].first == 4) {
      rank.first = 4; // Straight
      rank.second = hand[4].first;
    } else {
      rank.first = 0; // High Card
      rank.second = hand[4].first;
    }
    return rank;
  } else if (values.size() == 3) { // Three of a Kind or Two Pairs
    for (int value : values) {
      int count = count_if(hand.begin(), hand.end(), 
          [value](card_t card) { return (card.first == value); });
      if (count == 3) {
        rank.first = 3; // Three of a Kind
        rank.second = value;
        return rank;
      } else if (count == 1) {
        values.erase(value);
      }
    }
    rank.first = 2; // Two Pairs
    rank.second = *values.rbegin();
    return rank;
  } else if (values.size() == 4) { // hand must be One Pair
    rank.first = 1;
    for (int value : values) {
      int count = count_if(hand.begin(), hand.end(), 
          [value](card_t card) { return (card.first == value); });
      if (count == 2) {
        rank.second = value;
        return rank;
      }
    }
  }
  return rank;
}

bool player_1_wins(hand_t &player_1, hand_t &player_2) {
  rank_t player_1_rank = rank_hand(player_1);
  rank_t player_2_rank = rank_hand(player_2);
  if (player_1_rank > player_2_rank) { // compare rank
    return true;
  } else if (player_1_rank == player_2_rank) { // compare card values
    for (int i = 4; i >= 0; i--) {
      if (player_1[i].first != player_2[i].first) {
        if (player_1[i].first > player_2[i].first) {
          return true;
        } else {
          return false;
        }
      }
    }
    return false;
  }
  return false;
}
