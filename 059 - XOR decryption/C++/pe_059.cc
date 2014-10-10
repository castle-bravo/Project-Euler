/*
 * file:    pe_059.cc
 * title:   XOR decryption
 * author:  Alexander P. Gosselin
 * e-mail:  alexandergosselin@gmail.com
 * date:    October 9, 2014
 */

#include <fstream>
#include <iostream>
#include <string>
using namespace std;

string decode(string &cipher, char (&key)[3]);

int main() {
  string cipher; // ciphertext
  ifstream ifs ("../cipher.txt");
  for (string n; getline(ifs, n, ','); ) {
    cipher.push_back(stoi(n));
  }
  ifs.close();
  for (char x = 'a'; x <= 'z'; x++) {
    for (char y = 'a'; y <= 'z'; y++) {
      for (char z = 'a'; z <= 'z'; z++) {
        char key[3] = { x, y, z };
        string plain = decode(cipher, key);
        if (plain.size() == cipher.size()) {
          //cout << string(key, key + 3) << endl;
          //cout << plain << endl; 
          int ascii_sum = 0;
          for (char c : plain) {
            ascii_sum += c;
          }
          cout << ascii_sum << endl;;
          return 0;
        }
      }
    }
  }
  return 0;
}

string decode(string &cipher, char (&key)[3]) {
  string plain;
  plain.reserve(cipher.length());
  for (size_t i = 0; i < cipher.length(); i++) {
    char c = cipher[i] ^ (key[i % 3]);
    if (c < ' ' || c > 'z' || (c > '\"' && c < '\'')) break;
    plain.push_back(c);
  }
  return plain;
}
