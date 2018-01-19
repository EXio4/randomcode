#include <iostream>
#include <fstream>
#include <map>
#include <vector>
#include <string>

void unscrambler(const std::vector<std::string>& dict, std::string word);

void program(const std::vector<std::string>& dict) {
    
    while (true) {
        std::string chars;
        std::cout << "Write the characters from the word you wanna unscramble (without spaces): " << std::endl;
        std::cout << ">> ";
        std::cin >> chars;

        unscrambler(dict, chars);
    }
}

int main(int argc, char** argv) {
    
    std::vector<std::string> dict;
    {
        std::string dict_file = "dict.txt";
        
        if (argc > 1) {
            dict_file = argv[1];
        }
        
        {
            std::ifstream f(dict_file.c_str());
            if (!f) {
                std::cout << "Error loading dictionary :(" << std::endl;
                return -1;
            }
            while (!f.eof()) {
                std::string line;
                std::getline(f,line);
                dict.push_back(line);
            }
        }
    }
    

    program(dict);
    
    return 0;
}

class Matcher {
private:
    int len;
    std::map<char, int> cs;
public:
    Matcher(const std::string& word) {
        for (auto w : word) {
            cs[w]++;
        }
        len = word.size();
    }
    bool matches(const std::string& str) {
        if (str.size() != len) return false; /* as an optimization, most words will not match the length, so we should avoid building the map */
        std::map<char, int> os;
        for (auto s : str) {
            os[s]++;
        }
        return os == cs;
    }
};

void unscrambler(const std::vector<std::string>& dict, std::string word) {
 
    std::vector<std::string> results;
    Matcher m(word);
    
    for (auto w : dict) {
        if (m.matches(w)) {
            results.push_back(w);
        }
    }
    
    if (results.size() == 0) {
        std::cout << "Word not found :(" << std::endl;
    } else {
        for (auto w : results) {
            std::cout << " - " << w << std::endl;
        }
    }
    
    std::cout << "-----------------------------------" << std::endl;
    
    
}
