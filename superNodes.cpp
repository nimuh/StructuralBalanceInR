#include <Rcpp.h>
#include <queue>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame create_super_nodes(DataFrame g_v, DataFrame g_e) {

    std::string node("node");
    std::map<std::string, std::string> supers;
    std::map<std::string, std::string> colors;
    std::map<std::string, std::string> signs;
    std::map< std::string, std::vector<std::string> > neigh;
    std::queue<std::string> nias;

    // THIS SECTION INITIATES ALL TABLES NEEDED
    //////////////////////////////////////////////////////////
    // THESE ARE ALL REFERENCES
    // Color assignments 
    CharacterVector color = g_v["color"];

    // Super node assignments
    CharacterVector super = g_v["super"];

    // Node names
    CharacterVector names = g_v["name"];

    // Edges with signs
    CharacterVector from = g_e["from"];
    CharacterVector to = g_e["to"];
    CharacterVector sign = g_e["sign"];

    // use std:: string to convert vector elements to append them
    for (int i = 0; i < names.size(); i++) {
        std::string name = (node + std::string(names[i]));
        supers[name] = super[i];
        colors[name] = color[i];
        nias.push(name);
    }

    for (int i = 0; i < from.size(); i++) {
        std::string edge_name1 = (node + std::string(from[i]) + node + std::string(to[i]));
        std::string edge_name2 = (node + std::string(to[i]) + node + std::string(from[i]));
        signs[edge_name1] = sign[i];
        signs[edge_name2] = sign[i];
    }

    for (int i = 0; i < names.size(); i++) {
        std::vector<std::string> adj;
        for (int j = 0; j < from.size(); j++) {
            if (names[i] == from[j]) {
                std::string nei_name = (node + to[j]);
                adj.push_back(nei_name);
            }
        }
        std::string curr = (node + names[i]);
        neigh[curr] = adj;
    }
    //////////////////////////////////////////////////////////////////
    
    // CORE LOGIC
    /////////////////////////////
    int super_node = 1;
    std::string S;
    std::string curr;
    while(!nias.empty()) {

        S = nias.front();
        nias.pop();
        if (supers[S] != "-1") {
            continue;
        }
        
        std::queue<std::string> Q;
        std::map<std::string, std::string> blacklist;
        Q.push(S);

        while(!Q.empty()) {
            curr = Q.front();
            Q.pop();

            if (blacklist[curr] != curr) {
                colors[curr] = "2";
                supers[curr] = std::to_string(super_node);
                std::vector<std::string> nei = neigh[curr];
                for (int i = 0; i < nei.size(); i++) {
                    std::string n = nei[i];
                    if (supers[n] == "-1") {
                        std::string edge = (curr + n);
                        if (signs[edge] == "1") {
                            if (colors[n] == "1") {
                                colors[n] = "2";
                                Q.push(n);
                            }
                        }
                        else {
                            blacklist[n] = n;
                            colors[n] = "1";
                        }
                    }
                }
            }

        }
        super_node = super_node + 1;

    }

    for (int i = 0; i < names.size(); i++) {
        std::string name = (node + std::string(names[i]));
        super[i] = supers[name];
    }
    DataFrame df = DataFrame::create(Named("names") = names, Named("supers") = super);
    return df;
}
