//
// GraphTools.cpp
// Some tools for graph calculation. Graphtools is a class which contains two individual class
// Floyd and Dijiastra. And both of it can calculate the shortest path. The Dijiastra can calculate the
// betweeness since it need the number of the shortest path from one node to the other.
//
//The programme will choose Floyd or other matrix-base algorithm when the max-degree(n) of the matrix is satisfy the relationship n*n*n > size*size
//Since the matix-base algorithm have a better performance in a density matix.
//
// First sub-class is Floyd which provide some ports to calculate shortest path.
// Floyd
// --Floyd(calculation)
// --FloydBasedBetweeness(choose the node with high betweeness as priority)
//
// Second sub-class is dijistra which provide some ports to calculate shortest path and betweeness
// Dijiastra
// --class Node
// --redifinition of how to compare Node
// --Dijisstra(constructor)
// --resetNet
// --DijiastraCalculation
// --claculatePath
//
// class node is a sub-class of Dijiastra and contains pointer to node's father and record of its neighbor.
// Node
// --Node(constructor)
//
//5 output function
//metric_global_shortestPath //calculate shortest path
//metric_global_shortestDetails //calculate the path chain
//metric_global_shortestDetailsBasedBetween //calculate path chain base on betweenness
//metric_node_betweeness //calculte betweenness
//metric_global_triangle //calculate triangle
//
//
//
// Corect GraphTool file
//[Rcpp::depends(RcppArmadillo)]
#include <iostream>
#include <vector>
#include <set>
#include <queue>

#include <RcppArmadillo.h>
#include <map>
using namespace std;
using namespace Rcpp;

const double metric_global_DISTMAX = R_PosInf;

class metric_global_GraphTools{
private:
    double* distMap;        //the matrix of distance between each node
    double* shortestMap;    //the matrix of shortest path between each node
    double* betweenness;    //the matrix of betweenness(it will not be initialized if it is not needed
    long int* directMap;    //the matrix of the shortest path
    long int size;          //the number of nodes
    bool isGraphSym;        //a bool value shows whether the matrix is symmetry
    int taskType;       //a bool value shows whether the betweenness is needed 0:just shortestPath 1:betweenness 2:triangle
    long int triangles;
    long int* degree;
    bool sparseGraph;
    
    class Floyd{
    public:
        Floyd(long int size, double* distMap, double* shortestMap){
            for (int i = 0; i < size*size; i++) {
                shortestMap[i] = distMap[i];
            }
            for(int k = 0;k < size; k++){
                for(int i = 0; i < size; i++){
                    for(int j = 0; j < size; j++){
                        if(i == j || i == k || j == k) continue;
                        if(shortestMap[i * size + j] > (shortestMap[i * size + k] + shortestMap[k * size + j])){
                            shortestMap[i * size + j] = shortestMap[i * size + k] + shortestMap[k * size + j];
                        }
                    }
                }
            }
        }
        
        Floyd(long int size, double* distMap, double* shortestMap, long int* directMap){
            for (int i = 0; i < size*size; i++) {
                shortestMap[i] = distMap[i];
            }
            for(int k = 0;k < size; k++){
                for(int i = 0; i < size; i++){
                    for(int j = 0; j < size; j++){
                        if(i == j || i == k || j == k) continue;
                        if(shortestMap[i * size + j] > (shortestMap[i * size + k] + shortestMap[k * size + j])){
                            shortestMap[i * size + j] = shortestMap[i * size + k] + shortestMap[k * size + j];
                            directMap[i * size + j] = k;
                        }
                    }
                }
            }
            for(int k = 0;k < size; k++){
                for(int i = 0; i < size; i++){
                    for(int j = 0; j < size; j++){
                        if(i == j || i == k || j == k) continue;
                        if (shortestMap[i * size + j] == (shortestMap[i * size + k] + shortestMap[k * size + j])) {
                            if (shortestMap[directMap[i*size + j]*size + j] > shortestMap[k*size + j]){
                                directMap[i * size + j] = k;
                            }// this if will choose the short edge for priority. For example, A->B->C and A->C,
                            //the distance is equal and it will choose B as A's father.
                        }
                        else if(shortestMap[i * size + j] > (shortestMap[i * size + k] + shortestMap[k * size + j])){
                            shortestMap[i * size + j] = shortestMap[i * size + k] + shortestMap[k * size + j];
                            directMap[i * size + j] = k;
                        }
                    }
                }
            }
        }//do floyd calculation
        //why I do the shortest path calculation twice. That because the floyd calculate the shortest path one
        //by one which means. Although the value of shortest path is correct, the father node of some nodes will
        //be wrong. A->B->C will be A->C and there is no edge between A and C. A->B+B->C = A->C and C is larger
        //than B. This case will happened. So I will apply floyd another time in order to make sure it is correct.
    };
    
    class FloydBasedBetweenness{
    public:
        FloydBasedBetweenness(double* betweenness, long int size, double* distMap, double* shortestMap, long int* directMap){
            for(int k = 0;k < size; k++){
                for(int i = 0; i < size; i++){
                    for(int j = 0; j < size; j++){
                        if(i == j || i == k || j == k) continue;
                        if (shortestMap[i * size + j] == (shortestMap[i * size + k] + shortestMap[k * size + j])) {
                            if (shortestMap[directMap[i*size + j]*size + j] > shortestMap[k*size + j]){
                                directMap[i * size + j] = k;
                            }
                        }
                        if (shortestMap[i * size + j] == (shortestMap[i * size + k] + shortestMap[k * size + j])) {
                            if (betweenness[directMap[i*size + j]] < betweenness[k]){
                                directMap[i * size + j] = k;
                            }
                        }
                    }
                }
            }
        }//do floyd calculation
        //since the shortest path algorithm always choose the last node it accesses to. So it may be necessary
        //to make the node with higher betweeness popular. And in this function, the node with higher betweeness
        //will be treat as priority
    };
    
    class Dijiastra{
    private:
        class Node{
        private:
            int name;               //name
            Node* shortestFather;   //who is your father:)
            double distance;        //the current distance from the start node to here
            //std::vector<std::pair<Node*, double> > neighbor;
            int state; //0 unuse, 1 searching, 2 finish
            long int degree;
        public:
            std::vector<std::pair<Node*, double> > neighbor;// a list of node which is connected to current node
            vector<Node*> shortestFatherList;               // a list of node which is the father of current node
            // and a member of the shortest path
            vector<Node*> shortestChildList;                // a list of node which is the child of current node
            // and a member of the shortest path
            
            Node(int name){
                this->name = name;
                shortestFather = NULL;
                distance = metric_global_DISTMAX;
                state = 0;
                degree = 0;
            }//init every node, state represent the access state
            
            Node(int name, long int degreeValue){
                this->name = name;
                shortestFather = NULL;
                distance = metric_global_DISTMAX;
                state = 0;
                degree = degreeValue;
            }//init every node, state represent the access state
            
            void addNeighbor(Node* nei, double dis){
                this->neighbor.push_back(std::make_pair(nei, dis));
            }//add neighbor to the list, with distance of the neighbor
            
            std::pair<Node*, double> getNeighbor(int index){
                return this->neighbor[index];
            }//get the specific neighbor in the list
            
            int getName() const {
                return name;
            }//return the name of the node
            
            long int getDegree() const {
                return degree;
            }
            
            void setFather(Node* father){
                this->shortestFather = father;
            }//set the father of current node for path tracking
            
            Node* whoisYourFather(){
                return this->shortestFather;
            }//tell others who is your father
            
            int compDistance(double dis, Node* targetFather){
              if (dis - this->distance > std::numeric_limits<double>::epsilon()) {
                return 0;
              }
              else if (abs(dis - this->distance) > std::numeric_limits<double>::epsilon()){
                return 2;
              }
              return 1;
            }//check if the new node is shorter than the current node, if it is
            //update the current father node to new node
            
            void setDistance(double dis){
                this->distance = dis;
            }//set the distance as the input(normally it should not be used
            
            const int numberofNeighbor(){
                return (int)neighbor.size();
            }//return the number of neighbor for starting a loop or something else
            
            void changeState(){
                if (this->state == 0) {
                    state++;
                }
                else if (this->state == 1){
                    state++;
                }
                else return;
            }//change the state of the node from new to access and finished
            
            void resetNode(){
                state = 0;
                distance = metric_global_DISTMAX;
                shortestFather = NULL;
                shortestChildList.clear();
                shortestFatherList.clear();
            }//reset function of the node for reset the network
            
            bool askState(int sta){
                return state==sta;
            }//tell other functions about the state of the node
            
            double getDistance() const {
                return this->distance;
            }//return the current distance of the node
        };
        
    public:
        std::vector<Node*> listofNode; //the list of node in the whole network
        
        class compareNode: binary_function<Node*, Node*, bool>{
        private:
            bool reverse;
        public:
            compareNode(const bool& reverse = false){
                this->reverse = reverse;
            }// prepare for reverse, which means make the map rank from large to small
            bool operator()(const Node* const &a, const Node* const &b) const{
                if (reverse) {
                    if (a->getDistance() == b->getDistance()) {
                        return a->getName() > b->getName();
                    }
                    return a->getDistance() > b->getDistance();
                }
                else {
                    if (a->getDistance() == b->getDistance()) {
                        return a->getName() < b->getName();
                    }
                    return a->getDistance() < b->getDistance();
                }
            }
        };//define a new class which compare the node and make the priority_queue works as we want
        
        Dijiastra(long int size, double* distMap, double* shortestMap, long int* directMap){
            initNet(size, distMap);
            
            for (int i = 0; i < size; i++) {
                DijastraCalculation(i, shortestMap, size, directMap);
            }//start calculation
        }
        
        Dijiastra(double* betweenness, long int size, double* distMap, double* shortestMap, long int* directMap){
            for (int i = 0; i < size; i++) {
                betweenness[i] = 0;
            }//init all node
            
            initNet(size, distMap);
            
            for (int i = 0; i < size; i++) {
                DijastraCalculation(i, shortestMap, betweenness, size, directMap);
            }//start calculation
        }
        
        Dijiastra(long int size, double* distMap, long int & triangles, long int* degree){
            initNet(size, distMap);
            
            triangles = TrianglesDijiaCalculation(size, degree);
        }
        
        void initNet(long int size, double*distMap){
            for (int i = 0; i < size; i++) {
                Node* temp = new Node(i);
                listofNode.push_back(temp);
            }//init all node
            
            for (int i = 0; i < size; i++) {
                for (int j = 0; j < size; j++) {
                    if (j != i) {
                        if (distMap[i*size+j] < metric_global_DISTMAX) {
                            listofNode[i]->addNeighbor(listofNode[j], distMap[i*size+j]);
                        }
                    }
                }
                /*sort(listofNode[i]->neighbor.begin(), listofNode[i]->neighbor.end(), [](const std::pair<Node*,double> &left, const std::pair<Node*,double> &right) {
                  return left.second < right.second;
                });*/
            }//add the neighbor to the node
        }
        
        void resetNet(){
            std::vector<Node*>::iterator pathIter = this->listofNode.begin();
            for (; pathIter != listofNode.end(); pathIter++) {
                (*pathIter)->resetNode();
            }
        }// resetNet for next calculation
        
        long int TrianglesDijiaCalculation(long int size, long int* degree){
            long int triangles = 0;
            
            for (long int i = 0; i < size; i++) {
                if (i >= size || i < 0) {
                    return -2;
                }
                
                Node* startNode = listofNode[i];
                vector<Node*> reachedNodeFirstRound;
                map<Node*, int> reachedNodeSecondRound;
                
                std::vector<std::pair<Node*, double> >::iterator q = startNode->neighbor.begin();
                for (; q != startNode->neighbor.end(); q++) {
                    if ((q->first->getDegree() >= startNode->getDegree())) {
                        reachedNodeFirstRound.push_back((*q).first);
                    }
                }
                
                for (long unsigned int j = 0; j < reachedNodeFirstRound.size(); j++) {
                    q = reachedNodeFirstRound[j]->neighbor.begin();
                    for (; q != reachedNodeFirstRound[j]->neighbor.end(); q++) {
                        if (q->first->getDegree() >= reachedNodeFirstRound[j]->getDegree()) {
                            if (q->first->getName() != i) reachedNodeSecondRound[q->first]++;
                        }
                    }
                }
                std::map<Node*, int>::iterator p = reachedNodeSecondRound.begin();
                for (;p != reachedNodeSecondRound.end(); p++) {
                    q = p->first->neighbor.begin();
                    for (; q != p->first->neighbor.end(); q++) {
                        if ((*q).first->getName() == i) {
                            triangles++;
                        }
                    }
                }
            }
            if (triangles % 3 != 0) {
                triangles = -1;
            }
            else triangles = triangles/3;
            return triangles;
        }
        
        void DijastraCalculation(long int start, double* shortestMap, long int size, long int* directMap){
            // std::priority_queue<Node*, vector<Node*>, compareNode> dealMap;
            set<Node*, compareNode > nodeQueue;
            
            resetNet();// make sure the net is clean
            
            if (start >= size || start < 0) {
                return;
            }
            
            Node* startNode = listofNode[start];
            startNode->setDistance(0);
            startNode->changeState();
            startNode->changeState();
            // dealMap.push(listofNode[start]);
            // dealMap.push(startNode);
            nodeQueue.insert(startNode);
            
            vector<Node*> reachedNode;
            
            while (!nodeQueue.empty()) {
              //Node* top = dealMap.top();
              //dealMap.pop();
              Node* top = *nodeQueue.begin();
              nodeQueue.erase(nodeQueue.begin());
              
              std::vector<std::pair<Node*, double> >::iterator q = top->neighbor.begin();
              for (; q != top->neighbor.end(); q++) {
                double newDis = (*q).second + top->getDistance();
                int compRes = (*q).first->compDistance(newDis, top);
                if (compRes){
                  if ((*q).first->askState(0)) {
                    (*q).first->changeState();
                    (*q).first->setDistance(newDis);
                    (*q).first->setFather(top);
                    nodeQueue.insert((*q).first);
                  }
                  else if ((*q).first->askState(1) && compRes == 2) {
                    nodeQueue.erase((*q).first);
                    (*q).first->setDistance(newDis);
                    (*q).first->setFather(top);
                    nodeQueue.insert((*q).first);
                  }
                }
              }
              
              if (top->getName() != start) {
                shortestMap[start*size + top->getName()] = top->getDistance();
                directMap[start*size + top->getName()] = top->whoisYourFather()->getName();
              }
              top->changeState();
              reachedNode.push_back(top);
            } 
            
            //return GetPathVector(start, reachedNode);
            //about this part see the paper of betweenness for more details
    }
        
    void DijastraCalculation(long int start, double* shortestMap, double* betweenness, long int size, long int* directMap){
            //std::priority_queue<Node*, vector<Node*>, compareNode> dealMap;
            set<Node*, compareNode > nodeQueue;
            
            resetNet();// make sure the net is clean
            
            if (start >= size || start < 0) {
                return;
            }
            
            double* omega = new double [size];
            double* lamda = new double [size];
            for (int i = 0; i < size; i++) {
                omega[i] = 0;
                lamda[i] = 0;
            }
            omega[start] = 1;
            
            Node* startNode = listofNode[start];
            startNode->setDistance(0);
            startNode->changeState();
            startNode->changeState();
            //dealMap.push(listofNode[start]);
            nodeQueue.insert(startNode);
            //dealMap.push(startNode);
            
            set<Node*, compareNode> reachedNode;
            
            while (!nodeQueue.empty()) {
              //Node* top = dealMap.top();
              //dealMap.pop();
              Node* top = *nodeQueue.begin();
              nodeQueue.erase(nodeQueue.begin());
              
              std::vector<std::pair<Node*, double> >::iterator q = top->neighbor.begin();
              for (; q != top->neighbor.end(); q++) {
                int compRes = (*q).first->compDistance((*q).second + top->getDistance(), top);
                double newDis = (*q).second + top->getDistance();
                if (compRes){
                  if ((*q).first->askState(0)) {
                    (*q).first->changeState();
                    (*q).first->setDistance(newDis);
                    (*q).first->shortestFatherList.clear();
                    (*q).first->shortestFatherList.push_back(top);
                    (*q).first->setFather(top);
                    nodeQueue.insert((*q).first);
                    omega[(*q).first->getName()] = omega[top->getName()];
                  }
                  else if ((*q).first->askState(1) && compRes == 2) {
                    nodeQueue.erase((*q).first);
                    (*q).first->setDistance(newDis);
                    (*q).first->shortestFatherList.clear();
                    (*q).first->shortestFatherList.push_back(top);
                    (*q).first->setFather(top);
                    nodeQueue.insert((*q).first);
                    omega[(*q).first->getName()] = omega[top->getName()];
                  }
                  else if (compRes == 1) {
                    (*q).first->shortestFatherList.push_back(top);
                    omega[(*q).first->getName()] += omega[top->getName()];
                  }
                }
              }
                
                if (top->getName() != start) {
                    shortestMap[start*size + top->getName()] = top->getDistance();
                    directMap[start*size + top->getName()] = top->whoisYourFather()->getName();
                }
                top->changeState();
                reachedNode.insert(top);
            }
            
            for (std::set<Node*>::reverse_iterator i = reachedNode.rbegin(); i != reachedNode.rend(); i++) {
              long int w = (*i)->getName();
              // cout << (*i)->getDistance() << " ";
              for (long unsigned int j = 0; j < (*i)->shortestFatherList.size(); j++) {
                long int v = (*i)->shortestFatherList[j]->getName();
                lamda[v] += (omega[v]/omega[w]*(1 + lamda[w]));
              }
              if (w != start) {
                betweenness[w] = betweenness[w] + lamda[w];// /((size-1)*(size-2))
              }
            }
            // cout << endl;
            
            delete [] omega;
            delete [] lamda;
            //return GetPathVector(start, reachedNode);
            //about this part see the paper of betweenness for more details
        }
    };
    
    vector<long int> calculateSinglePath(long int start, long int end){
        vector<long int> res;
        res.push_back(end);
        long int nextNode = end;
        while (nextNode != start) {
            nextNode = getSingleDirect(start, nextNode);
            res.push_back(nextNode);
        }
        
        vector<long int> resR;
        for (long int i = res.size() - 1; i >= 0; i--) {
            resR.push_back(res[i]);
        }                                   //we get the path from end to start, so it should be reversed.
        return resR;
    }//calculate one single path
    
    long int calculatePath(){
        long unsigned int length = 0;
        for (long int i = 0; i < size; i++) {
            for (long int j = 0; j < size; j++) {
                if (i != j) {
                    EPath[i][j] = calculateSinglePath(i,j);
                    if (EPath[i][j].size() > length) {
                        length = EPath[i][j].size();
                    }//the length of the longest path should be recorded, it will be used in output matrix construction
                }
                else {
                    vector<long int> eq;
                    eq.push_back(-1);
                    EPath[i][j] = eq;
                }//when i == j no path but -1
            }
        }
        return length;
    }//calculate the total path by running calculateSinglePath() many times.
    
public:
    
    vector<vector<vector<long int> > > EPath;   //3D matrix which record the path
    
    metric_global_GraphTools(double* inputMap, long int size, int taskType){
        this->taskType = taskType;
        if (taskType == 1 || taskType == 3) {
            betweenness = new double [size];
        }
        isGraphSym = true;
        this->size = size;
        distMap = new double [size*size];
        shortestMap = new double [size*size];
        directMap = new long int [size*size];
        triangles = 0;
        degree = new long int [size];
        
        for (int i = 0; i < size; i++) {
            degree[i] = 0;
        }
        
        EPath.resize(size);
        for (int i = 0; i < size; i++){
            EPath[i].resize(size);
            for (int j = 0; j < size; j++){
                directMap[i*size + j] = i;
                if (inputMap[i*size + j] != 0){
                    distMap[i*size + j] = inputMap[i*size + j];
                    degree[j]++;
                }
                else distMap[i*size + j] = metric_global_DISTMAX;
                
                if (isGraphSym) {
                    if (inputMap[i*size + j] != inputMap[j*size + i]) {
                        isGraphSym = false;
                    }
                }//see whether it is sym
            }
        }
        //        for (int i = 0; i < size*size; i++){
        //            if (inputMap[i] != 0){
        //                distMap[i] = inputMap[i];
        //            }
        //            else distMap[i] = metric_global_DISTMAX;
        //        }
        
        if (sparseJudgement(degree, size)) {
            sparseGraph = true;
        }
        else sparseGraph = false;
        
        for (int i = 0; i < size*size; i++){
            shortestMap[i] = metric_global_DISTMAX;
        }
        
        shortestPathCalculation();
    }//init and calculate the shortest path, load data from double* and alloc memory for furhter calculation
    
    bool sparseJudgement(long int* degree, long int size){
        long int maxDegree = 0;
        for (int i = 0; i < size; i++) {
            if (maxDegree < degree[i])
                maxDegree = degree[i];
        }
        if (maxDegree*maxDegree*maxDegree <= size) {
            return true;//sparse
        }
        else return false;//not sparse
    }
    
    long int shortestPathDetails(){
        return calculatePath();
    }//start to record the path if it is needed
    
    void triangleArmadillo(){
        triangles = 0;
        arma::mat I(distMap,size,size,true);
        
        arma::umat comparePath = (I != metric_global_DISTMAX);

        for (int i = 0; i < size; i++) {
            arma::umat atNode = comparePath.row(i);
            arma::umat toNode = comparePath.col(i);
            arma::umat needPath = atNode.t()*toNode.t();
            
            arma::umat result = (comparePath % needPath);
            triangles+=(long int)arma::accu(result);
            //cout << triangles << endl;
        }
        triangles/=3;//calculation triangle in matrix form.
    }
    
    void shortestPathCalculation(){
        /*if (taskType == 1) {
            return;
        }*/
        switch (taskType) {
            case 0:
                if (sparseGraph) {
                    Dijiastra(size, distMap, shortestMap, directMap);
                }
                else Floyd(size, distMap, shortestMap, directMap);
                //Floyd(size, distMap, shortestMap, 1);
                break;//only shortest path
                
            case 1://betweenness
                Dijiastra(betweenness, size, distMap, shortestMap, directMap);
                if (isGraphSym) {
                    for (int i = 0; i < size; i++) {
                        betweenness[i] = betweenness[i]/2.0;
                    }
                }//calculate the betweenness and if the matrix is symmetric, betweeenness should be devided by 2
                break;
                
            case 2://triangles
                if (sparseGraph) {
                    Dijiastra(size, distMap, this->triangles, degree);
                    if (isGraphSym) {
                        triangles*=2;
                    }
                }
                else triangleArmadillo();
                break;
                
            case 3://shortestpath details
                Dijiastra(size, distMap, shortestMap, directMap);
                //FloydBasedBetweenness(betweenness, size, distMap, shortestMap, directMap);
                break;
                
            case 9://test case
                Dijiastra(size, distMap, shortestMap, directMap);
                Floyd(size, distMap, shortestMap, directMap);
                betweenness = new double [size];
                Dijiastra(betweenness, size, distMap, shortestMap, directMap);
                Dijiastra(size, distMap, this->triangles, degree);
                if (isGraphSym) {
                    triangles*=2;
                }
                // cout << triangles << endl;
                if (taskType != 1) {
                    Dijiastra(betweenness, size, distMap, shortestMap, directMap);
                }
                FloydBasedBetweenness(betweenness, size, distMap, shortestMap, directMap);
                triangleArmadillo();
                // cout << triangles << endl;
                
            default:
                break;
        }
    }//choose the right algorithm for shortest path(I hope)
    
    ~metric_global_GraphTools(){
        delete [] distMap;
        delete [] directMap;
    }//release the memory
    
    long int getSingleDirect(long int start, long int end){
        return directMap[start*size + end];
    }//get the single direct of the path
    
    double* getShortestMap(){
        if (shortestMap != NULL) {
            return shortestMap;
        }
        else return NULL;
    }
    
    vector<vector<vector<long int> > > getPath(){
        return EPath;
    }
    
    double* getBetweenness(){
        if (betweenness == NULL) {
          //cout << "Alloc Error" << endl;
        }
        return betweenness;
    }//return the betweenness
    
    long int getTriangles(){
        return this->triangles;
    }
    
    double getDiameter(){
        double max = 0;
        for (int i = 0; i < size*size; i++) {
            if (shortestMap[i] < metric_global_DISTMAX && shortestMap[i] > max) {
                max = shortestMap[i];
            }
        }
        return max;
    }
};

//int main(NumericMatrix diaMap) {
//    long int size = diaMap.nrow();
//    
//    if (diaMap.nrow() != diaMap.ncol())
//        throw Rcpp::stop("Equal Size");
//
//    double* inputMatrix = new double [size*size];
//    for (int i = 0; i < size*size; i++) {
//        inputMatrix[i] = diaMap[i];
//    }
//    
//    MGraph tempx(size, inputMatrix);
//    tempx.Floyd();
//    
//    NumerticMatrix disRes(size, size);
//    double* resultDist = tempx.getRA();
//    for (int i = 0; i < size*size; i++) {
//        distRes[i] = resultDist[i];
//    }
//}

// [[Rcpp::export]]
SEXP metric_global_shortestPath(NumericMatrix disMap){
    long int size = disMap.nrow();
    
    if (disMap.nrow() != disMap.ncol()){
        throw Rcpp::exception("Size Not Equal");
        return NumericVector::create(0);
    }
    
    double* inputMatrix = new double [size*size];
    for (int i = 0; i < size*size; i++) {
        if (disMap[i] <= 0) {
            inputMatrix[i] = metric_global_DISTMAX;
        }
        else inputMatrix[i] = disMap[i];
        // cout << inputMatrix[i];
    }
    
    metric_global_GraphTools tempx(inputMatrix, size, 0);
    delete [] inputMatrix;
    
    NumericMatrix disRes(size, size);
    double* resultDist = tempx.getShortestMap();
    for (int i = 0; i < size*size; i++) {
        disRes[i] = resultDist[i];
    }
    NumericVector Diameter(1);
    double diameter = tempx.getDiameter();
    Diameter = NumericVector::create(diameter);
    
    return List::create(Named("Geodesic distances") = disRes, Named("Diameter") = Diameter);
}

// [[Rcpp::export]]
SEXP metric_global_shortestDetails(NumericMatrix disMap){
    long int size = disMap.nrow();
    
    //    if (disMap.nrow() != disMap.ncol())
    //        throw Rcpp::stop("Equal Size");
    //    if (disMap.nrow() != disMap.ncol())
    //        throw Rcpp::warning("Equal Size");
    if (disMap.nrow() != disMap.ncol()){
        throw Rcpp::exception("Size Not Equal");
        return NumericVector::create(0);
    }
    
    double* inputMatrix = new double [size*size];
    for (int i = 0; i < size*size; i++) {
        if (disMap[i] <= 0) {
            inputMatrix[i] = metric_global_DISTMAX;
        }
        else inputMatrix[i] = disMap[i];
    }
    
    metric_global_GraphTools tempx(inputMatrix, size, 3);
    long unsigned int length = tempx.shortestPathDetails();
    
    vector<vector<vector<long int> > > result = tempx.getPath();
    
    NumericVector out=NumericVector(Dimension(length,size,size));
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            for (long unsigned int k = 0; k < length; k++) {
                if (k < result[i][j].size()) {
                    out[i*size*length + j *length + k] = result[i][j][k];
                }
                else out[i*size*length + j *length + k] = -1;
            }
        }
    }
    
    return out;
}

// [[Rcpp::export]]
SEXP metric_node_betweeness(NumericMatrix disMap){
    long int size = disMap.nrow();
    
//    if (disMap.nrow() != disMap.ncol())
//        throw Rcpp::stop("Equal Size");
    if (disMap.nrow() != disMap.ncol()){
        throw Rcpp::exception("Size Not Equal");
        return NumericVector::create(0);
    }
    
    double* inputMatrix = new double [size*size];
    for (int i = 0; i < size*size; i++) {
        if (disMap[i] <= 0) {
            inputMatrix[i] = metric_global_DISTMAX;
        }
        else inputMatrix[i] = disMap[i];
    }
    
    metric_global_GraphTools tempx(inputMatrix, size, 1);
    double* betweenness = tempx.getBetweenness();
    delete [] inputMatrix;
    
    NumericVector between(size);
    for (int i = 0; i <size; i++) {
        between[i] = betweenness[i];
    }
    
    return between;
}

// [[Rcpp::export]]
SEXP metric_global_triangle(NumericMatrix disMap){
    long int size = disMap.nrow();
    
    if (disMap.nrow() != disMap.ncol()){
        throw Rcpp::exception("Size Not Equal");
        return NumericVector::create(0);
    }
    
    double* inputMatrix = new double [size*size];
    for (int i = 0; i < size*size; i++) {
        if (disMap[i] <= 0) {
            inputMatrix[i] = metric_global_DISTMAX;
        }
        else inputMatrix[i] = disMap[i];
    }
    
    metric_global_GraphTools tempx(inputMatrix, size, 2);
    delete [] inputMatrix;
    
    NumericVector Diameter(1);
    double diameter = tempx.getTriangles();
    Diameter = NumericVector::create(diameter);
    
    return Diameter;
}
