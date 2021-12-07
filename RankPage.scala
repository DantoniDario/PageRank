//Dantoni Dario - 18.12.2019
//Project 1 Page Rank
//Algorithmics and Data Managment
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
object Project1_Dario_Dantoni extends App {

  case class Node(name: String) { //create a class Node which is a string
    var adjacents: Set[Node] = Set() //it contains adjacents which are a set of nodes..
    var pr = 0.0 // ..it contains "pr" which is its pageRank value ..
    var tr = 0.0 // ..it contains a "tr" which is its transmission value..
    var pr1 = 0.0 // .. and it contains an other pagerRank that I created. It will receive the "tr" of other nodes without affecting its "pr". The variable "pr" is the one used to calculate the value of the "tr"
  }
  case class Graph() {
    var nodes = Set[Node]() //create a class "Graph" which will contain all the nodes (unique node, there is not twice the same node)

    def readFromFile(f: String) = { //create the function readFromFile which reads all the directed edges of a graph from a text file
      var ns = Map[String, Node]() //create a map which will contain string in the key side and a node in the value side
      println("All the directed edges:")
      for (l <- Source.fromFile("/Users/dantoni/Desktop/*Algorithmics and Data Management (openbook)/Project/Files/" + f).getLines) { //for each line of the file..
        val i = l.indexOf("->") // ..compute the place where "->" is in the file..
        val srcName = l.substring(1, i - 2) //..retrieve what's inside the "" befor the "->"..
        val destName = l.substring(i + 4, l.size - 2)//..and retrieve what's inside the "" after the "->"
        if (!(ns contains srcName)) { //if the map "ns" doesn't contain the "srcName"..
          val s = Node(srcName) //..create a node which will contain the "srcName"..
          ns = ns + (srcName -> s) //..add the "srcName" in the key side and the corresponding node in the value side of the "ns" map..
          nodes += s //..add this node to "nodes" (noticed that if "nodes" already contains the node, it won't be added)..
        }
        val src = ns(srcName) //..and finally create a node which is the corresponding node to the "srcName" in the map ns
        if (!(ns contains destName)) { //if the map "ns" doesn't contain the "destName"..
          val s = Node(destName) //..create a node which will contain the "destName"..
          ns = ns + (destName -> s) //..add the "destName" in the key side and the corresponding node in the value side of the "ns" map..
          nodes += s //..add this node to "nodes" (noticed that if "nodes" already contains the node, it won't be added)..
        }
        val dest = ns(destName) //..and finally create a node which is the corresponding node to the "destName" in the map ns
        src.adjacents = src.adjacents + dest //add to the "srcName" node its adjacent which is the "destName" node..
        println(srcName + " -> " + destName) //.. and print each edge one by one

      }
      println("")
      println("All unique nodes with their corresponding adjacents:")
      for(i<-nodes) {
        println(i.name + " ->  " + i.adjacents + "   ")
      } //print each unique node with their corresponding adjacents (an other way to print the edge)
    }
    def pageRank(k: Int) = { //create the function pageRank which will compute the pageRank of all nodes after k iterations
      var d = 0.85 //create a value which is the damped effect of "pr". Empiricaslly set to 0.85
      for (i<- nodes)  // for each node in "nodes"..
        i.pr = 1.00/nodes.size //..initialize their pr value to 1/(the number of node in "nodes")
      for (n <- 1 to k) { //repeat the loop k times
        for (i <- nodes) { //for each node..
          i.tr = i.pr / i.adjacents.size //..compute its "tr" value which is its actual "pr"/(the number of adjacent it contains)
          for (j <- i.adjacents) { //for each adjacent of the "i" node..
            j.pr1 += i.tr//..it (the adjacent) receives the "tr" of the "i" node which is added to the other "tr" (if there is others) in the "pr1" of the adjacents
          }
        }
        for (i <- nodes) { //once the iteration is done, take each nodes..
          i.pr = i.pr1 //..update its new "pr" which is the sum of all the "tr" that its "pr1" received..
          i.pr1 = 0 //..initialize its "pr1" to 0..
          i.pr = (1.00 - d) / nodes.size + d * i.pr //..and add the damped effect to its "pr".
        }
      }
      println("")
      println("PageRank of each nodes:")
      for (i <- nodes)
        println(i.name + " -> " + i.pr) //Print the pageRank of all the nodes
    }
    def print5Best(k:Int) = { //create the function print5Best which prints the k nodes with the highest pageRank (they are sorted based on their pageRank)
      var a = ArrayBuffer[Node]() //create an array which will receive all the nodes
      for (i <- nodes) //for each nodes..
        a += i //..add it to the array
      def sortArray (v:ArrayBuffer[Node]):ArrayBuffer[Node]={ //create a function which sorts in a descending order the nodes based on their pageRank
        var i=0 //create a variable which determine the position in the array
        var j=0 //create another variable which determine the position in the array
        var arraySize=v.length //compute a variable which is the size of the array, useful to stop the 2 loops below
        var max=0.0 //create a variable which is the pageRank value of the nodes in the first position of the array
        var maxx= v(i) //create a variable which the nodes in the first position of the array
        var posmax=0 //create a variable which determine the first position in the array
        if (arraySize>1 ){ //if the size of the array is bigger than 1 (because if it is less or equal to 1, I don't need to sort it)
          while (i<(arraySize-1)){ //while "i" position is less than the arraysize-1 (remember that i scala, position start at 0 and we don't need that "i" reach the last position because in the last position there is no more value after to compare)..
            posmax=i //..compute "posmax" as being equal to "i"..
            maxx= v(i) //..compute "maxx" as being the node in the ith position..
            max=v(i).pr //..compute "max" as being equal to the pageRank of the node in the first position..
            j=i+1 //..and compute "j" as being equal to the position just after "i"
            while (j<arraySize){ //while "j" position is less than the arraysize (remember that "i" can reach the second last position in the array so "j" has to have the possibility to reach the last one to be compared to "i"..
              if(v(j).pr>max){ //..if the pageRank value of the nodes inside the jth position is higher than the pageRank value of the nodes inside the ith position
                maxx=v(j) //..compute maxx as being the nodes which has the highest pageRank value
                max =v(j).pr //..compute max as being the highest pageRank value
                posmax=j //..compute posmax as being the position where the node with the highest pageRank value is
              }
              j+=1 } //now compare this "best" nodes with the one in the following position (j+1)
            if(posmax != i){ //at the end of this loop (when j passes the last position), if the position of the best nodes is the not he same as the node in the ith position..
              v(posmax)=v(i) //..compute the nodes in the posmaxth position as being equal to the node in the ith position..
              v(posmax).pr=v(i).pr //..compute the pageRank value of the nodes in the posmaxth position as being equal to the pageRank value of the node in the ith position..
              v(i)=maxx //..compute the nodes in the ith position as being equal to the node in the maxxth position..
              v(i).pr=maxx.pr}//..compute the pageRank value of the nodes in the ith position as being equal to the pageRank value of the node in the maxxth position..
            i+=1 } //to summarize we just inverse the position of the node (with its pageRank value) with the highest pageRank value with the one in the ith position
        } //..and once the the best node has been computed in the ith position, keep going the comparison with the next position of i (i+1).
        v
      }
      var b = sortArray(a) //create the variable "b" which is the sorted version of the array a
      println("")
      println("Sorted nodes based on their pageRank:")
      if (b.length<k) { //if the length of "b" is smaller than k..
        for (i <- 0 to b.length - 1) { //..for i as being equal to the position of the array from 0 to b.length-1 (remember that in scala, position starts at 0)
          println(b(i).name + " -> " + b(i).pr) //..print the name of the node (remember that a node contain a name which is a string) with its pageRank value
        }
      }
        else{ //if the length of "b" is NOT smaller than k..
          for (i <- 0 to k-1) { //..for i as being equal to the position of the array from 0 to k-1 (k is arbitrary, it's depend how many best nodes we want to print)..
            println(b(i).name + " -> " + b(i).pr) //..print the name of the node (remember that a node contain a name which is a string) with its pageRank value
        }
      }
    }
  }
  var g = Graph() //create the graph that will contain all the nodes (set of unique nodes)
  g.readFromFile("graph-mini.txt") //add the nodes contained in the file in the graph "g"
  g.pageRank(20) //add to the node in g, their pageRank after 20 iterations
  g.print5Best(5) //sort the node in g, based on their pageRank and print the 5 nodes with the highest pageRank value
}
