/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>
/**
* Performs segment scan to find strongest neighbor for each src node
* @param src The source array in the edge list
* @param oldDst The current dst array in the edge list -> DONT MODIFY
* @param newDst The modified dst array produced by this GPU kernel function
* @param oldWeight The current weight array in the edge list -> DONT MODIFY
* @param newWeight The modified weight array produced by this GPU kernel function
* @param madeChanges If our output is different than our input then we must set *madeChanges to 1, so the host will know to launch another step of the scan.
* @param distance The distance between array locations being examined. This is always a power of 2.
* @param numEdges The size of the index, weight, and flags arrays.
*/
__global__ void strongestNeighborScan_gpu(int * src, int * oldDst, int * newDst, int * oldWeight, int * newWeight, int * madeChanges, int distance, int numEdges) {
	int totalThreads = blockDim.x * gridDim.x; //the total amount of threads
	int tid = blockIdx.x * blockDim.x + threadIdx.x; //the thread ID 
	int i; //intialize the counter for the for loop
	for (i = tid; i < numEdges; i+= totalThreads) { //the for loop from the slides for thread assigning
		if ((i - distance) < 0) { //check for out of bounds case
			newDst[i] = oldDst[i];
			newWeight[i] = oldWeight[i];
		}
		else if (src[i] == src[i-distance]) { //if theyre the same segment, compute for the larger weight
			//plug in the larger value for the weights
			if (oldWeight[i] > oldWeight[i-distance]) { //if the current weighs more
				newDst[i] = oldDst[i];
				newWeight[i] = oldWeight[i];
				if(oldDst[i] != newDst[i]) {
					*madeChanges = 1;
				}
			}
			else { //if not
				newDst[i] = oldDst[i-distance];
				newWeight[i] = oldWeight[i-distance];
				if(oldDst[i] != newDst[i]) {
					*madeChanges = 1;
				}
			}
		}
		else { //if src of the current is not equal to the src of i-distance, just plug in the current one as the newdst + weight, dont need to do conditions
			newDst[i] = oldDst[i];
			newWeight[i] = oldWeight[i];
			if(oldDst[i] != newDst[i]) {
				*madeChanges = 1;
			}
		}
	}
}
