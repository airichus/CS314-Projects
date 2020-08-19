/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>
/**
 * Collates results of segment scan (or segment prefix sum), putting last value from each segment into an array
 * Note that behavior for empty segments is undefined. E.g. if there's no segment with source node 2, then output[2] might contain garbage data.
 * @param src The segment ID for each edge in the scan result
 * @param scanResult The scan result or prefix sum result that we're collating
 * @param output The output
 * @param numEdges The size of the src and scanResult arrays.
*/
__global__ void collateSegments_gpu(int * src, int * scanResult, int * output, int numEdges) {
	int totalThreads = blockDim.x * gridDim.x; //the total amount of threads
	int tid = blockIdx.x * blockDim.x + threadIdx.x; //the thread ID 
	int i;
	for (i = tid; i < numEdges; i+= totalThreads) {
		//compares src[i] with src[i+1], if they are not equal, then the i-th data element is the last one in its own segment
		//if the very last element of the input, just store it
		 if(src[i] == src[numEdges-1]) {
			output[src[i]] = scanResult[i];
		}
		//check if the last element of the segment represented by the src value -> if it is, assign the output as the scanresult
		 if (src[i] != src[i+1]) {
			output[src[i]] = scanResult[i];
		}
	}
}
