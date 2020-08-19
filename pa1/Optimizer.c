/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2014                              *
 *  Authors: Ulrich Kremer                   *
 *           Hans Christian Woithe           *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

/* not necessary bc instrUtils already has something to do this rip
//makes the pointer start at the bottom of the instruction list -> start at tail
void startAtBottom(Instruction *ptr) {
    while(ptr->next != NULL) {
        ptr = ptr->next;
    }
}
*/
//should be able to delete the dead code from the list -> doesnt matter where it starts but go with tail
//iterates through the instruction list and deletes the non-critical values marked previously -> basically like the delete from LL in cs211
void deadCodeEliminator(Instruction *ptr) {
    Instruction *temp; //used to save the instruction to be deleted+freed
    while(ptr->prev != NULL) {
        if(ptr->critical == '1') { //if critical, just move to the next instruction/node
            ptr = ptr->prev;
			continue;
        }
        else {//the value of the instruction isnt critical so remove it
            //if the node to be deleted is somewhere in the middle -> not tail
            if(ptr->next != NULL && ptr->prev != NULL) {
                ptr->prev->next = ptr->next; //make ptr's prev node point to the next
                ptr->next->prev = ptr->prev; //make ptr's next node point back to ptr's prev
                temp = ptr->next; //set the temp variable to the next
                free(ptr); //free the instruction
                ptr = temp; //move to the next
            }
            //if the node to be deleted is the last node of the list aka head, just set the prev's next to NULL
            else if (ptr->next == NULL){
                ptr->prev->next = NULL;
                free(ptr);
            }
        }
        ptr = ptr->prev;
    }
    return;
}
//marks the READ and WRITE instructions as critical -> could be recursive or non-recursive
void readWrite(Instruction* ptr) {
    if(ptr->prev == NULL) {
        return;
    }
    if(ptr->opcode == READ || ptr->opcode == WRITE) {
        ptr->critical = '1';
    }
    return readWrite(ptr->prev);
}
//recursively searches through the instruction list to mark the instructions with the related register #
void searchAndMark(Instruction* instr, int reg) {
    //return if the pointer has cleared through all the instructions -> could do if NULL or use a while loop?
	if(instr == NULL) {
		return;
	}
    //use switch statements to cover each case mentioned in the TA powerpoint
    switch(instr->opcode) {
        //For case LOAD, just check if the field has the register and call recursion again for the 2nd field since we deem the instruction to be important
        case LOAD:
            if(instr->field1 == reg) {
                instr->critical = '1';
                searchAndMark(instr->prev, instr->field2);
            }
            break;
        //for case LOADI, the 2nd field is just a constant so we can ignore calling the function again and just mark
        case LOADI:
            if(instr->field1 == reg) {
                instr->critical = '1';
            }
            break;
        //for case STORE, just check if the field has the register and call recursion again for the 2nd field since we deem the instruction to be important
        case STORE:
            if(instr->field1 == reg) {
                instr->critical = '1';
                searchAndMark(instr->prev, instr->field2);
            }
            break;
        //for the calculations, they have a field2 and field3 that are important if the instruction is critical
        case ADD:
        case SUB:
        case MUL:
        case AND:
        case OR:
            if(instr->field1 == reg) {
                instr->critical = '1';
                searchAndMark(instr->prev, instr->field2);
                searchAndMark(instr->prev, instr->field3);
                instr = instr->prev;
            }
            break;
        //since READ and WRITE are already marked, just break out of the switch. or just use default?
        case READ:
        case WRITE:
            break;
        
    }
	//move to the next/previous instruction if exiting the switch
	searchAndMark(instr->prev, reg);
}
//the main
int main()
{
    Instruction *head;
    //if the head instruction isnt there, then the list is null => no instructions
    head = ReadInstructionList(stdin);
    if (!head) {
        WARNING("No instructions\n");
        exit(EXIT_FAILURE);
    }
//CRITICAL is a char so use ''!
  //  printf("Successfully started\n");
    int temp; //the register/variable used in a WRITE statement that needs to be found in store and etc
    Instruction *instr = head; //use to scan through the instructions and mark as critical/non-critical
    instr = LastInstruction(instr); //make it start at the end of the instruction list AKA tail instead of head
  //  printf("Successfully called last instruction\n");
    readWrite(instr); //function that marks all READ and WRITE instructions as critical
   // printf("Successfully ran readWrite\n");
    instr = LastInstruction(instr); //need to set it back to the tail since it went to the head in readWrite
    Instruction *writeptr = head;
    writeptr = LastInstruction(writeptr);
   // printf("Successfully ran stuff for writeptr\n");
    //now loop from the last to the first instruction, searching for WRITE and its associated STORE -> should be able to find multiple writes like in one of the testcases
    while(instr->prev != NULL) {
      //  printf("Successfully entered the first while\n");
        if(instr->opcode == WRITE) {
         //   printf("Successfully found a write statement\n");
            temp = instr->field1; //store the field that needs to be found in a STORE instruction
            //loop through using a new pointer that will search for STORE instructions -> if field1 = the field1 of the write, mark critical
            while(writeptr->prev != NULL) {
                if(writeptr->field1 == temp && writeptr->opcode == STORE) {
                    writeptr->critical = '1';
         //           printf("Found the right store\n");
                    searchAndMark(writeptr->prev, writeptr->field2); //call to find all related instructions for the field2
					break; //exit the loop so we dont get the other STORE's for the same variable -> only latest one necessary
                }
                writeptr = writeptr->prev; //if not found, move to the next previous instruction
            }
        }
        //printf("temp is equal to %d\n", temp);
        instr = instr->prev; //if not found, move to the next previous instruction
    }
	/*
	while(instr->next != NULL) {
		printf("The current value of critical is %d\n", instr->critical);
		instr = instr->next;
	}
	*/
    //eliminates the instructions that were not marked as critical (1) and free's from the doubly LL
    //make the pointer start at the tail again for the eliminator
   instr = LastInstruction(instr);
   deadCodeEliminator(instr);

    if (head) {
        PrintInstructionList(stdout, head);
        DestroyInstructionList(head);
    }
    return EXIT_SUCCESS;
}



