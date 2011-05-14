#define _GNU_SOURCE
#define write(x) printf("In %d\n",x);
#define writeout(x) printf("Out %d\n",x);

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <time.h>

/**
    Thu 5 May, 2011.
    Provided within is both an implementation of the heapsort algoritm
    and the heapsort data structure. heapsort is conducted using in place
    sorting and the "heapify" heap construction that yields O(n) time complexity
    when creating the heap. It involves constructing the heap from a given set
    of numbers from the bottom up. It does this recursively.
**/

struct heap {
    size_t size; //Maximum total size
    size_t current_size; //Current size of the heap
    size_t type_size; //The size of a single element in bytes
    comparison_fn_t compfunc; //The user defined comparison function
    void *elements; //Pointer to the elements
};

/*
  Create a new heap.

  Inputs:
  initial_size -> The initial capacity of the heap
  typesize -> The size in bytes of a single element
  comp -> The comparision function that defines the ordering of the elements

  Return: Null if malloc fails to allocate space.
*/
struct heap * make_heap(size_t initial_size, size_t typesize, comparison_fn_t comp){
    struct heap *newheap = malloc(sizeof(struct heap));

    if (!newheap){
        perror("Failed malloc in make_heap()");
        return NULL;
    }
    newheap->current_size = 0;
    newheap->size = initial_size + 1;
    newheap->type_size = typesize;
    newheap->compfunc = comp;
    newheap->elements = malloc( (initial_size + 1) * typesize);

    if(!newheap){
        perror("Failed malloc in make_heap()");
    }

    return newheap;
}

/*
  Swap two elements in an array

  Inputs:
  array -> The array to swap elements in
  index, index2 -> The index to swap
  element_size -> The size of a single element in the array
*/
void swap(void *array, size_t index, size_t index2, size_t element_size){
    char buf[element_size];

    memcpy(buf, array + index * element_size, element_size);

    memmove(array + index * element_size,
            array + index2 * element_size,
            element_size);

    memmove(array + index2 * element_size, buf, element_size);
}

/*
  Insert an element into the heap

  Inputs:
  heap -> The heap to insert
  e -> The element to insert

  Return: Null if a call to realloc fails to allocate space
*/
struct heap * insert_heap(struct heap *heap, const void *e){
    if (heap->size == heap->current_size){

        heap->elements = realloc(heap->elements,
                                 heap->type_size * heap->size * 2);

        if(!heap->elements){
            perror("Failed realloc in insert_heap()");
            return NULL;
        }

        heap->size *= 2;
    }

    size_t index = heap->type_size * heap->current_size;
    memcpy(heap->elements + index, e, heap->type_size);

    //Up bubble.
    size_t position = heap->current_size;
    size_t parent = (position - 1) / 2;

    while(position > 0){

        //((position - 1) / 2) should result in integer division.
        if (heap->compfunc( heap->elements + parent * heap->type_size,
                            heap->elements + position * heap->type_size) > 0){

              swap(heap->elements, parent, position, heap->type_size);
              position = parent;
              parent = (position - 1) / 2;
            }
            else{
                //Parent is equal to or less than bubbled up element so there
                //is no need to continue.
                break;
            }
    }
    heap->current_size++;

    return heap;
}

/*
  Down bubble on the inserted heap starting at `position'

  Inputs:
  elements -> The array holding the elements of the heap
  position -> The index within the array where the root is stored
  size -> The size of the heap
  type_size -> The size in bytes of a single element within the heap
  compfunc -> The function that determines the ordering of elements within the heap
*/
static void downbubble(void * elements, size_t position,
    size_t size, size_t type_size, comparison_fn_t compfunc){

    //Return if down bubbling is unnecessary. I.e one or zero items in the heap.
    if(size < 2) return;

    //Down bubble (we have at least 2 items)
    size_t child_left, child_right, swap_target;
    void *pos_ptr, *child_left_ptr, *child_right_ptr;

    child_left = position * 2 + 1;
    child_right = child_left + 1;

    //Remember, a node can either have a left and a right child,
    //or a left child only, or no children. That's all thats possible.
    while(1){

         pos_ptr = elements + position * type_size;
         child_left_ptr = elements + child_left * type_size;
         //If a right child exists then a left child is guaranteed to exist.

         if(child_right < size){
            //Compare left and right, and take the min.

            child_right_ptr = elements + child_right * type_size;

            if(compfunc(child_left_ptr, child_right_ptr) > 0){
                swap_target = child_right;
                child_left_ptr = child_right_ptr;
            }
            else{
                swap_target = child_left;
            }

            //Now compare the root with the min of the children to see if a
            //swap is necessary.
            if(compfunc(pos_ptr, child_left_ptr) > 0){
                swap(elements, position, swap_target, type_size);
                position = swap_target;
            }else{ break; }
         }
         else if(child_left < size){
            if(compfunc(pos_ptr, child_left_ptr) > 0){
                swap(elements, position, child_left, type_size);
                position = child_left;
            }
            else{ break; }
         }
         else{
             //No swapping took place, so no more down bubbling is necessary
             break;
         }
         child_left = position * 2 + 1;
         child_right = position * 2 + 2;
    }
}

/*
  Remove a single element from the heap. This item has had space allocated
  for it with malloc so you must free it eventually.

  Inputs:
  heap -> The heap to remove the root element.

  Return: null if a malloc error occured.
  The same heap if the size of the heap is 0.
*/
void * remove_heap(struct heap *heap){
    if(!heap->current_size) return heap;

    void * last_element_addr;
    void * retval = malloc(heap->type_size);

    if(!retval){
        perror("remove_heap() malloc error");
        return retval;
    }

    last_element_addr = heap->elements + (heap->current_size - 1) * heap->type_size;

    memcpy(retval,heap->elements,heap->type_size);
    memmove(heap->elements, last_element_addr, heap->type_size);

    heap->current_size--;

    downbubble2(heap->elements, 0, heap->current_size, heap->type_size, heap->compfunc);

    return retval;
}

/*
  Free the memory associated with a heap (The struct and the elements contained
  within)
*/
void destroy_heap(struct heap *heap){
    free(heap->elements);
    free(heap);
}

/*
  Create a heap in linear time.

  Inputs:
  elements -> An array holding the elements for which a new heap is to be
  constructed from.
  n -> The number of elements and the resulting size of the new heap.
  esize -> The size of a single element in the array
  compfunc -> The comparision function determining the ordering of elements
  within the heap.

  Return: null if a malloc error occured, a new heap otherwise.
*/
struct heap *heapify(void * elements, const size_t n, const size_t esize,
                    comparison_fn_t compfunc){

    struct heap * newheap = malloc(sizeof(struct heap));

    if(!newheap){
        perror("Malloc failure in heapify()");
        return NULL;
    }

    newheap->elements = malloc(n * esize);

    if(!newheap->elements){
        perror("Malloc failure in heapify()");
        return NULL;
    }

    memcpy(newheap->elements, elements, esize*n);

    void buildheap_direct(){

        int root_idx = n-1;
        while(root_idx > -1)
            downbubble2(newheap->elements, root_idx--, n, esize, compfunc);
    }

    void buildheap(size_t root_idx){

        if ( root_idx >= n ){
            return;
        }

        buildheap(root_idx * 2 + 1);
        buildheap(root_idx * 2 + 2);

        downbubble(newheap->elements, root_idx, n, esize, compfunc);
    }

    //buildheap(0);
    buildheap_direct();
    newheap->current_size = n;
    newheap->size = n;
    newheap->compfunc = compfunc;
    newheap->type_size = esize;

    return newheap;
}

/*
  In place heapsort. Space the size of the input is allocated with malloc
  to hold the sorted result. The comparision function must be defined to return
  the opposite of what you actually desire when comparing two elements.

  Input:
  elements -> The array to sort
  n -> The size of the array
  esize -> The size in bytes of a single element within the array
  compfunc -> A function that determines the ordering of the elements in the
  _opposite_ ordering of what you desire

  Return: null on a malloc error, a new heap otherwise.
*/
void * heapsort(void *elements, const size_t n, const size_t esize,
comparison_fn_t compfunc){
    struct heap * constructed_heap = heapify(elements,n,esize,compfunc);
    void * underlying_data = constructed_heap->elements;
    char buf[esize];
    size_t last_idx;

    //n-1 because once we have a single element left to remove we are done
    for(size_t i = 0; i < n-1; i++){
      last_idx = n - i - 1;
      swap(underlying_data, 0, last_idx, esize);
      downbubble(underlying_data, 0, last_idx,esize,compfunc);
    }
    free(constructed_heap);
    return underlying_data;
}

/*Test inserting and removing elements from a heap*/
void test_heap_insert_remove(){
    printf("\nConstructing new test\n");
    size_t test_size = 10;
    struct heap *yourheap = make_heap(5,sizeof(int),testcompare);
    int * r;
    int t[test_size];

    for(int i = 0; i < test_size; i++){
        t[i] = rand();
    }

    for(int i = 0; i < sizeof(t) / sizeof(int); i++){
        insert_heap(yourheap,t+i);
    }

    for(int i = 0; i < sizeof(t) / sizeof(int) ; i++){
        write( *( (int *) (yourheap->elements) + i ));
    }

    for(int i = 0; i < sizeof(t) / sizeof(int) ; i++){
        r = (int *) remove_heap(yourheap);
        writeout( *((int *)r)) ;
    }
}

/*A comparison function used in the tests*/
int testcompare(const void *e, const void *e1){
    int *ec = (int *)e;
    int *e1c = (int *)e1;

    if(*ec > *e1c) return 1;
    else if(*ec < *e1c) return -1;
    return 0;
}

/*A comparison function used in the heapsort tests*/
int heapsort_testcompare(const void *e, const void *e1){
    int *ec = (int *)e;
    int *e1c = (int *)e1;

    if(*ec < *e1c) return 1;
    else if(*ec > *e1c) return -1;
    return 0;
}

/*Test creating a heap with heapify*/
void test_heapify(){
    printf("\nConstructing \"heapify\" operation\n");

    size_t test_size = 15;
    struct heap *yourheap;
    int * r;

    int t[test_size];
    for(int i = 0; i < test_size; i++){
        t[i] = rand() % 25;
    }
    //int t[] = {12,9,24,1,7,4,8};
    //int t[] = {22,21,4};

    for(int i = 0; i < sizeof(t) / sizeof(int) ; i++){
       write(t[i]);
    }

    yourheap = heapify(t,sizeof(t)/sizeof(int),sizeof(int),heapsort_testcompare);

    for(int i = 0; i < sizeof(t) / sizeof(int) ; i++){
        r = (int *) remove_heap(yourheap);
        writeout( *((int *)r)) ;
    }
}

/*Test heapsort*/
void test_heapsort(){
    printf("\nHeapsort test\n");

    size_t test_size = 10;

    int t[test_size];
    for(int i = 0; i < test_size; i++){
        t[i] = rand() % 25;
    }
    //int t[] = {22,21,4};

    for(int i = 0; i < sizeof(t) / sizeof(int) ; i++){
       write(t[i]);
    }

    int *result = heapsort(t, test_size, sizeof(int), heapsort_testcompare);

    for(int i = 0; i < sizeof(t) / sizeof(int) ; i++){
       writeout(result[i]);
    }
}

int main(int argc, char **argv){

    srand(time(0));
    test_heapsort();

    return 0;
}
