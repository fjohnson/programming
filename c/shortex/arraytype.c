#include <stdio.h>
#include <stdlib.h>
/* April 30, 2011
   I just found out today that arrays are an actual type that are distinct from 
   pointers! I always assumed it was a bit of syntactic sugar behind the scenes
   and that array syntax was just a short hand for pointer arithmetic. I.e
   a[3] = *(a+3) = 3[a]. Well it turns out that arrays are an actual type!
   Here is an experiment where I test the boundaries of the pointer/array 
   duality. The aim is to find out when and where arrays cannot be adequately
   described in terms of pointers, or any other unique border cases that 
   apply to arrays only. I.e int j[3]; sizeof(j)

   RESULTS:

   The results of this experiement show that there are only really three things
   that are important remember. 

   1.A name of an array gives you the address of its first element
   which is just a pointer to its first element. Thus the name of an
   array is of type "Pointer to type of array contents". 
   I.e int name[5]; Then name is of type (int *). 

   2.The address of an array gives us a pointer to the type of array
   it points to. Well that is obvious enough but lets make it clearer.
   int name[5] has type (int [5]). &name has type (int (*)[5]).
   The key here is that not only are arrays a specific type, but the
   dimensionality further specifies the type of array.

   3.The compiler relies on the type of array to perform specific pointer
   arithmetic. Thus void func(int j[][3]) is different than 
   void func(int *j[3]). The first is an array holding int arrays of size
   3 where the spacing in memory is assumed to be contigious. The second
   is a one dimensional array holding pointers to ints. j + 2 on the first
   form yields the address of j bumped up by sizeof(int) * 3 * 2 bytes.
   j + 2 on the second form yields the address of j bumped up by 
   size of(int *) * 2. So in the first form the compiler understands that
   we want to skip past the first six ints while in the second form it
   understands we want to examine the third pointer. Incidently
   void func(int **j) is equivalent to void func(int *j[3]) further stressing
   the distinction and potential pitfalls between an int ** and int[][].

   The proper way to pass an array then is to tell the compiler what
   type of array we are passing. So void func(int j[][3]) could be rewritten
   as void func(int (*j)[3]). Now this is confusing because it looks like
   what we have in the second form is just "a pointer to int [3] arrays".
   But actually it is "a pointer to int [3] arrays that are assumed to be
   contigious in memory". j + 2 will still yield the same result in both
   incarnations. It would be incorrect to say void func(int (*j)[300]) 
   because then j + 2 would yield an address 300*sizeof(int)*2 bytes away
   from j. It would be incorrect to assume that j+2 does not follow
   directly from j+1 in memory.

   One last thing. int j[2][3][4] can be represented as int (*j)[3][4].
   j+1 in both cases results in sizeof(int)*3+4 bytes being added to
   the base address of j.

*/

/*These three next functions take in a region of memory,
  either a legitmate array type or a pointer to location
  reserved by an array and print out an element. They also
  check the size of the argument (the size is the same for
  all which means the argument is always a pointer even when
  it looks like an array type i.e int sd[300]) 
*/

/*A tiny bit of background information is necessary. When the compiler
  sees a declaration of void singleptr(int sd[]) or void singleptr(int
  sd[300]) it actually treats the parameter sd as a pointer and not an
  array type. Here is the reference for this fact: 

 "More important than addresses of arrays is what happens when you
  declare a function that takes an array as an argument. Because of
  the ‘conversion to the address of its first element’ rule, even if
  you do try to pass an array to a function by giving its name as an
  argument, you actually end up passing a pointer to its first
  element. The usual rule really does apply in this case! But what if
  you declare that the function does have an argument whose type is
  ‘array of something’—like this: 

  void f(int ar[10]); 

  What happens?  The answer may suprise you slightly. The compiler
  looks at that and says to itself ‘Ho ho. That's going to be a
  pointer when the function is called’ and then rewrites the parameter
  type to be a pointer. As a result, all three of these declarations
  are identical: 

  void f(int ar[10]); 
  void f(int *ar); 
  void f(int  ar[]); // since the size of the array is irrelevant!"

  http://publications.gbdirect.co.uk/c_book/chapter5/arrays_and_address_of.html
*/

/*Take an ARRAY type in as a ptr*/
void singleptr(int *sd){
  printf("SDptr %d\t",sd[3]);
  printf("SDptr size: %lud\n",sizeof(sd));
}

/*Take an ARRAY type in as an ARRAY*/
void singlearray(int sd[]){
  printf("SDarray %d\t",sd[3]);
  printf("SDarray size: %lud\n",sizeof(sd));
}

/*Take an ARRAY type in as an ARRAY with an arbitrary incorrect dimension*/
void singlearraydim(int sd[300]){
 printf("SDarrayd %d\t",sd[3]);
 printf("SDarrayd size: %lud\n",sizeof(sd));
}

/*Begin multidimensional array classification*/

void multi_ptr_ptr(int **md){
  printf("MD pointer to pointer %d\t",md[1][2]);
  printf("MD pointer to pointer size: %lud\n",sizeof(md));
}

/*Take in an ARRAY type holding int pointers*/
void multi_array_ptr(int *md[]){
  printf("MD int pointer array %d\t",md[1][2]);
  printf("MD int pointer array size: %lud\n",sizeof(md));
}

/*Take in an ARRAY type holding int pointers. Size of the array is defined.*/
void multi_array_ptr_dim(int *md[300]){
  printf("MD int pointer array of size 300 %d\t",md[1][2]);
  printf("MD int pointer array of size 300: %lud\n",sizeof(md));
}
/*Same thing but with the correct size*/
void multi_array_ptr_dim_correct(int *md[4]){
  printf("MD int pointer array of correct size (4) %d\t",md[1][2]);
  printf("MD int pointer array of correct size (4) size: %lud\n",sizeof(md));
}

/*Take in a ptr to an array. This won't compile. A size must be defined.*/
/*
void multi_ptr_array(int (*md)[]){
  printf("MDptrptr %d\t",md[1][2]);
  printf("MDptrptr size: %lud\n",sizeof(md));
}
*/

/*Take in a ptr to an array (fake size)*/
void multi_ptr_array_dim(int (*md)[300]){
  printf("MD pointer to type (int [300]) %d\t",md[1][2]);
  printf("MD pointer to type (int [300]) size: %lud\n",sizeof(md));
}

/*Same thing, but correct size*/
void multi_ptr_array_dim_correct(int (*md)[4]){
  printf("MD pointer to type (int [4]) (correct) %d\t",md[1][2]);
  printf("MD pointer to type (int [4]) (correct) size: %lud\n",sizeof(md));
}

/*Take in an ARRAY type that holds int ARRAYS of assumed size 300*/
void multiarray(int md[][300]){
  printf("MD array holding int [300] arrays %d\t",md[1][2]);
  printf("MD array holding int [300] arrays size: %lud\n",sizeof(md));
}

/*Take in an ARRAY type that holds int ARRAYS of the correct size for
  the test*/
void multiarray_correct(int md[][4]){
  printf("MD array holding int [4] arrays (correct) %d\t",md[1][2]);
  printf("MD array holding int [4] arrays (correct) size: %lud\n",sizeof(md));
}

/*Results indicate that when passing a 1D array to a function it does not
  matter if we refer to the array or through a pointer. The definition of
  the parameter does not seem to make the result differ either.*/ 

void onedtest(){
  /*Test out one dimensional arrays*/

  int bat[4]; /*Array*/
  int *batptr = bat; /*Pointer representation of array*/
  bat[3] = 28;

  singleptr(bat); 
  singleptr(batptr);
  singlearray(bat);
  singlearray(batptr);
  singlearraydim(bat);
  singlearraydim(batptr);

  /*It is impossible to do a sizeof on batptr with the hopes of getting
    back the size of the array. Because sizeof(batptr) returns the size
    of an int ptr and sizeof(*batptr) returns the size of bat[0]. So
    this is one noticeable difference in the representations.*/
}

void twodtest(){
  /*Test out two dimensional arrays*/
  int fat[2][4];
  
  /*
  int **fat = malloc(sizeof(int *)*2);
  *fat = malloc(sizeof(int)*4);
  *(fat + 1) = malloc(sizeof(int)*4);
  */

  /*This is the correct way to point to the above array*/
  int (*fatptr)[4] = fat; 

  /*This is what I assume is a looser way of pointing*/
  int **fatptr_loose = (int **) fat;

  
  fat[1][2] = 100;
  /*
  printf("%d\n",fat);
  printf("%d\n",*fat);
  printf("%d\n",**fat);
  printf("%d\n",*(fat+1));
  printf("%d\n",(fat+1));
  printf("Size of int %d size of int ptr: %d\n",sizeof(int),sizeof(int *));
  printf("  fatptr_loose:%lu fatptr:%lu fat:%lu\n",fatptr_loose,fatptr,fat); //*(fat[1]+2)
  printf("fatptr_loose+1:%lu fatptr+1:%lu fat+1:%lu\n",fatptr_loose+1,fatptr+1,fat+1); //*(fat[1]+2)
  */
  
  multi_ptr_ptr(fat);
  multi_ptr_ptr(fatptr);
  multi_ptr_ptr(fatptr_loose);

  multi_array_ptr(fat);
  multi_array_ptr(fatptr);
  multi_array_ptr(fatptr_loose);

  multi_array_ptr_dim(fat);
  multi_array_ptr_dim(fatptr);
  multi_array_ptr_dim(fatptr_loose);

  multi_array_ptr_dim_correct(fat);
  multi_array_ptr_dim_correct(fatptr);
  multi_array_ptr_dim_correct(fatptr_loose);

  multi_ptr_array_dim(fat);
  multi_ptr_array_dim(fatptr);
  multi_ptr_array_dim(fatptr_loose);

  multi_ptr_array_dim_correct(fat);
  multi_ptr_array_dim_correct(fatptr);
  multi_ptr_array_dim_correct(fatptr_loose);

  multiarray(fat);
  multiarray(fatptr);
  multiarray(fatptr_loose);

  multiarray_correct(fat);
  multiarray_correct(fatptr);
  multiarray_correct(fatptr_loose);
}

int main(int argc, char **argv){
  struct funny ptr;
  //onedtest();
  twodtest();

  return 0;
}
  
