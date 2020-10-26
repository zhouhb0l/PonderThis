#include <stdio.h>
#include <stdlib.h>

int main(){
int A,B,C,D,n;
  A=33;
  B=-72;
  D=A-B;
  
  srand(22113541);
one:
  printf("at one, %d,%d,%d\n",A,B,D);
  n=rand()%5;
  if(n==0){ goto five;};
  if(n==1){ goto six;};
  if(n==2){ goto nine;};
  if(n==3){ goto twelve;};
  if(n==4){ goto sixteen;};

five:
  printf("at five, %d,%d,%d\n",A,B,D);
  if(A==0){goto twelve;};

six:
  printf("at six, %d,%d,%d\n",A,B,D);
  B=B%D;
  //B=abs(B);
  A=D;
  D=A-B;

nine:
  printf("at nine, %d,%d,%d\n",A,B,D);
  if(B==0) {goto nineteen;};

ten:
  printf("at ten, %d,%d,%d\n",A,B,D);
  A=A+B;
  n=rand()%4;
  if(n==0){goto six;};
  if(n==1){goto thirteen;};
  if(n==2){goto fourteen;};
  if(n==3){goto twelve;};

twelve:
  printf("at twelve, %d,%d,%d\n",A,B,D);
   if(A==0){goto nineteen;};


thirteen:
  printf("at 130, %d,%d,%d\n",A,B,D);
  n=rand()%4;
  if(n==0){goto six;};
  if(n==1){goto ten;};
  if(n==2){goto fourteen;};
  if(n==3){goto twelve;};

fourteen:
  printf("at 140, %d,%d,%d\n",A,B,D);
   A=A-B;
goto thirteen;

sixteen:
  printf("at 160, %d,%d,%d\n",A,B,D);
  if(D==0){goto one;};

A=A+B;
goto thirteen;


nineteen:
  printf("at 190, %d,%d,%d\n",A,B,D);
printf("%d",D);

      
return 0;


}
