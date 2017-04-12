/*
 * this is the program
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef enum faciem {LEFT , RIGHT , CENTER , PASS } faces ;
faces die [] = {LEFT , RIGHT , CENTER , PASS , PASS , PASS };

int numPlayers;

const char * names [] = {
 "Whoopi",
 "Dale",
 "Rosie",
 "Jimmie",
 "Barbara",
 "Kyle",
 "Raven",
 "Tony",
 "Jenny",
 "Clint"
};

/* responses to each play */
const char * play [] = {
 "gives $1 to ",
 "gives $1 to ",
 "puts $1 in the pot ",
 "gets a pass "
};

/* initialize all players bank to $3 */
int bank[10]={3,3,3,3,3,3,3,3,3,3};
/* initialize pot to $0 */
int pot=0;

/* returns pseudo-random faces of die  */
faces roll()
{
    return die[rand() % 6];
}

/* are we still playing? */
bool inPlay()
{
 int holders=0;
 for (int i=0; i<numPlayers; i++)
     if (bank[i]>0)
        holders++;
 return (holders>1);
}

/* the winner has money in the bank */
int winner()
{
 for (int i=0; i<numPlayers; i++)
     if (bank[i]>0)
 	return (i);
 return (-1); // error
}

void dumpBank()
{
printf("-----------------------\n");
for (int i=0; i<numPlayers; i++)
	printf("bank[%d]=%d, ", i, bank[i]);
printf("-----------------------\n");
}


//int main( int argc, const char* argv[] )
int main()
{
    int seed;
    printf("Random seed:");
    scanf("%d", &seed);
    srand(seed);
    printf("How many players?");
    scanf("%d", &numPlayers);
    while (inPlay())
    {
//dumpBank();
        for (int i=0; i<numPlayers; i++)
        {
	   if (!inPlay()) break;
	   int newBank=bank[i];
	   if (newBank > 3) newBank=3;
           for (int j=0; j<newBank; j++)
           { 
               // if (j==0) printf("\nBANK=%d, %s rolls... ", bank[i], names[i]);
               if (j==0) printf("\n%s rolls... ", names[i]);
               int d=roll();
	       //printf(" ROLLING %u ",die[d]);
               printf("%s", play[d]);
	       if (d==0) /* left */
               {
		   int idx = (i+1) % numPlayers;
                   printf("%s ", names[idx]);
                   bank[idx]=bank[idx]+1;
               }
	       if (d==1) /* right */
               {
                   int idx = (i+numPlayers-1) % numPlayers;
                   printf("%s ", names[idx]);
                   bank[idx]=bank[idx]+1;
               }
	       if (d==2) /* pot */
		   pot++;
               if (d != 3)
		   bank[i]=bank[i]-1;
           } 
        }
    }
//dumpBank();

int w=winner();
printf("\n%s wins the $%d pot with $%d in the bank!\n", names[w], pot, bank[w]); 

}
