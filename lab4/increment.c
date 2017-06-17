# include <stdio.h>
# include <stdlib.h>
# include <unistd.h>
# include <pthread.h>
# include <getopt.h>

int counter=0;
pthread_mutex_t mutex;

struct threadArgs {
    int data;
};

void incrementCounter()
{
    pthread_mutex_lock(&mutex);

    // Perform stuff on shared variable
    int temp = counter;
    counter = temp + 1;

    pthread_mutex_unlock(&mutex);
}

void * threadFunc(void *myArgs)
{
    struct threadArgs * args = (struct threadArgs*)myArgs;
    int data = args->data;

    for (int i=0; i<data; i++)
    {
        incrementCounter();
    }
    
    return NULL;
}

int main(int argc, char *argv[])
{
    pthread_t thisThread;
    struct threadArgs args;

    int opt;
    int threadCount=5;
    // parse the command line options
	while((opt = getopt(argc, argv, "n:")) != -1)
	{
		switch (opt) {
		case 'n':
            threadCount=atoi(optarg);
            break;
		}
	}

    // Init Mutex
    pthread_mutex_init(&mutex,NULL);

    // Set Arguments and create thread
    args.data = 1000;
    for (int i=0; i<threadCount; i++)
    {
        pthread_create(&thisThread, NULL, threadFunc, &args);
    }

    // Wait for thread to finish executing
    pthread_join(thisThread, NULL);

    // Destry Mutex
    pthread_mutex_destroy(&mutex);
    
    printf("%d\n",counter);
}
