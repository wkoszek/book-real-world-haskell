#include <stdio.h>
#include <ctype.h>

/** snippet as_int */
int as_int(char *str)
{
    int acc; /* accumulate the partial result */

    for (acc = 0; isdigit(*str); str++) {
	acc = acc * 10 + (*str - '0');
    }

    return acc;
}
/** /snippet as_int */ 

int main(int argc, char **argv)
{
    int i;
    
    for (i = 1; i < argc; i++) {
	printf("%d\n", as_int(argv[i]));
    }
}
