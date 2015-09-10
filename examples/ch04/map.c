/** snippet uppercase */
#include <ctype.h>

char *uppercase(const char *in)
{
    char *out = strdup(in);
    
    if (out != NULL) {
	for (size_t i = 0; out[i] != '\0'; i++) {
	    out[i] = toupper(out[i]);
	}
    }

    return out;
}
/** /snippet uppercase */

/** snippet square */
void square(double *out, const double *in, size_t length)
{
    for (size_t i = 0; i < length; i++) {
	out[i] = in[i] * in[i];
    }
}
/** /snippet square */
