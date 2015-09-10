#include <pcre.h>

/** snippet pcre_compile **/
pcre *pcre_compile(const char *pattern,
                   int options,
                   const char **errptr,
                   int *erroffset,
                   const unsigned char *tableptr);
/** /snippet pcre_compile **/

/** snippet pcre_exec **/
int pcre_exec(const pcre *code,
              const pcre_extra *extra,
              const char *subject,
              int length,
              int startoffset,
              int options,
              int *ovector,
              int ovecsize);
/** /snippet pcre_exec **/
