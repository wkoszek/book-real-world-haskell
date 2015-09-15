#include <sys/types.h>

/** snippet book_info */
struct book_info {
    int id;
    char *name;
    char **authors;
};
/** /snippet book_info */

/** snippet roygbiv */
enum roygbiv {
    red,
    orange,
    yellow,
    green,
    blue,
    indigo,
    violet,
};
/** /snippet roygbiv */

struct vector 
{
    float x;
    float y;
};
    
/** snippet shape */
enum shape_type {
    shape_circle,
    shape_poly,
};

struct circle {
    struct vector centre;
    float radius;
};

struct poly {
    size_t num_vertices;
    struct vector *vertices;
};

struct shape 
{
    enum shape_type type;
    union {
	struct circle circle;
	struct poly poly;
    } shape;
};
/** /snippet shape */
