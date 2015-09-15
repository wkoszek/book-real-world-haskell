/** snippet oddList */
#include <list>

using namespace std;

list<int> oddList(const list<int>& in)
{
    list<int> out;
    
    for (list<int>::const_iterator i = in.begin(); i != in.end(); ++i) {
	if ((*i % 2) == 1)
	    out.push_back(*i);
    }

    return out;
}
/** /snippet oddList */
