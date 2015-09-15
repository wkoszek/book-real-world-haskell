/** snippet Tree */
class Tree<A>
{
    A value;
    Tree<A> left;
    Tree<A> right;

    public Tree(A v, Tree<A> l, Tree<A> r)
    {
	value = v;
	left = l;
	right = r;
    }
}
/** /snippet Tree */

/** snippet Example */
class Example 
{
    static Tree<String> simpleTree()
    {
	return new Tree<String>(
            "parent",
	    new Tree<String>("left leaf", null, null),
	    new Tree<String>("right leaf", null, null));
    }
}
/** /snippet Example */
