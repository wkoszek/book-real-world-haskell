/** snippet Adler32 */
public class Adler32 
{
    private static final int base = 65521;

    public static int compute(byte[] data, int offset, int length)
    {
	int a = 1, b = 0;

	for (int i = offset; i < offset + length; i++) {
	    a = (a + (data[i] & 0xff)) % base;
	    b = (a + b) % base;
	}

	return (b << 16) | a;
    }
}
/** /snippet Adler32 */
