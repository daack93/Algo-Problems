/* Trying this in Java after Haskell wore me down
 * Just to make sure I can even get the right algo 
 */
import java.io.*;
import java.util.Scanner;

class PrimeGen_2
{
    public static void main(String [] args0)
    {
        Scanner scan = new Scanner(System.in);
        int T = scan.nextInt();
        while(T-- > 0)
            solve(scan.nextInt(),scan.nextInt());
    }

    public static void solve(int a, int b)
    {
        int m = (a%2 == 0 ? a + 1 : a);
        m = (m < 3 ? 3 : m);
        int M = (b%2 == 0 ? b - 1 : b);
        boolean two = a < 3;

        int [] l = new int [(M - m)/2 + (two ? 2 : 1)];
        if(two)
            l[0] = 2;

        for(int i = 0 ; i < l.length - (two ? 1 : 0); ++i)
            l[i + (two ? 1 : 0)] = m + 2*i;

        for(int r = 3; r <= Math.sqrt(b); r+=2)
        {
            int s = m/r;
            s = (s % 2 == 0 ? s - 1 : s);
            s = (s < r ? r : s);
            for(int p = s; p*r <= b; p += 2)
            {
                int n = p*r;
                if(n >= m)
                    l[(n-m)/2 + (two ? 1 : 0)] = 0;
            }
        }

        int count = 0;
        for(int i = 0; i < l.length; ++i)
            if(l[i] > 1)
            {
                System.out.println(l[i]);
            }
    }
}
