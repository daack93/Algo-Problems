import java.util.*;

public class Palin_5
{

    public static void main (String [] args0)
    {
        Scanner scan = new Scanner(System.in);
        int T = scan.nextInt();
        scan.nextLine();

        while(T-- != 0)
        {
            System.out.println(solve(scan.nextLine()));
        }
    }

    public static String solve(String input)
    {
        String out = "";
        char [] asArray = input.toCharArray();
        int len = asArray.length;
        if (len < 2) return input;
        int mid = (len + 1)/2 - 1;
        int c = 1;
        for(int i = 0; i <= mid; ++i)
        {
            char fwd = asArray[i]; char bkd = asArray[len - (i+1)];
            c = (fwd < bkd + c ? 1 : 0);
        }
        for(int i = mid; i >= 0; i--)
        {
            char curr = asArray[i];
            if(c == 1)
            {
                if (curr != '9') c = 0;
                curr = (curr == '9' ? '0' : (char)(curr+1));
            }
            out += curr;
        }
        if (c == 1){
            out += '1';
            while (out.length() < len)
                out = '0' + out;
            out = '1' + out;
            return out;
        }
        if (len%2 == 0)
            out = rev(out) + out;
        else
            out = rev(out) + out.substring(1,out.length());
        return out;
    }

    public static String rev (String input)
    {
        String out = "";
        for(int i = 0; i < input.length(); ++i)
            out = input.charAt(i) + out;
        return out;
    }
}
