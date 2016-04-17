#SPOJ Problem 6: Arithmetics
#Author: David Ackerman
#Apr 17, 2016

def solve():
    expr = raw_input()

    if '-' in expr:
        i = expr.index("-")
        num1 = expr[:i]
        num2 = expr[(i+1):]
        ans = str(int(num1)-int(num2))
        num2 = '-' + num2
        l1,l2,l3 = len(num1),len(num2),len(ans)
        lm = max(l1,max(l2,l3))
        print ' '*(lm-l1)+num1
        print ' '*(lm-l2)+num2
        print '-'*lm
        print ' '*(lm-l3)+ans

    elif '+' in expr:
        i = expr.index("+")
        num1 = expr[:i]
        num2 = expr[(i+1):]
        ans = str(int(num1)+int(num2))
        num2 = "+" + num2
        l1,l2,l3 = len(num1),len(num2),len(ans)
        lm = max(l1,max(l2,l3))
        print ' '*(lm-l1)+num1
        print ' '*(lm-l2)+num2
        print '-'*lm
        print ' '*(lm-l3)+ans
    elif '*' in expr:
        i = expr.index("*")
        num1 = expr[:i]
        num2 = expr[(i+1):]
        placeProds = []
        placeProdStr = []

        i=1
        for c in num2[::-1]:
            prod = int(num1)*int(c)
            placeProdStr.append(str(prod))
            placeProds.append(prod*i)
            i *= 10

        num2 = "*" + num2

        if len(placeProds) == 1:
            ans = str(placeProds[0])
            l1,l2,l3 = len(num1),len(num2),len(ans)
            lm = max(l1,max(l2,l3))
            print ' '*(lm-l1)+num1
            print ' '*(lm-l2)+num2
            print '-'*lm
            print ' '*(lm-l3)+ans
        else:
            finans = str(sum(placeProds))
            l1,l2,lm = len(num1),len(num2),len(finans)
            l3 = max(l1,l2,len(str(placeProds[0])))
            print ' '*(lm - l1)+num1
            print ' '*(lm - l2)+num2
            print ' '*(lm - l3)+'-'*l3
            for place,placeStr in zip(placeProds,placeProdStr):
                print ' '*(lm-len(str(place))) + placeStr
            print '-'*lm
            print finans
    print

num = int(raw_input())
for i in range(num):
    solve()
