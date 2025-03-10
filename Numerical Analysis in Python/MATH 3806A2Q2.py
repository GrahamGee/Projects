import numpy
import math
import matplotlib
#MATH 3806 A2 Q2
#Note x has a domain of [-1.5,1.5]
#The function
def funcQ1(x):
    y=(math.sin(10*x)+3*x)*math.exp(-x**2)+0.2
    return(y)

#The derivative of the function
def funcQ1D(x):
    y=(-2*math.exp(-x**2)*x*math.sin(10*x))-(6*math.exp(-x**2)*x**2)+(10*math.exp(-x**2)*math.cos(10*x))+(3*math.exp(-x**2))
    return(y)

#Will use combined Newton-Bisection method to perform the root finding method as quickly as possible, while still being precise
#Start with bisection until we get close to root, then switch to newton method
#Note that the root is (-0.015435,0) from desmos

def newtBisect(f,fd,a,b,delta=1e-2,eps=1e-8):
    x0=-1
    #While loop for bisection method within delta tolerance
    while(True):
        c=(a+b)/2
        print("Delta iteration")
        if(f(a)*f(c)<0):
            b=c
        else:
            a=c
        
        #Breaks after meeting the first stopping rule set by delta
        if((b-a)/2 < delta):
            #Set starting value of newtons method x0 to midpoint of last interval of bisection
            x0=(a+b)/2
            break
        else:
            continue

    while(True):
        print("Epsilon iteration")
        fVal=f(x0)
        derivfVal=fd(x0)
        x1=x0-(fVal/derivfVal)
        #print(x1)
        if(abs(x1-x0)<eps):
            return((x1+x0)/2)
        else:
            x0=x1
            continue

#Calculating root with function, initial interval a=-0.1,b=0.1
value=newtBisect(funcQ1,funcQ1D,-1.5,1.5)
print("Value is: ",value)
