#MATH 3806 A3 Q3
import numpy as np
from numpy import *
import matplotlib.pyplot as plt
#A)
#See pdf for computed values of Lagrange Polynomials
def L20(x):
    return((x-2)*(x-3)/2)
def L21(x):
    return(-(x-1)*(x-3))
def L22(x):
    return((x-1)*(x-2)/2)

#Set interval
x = np.linspace(1, 3, 100)

#Get Y values
Y20=L20(x)
Y21=L21(x)
Y22=L22(x)

#Plots
plt.figure(figsize=(5, 5))
plt.plot(x, Y20, label=r"$L_{2,0}(x)$", color='r'), plt.plot(x, Y21, label=r"$L_{2,1}(x)$", color='g'), plt.plot(x, Y22, label=r"$L_{2,2}(x)$", color='c')
plt.xlabel("X"), plt.ylabel("Y")
plt.title("Plot of $L_{2,0}(x)$, $L_{2,1}(x)$, $L_{2,2}(x)$")
plt.legend(), plt.grid(True), plt.show()

#B)
#See pdf for answer

#C)
def f(x):
    return(np.log(x))

yf=f(x)

def P(x):
    return(-(np.log(2)*(x-1)*(x-3) + np.log(3)*((x-1)*(x-2)/2)))

yP=P(x)

plt.figure(figsize=(4,4))
plt.plot(x,yf,label=r"$f(x)",color='r')
plt.plot(x,yP,label=r"$P(x)",color='g')
plt.xlabel("X"), plt.ylabel("Y")
plt.title("Plot of $f(x)$ & $P(x)$")
plt.legend(), plt.grid(True), plt.show()
#The approximation P(x) of f(x) is not very close, as the function appears to be diverging at a higher degree power than that of f(x).
#It slopes downward much faster than that of f(x), I would imagine due to the missing first point, and possible lack of more points to give a better approximation.

#D)

#Points
yf1=f(1.5)
yP1=P(1.5)
yf2=f(2.4)
yP2=P(2.4)

#Absolute errors
AE1=np.abs(yP1-yf1)
AE2=np.abs(yP2-yf2)
print(AE1)
print(AE2)
#We can observe what was seen in part C), as our approximation for point 2 which is larger, has a larger absolute error, as P(x) diverges further from f(x).

#E)
#See pdf