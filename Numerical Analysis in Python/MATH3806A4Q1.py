#MATH 3806 Assignment 4 Question 1
import numpy as np
from numpy import *
import matplotlib.pyplot as plt

#A) See pdf

#B)
x0=np.array([0,10,20,30,40,50])
fx=np.array([0.90,1.41,2.52,5.38,17.5,24.3])
coef=np.array([0.90,0.051,0.003,0.000192,0.0000027,-0.000002])

#Degree five polynomial function
def px(x,z,c):
    return(c[0] + 
           c[1]*(x-z[0]) + 
           c[2]*(x-z[0])*(x-z[1]) + 
           c[3]*(x-z[0])*(x-z[1])*(x-z[2]) + 
           c[4]*(x-z[0])*(x-z[1])*(x-z[2])*(x-z[3]) + 
           #This last 5th degree is not helping the fit in this case, as some of the values of the table were becoming negative which was messing up the direction of the polynomial
           c[5]*(x-z[0])*(x-z[1])*(x-z[2])*(x-z[3])*(x-z[4]))
x=np.linspace(0, 50, 1000)
polynEstimate=px(x,x0,coef)

plt.figure(figsize=(6,6))
plt.plot(x0,fx,label=r"$f(x)$",color='r')
plt.plot(x,polynEstimate,label=r"$p(x)$",color='b')
plt.xlabel("Concentration"), plt.ylabel("Viscosity")
plt.title("Plot of $f(x)$ & $P(x)$ Interpolating Newtons Polynomial")
plt.legend(), plt.grid(True), plt.show()

#This fit is only reasonable until x=30, then it diverges

#C)
x1=2.5
x2=5
px1=px(x1,x0,coef)
px2=px(x2,x0,coef)

relVal1=abs(1.02-px1)/1.02
relVal2=abs(1.13-px2)/1.13
print(relVal1)
print(relVal2)
#I don't think this polynomial fits with the model very well, the 5th degree means it has to diverge in the opposite direction at the 5th power
# If it had been a 4th degree polynomial it would have followed the trend past x=30, for instance taking out the c[5] line as stated above
#The large increase from 17.5 to 24.3 caused negative values to appear in the last row, meaning the polynomial changed direction.