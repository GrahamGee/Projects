#MATH 3806 A3 Q1
import numpy as np
from scipy.integrate import quad

#Solved part of the initial problem on paper, see pdf for A & b matrices
A=np.array([[1,1,1,0,1,0],
            [-8,-7,-6,1,-9,1],
            [22,16,12,-3,29,-9],
            [-26,-16,-12,3,-39,29],
            [21,15,11,-3,18,-39],
            [-18,-9,-6,2,0,18]])
b=np.array([0,0,0,1,1,1])

c=np.linalg.solve(A,b)
print(c)
#The c values are correct, now we will integrate

#B)
#Defining each part of the partial fraction
def f1(x):
    return(c[0]/(x-1))
def f2(x):
    return(c[1]/(x-2))
def f3(x):
    return(c[2]/(x-3))
def f4(x):
    return(c[3]/((x-3)**2))
def f5(x):
    return((c[4]*x + c[5])/(x**2 + 1))

#Now solve the integral from 4 to 5 of each
int1,_=quad(f1,4,5)
int2,_=quad(f2,4,5)
int3,_=quad(f3,4,5)
int4,_=quad(f4,4,5)
int5,_=quad(f5,4,5)

#Add them all together for final answer
finalAnswer=int1+int2+int3+int4+int5
print(finalAnswer)
#This is the same answer I got on an online integral calculator, so it should be correct at 0.077674
