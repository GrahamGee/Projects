import numpy as np
from numpy.linalg import norm
from numpy import *
#MATH 3806 A3 Q2

#A)
#Derived formula is in pdf

#Computing M and B matrices
A=np.array([[17.444,2.021,0.453,0.031],[2.021,15.629,4.071,-1.181],[0.453,4.071,21.275,-0.447],[0.031,-1.181,-0.447,13.058]])
b=np.array([3.114,7.722,5.512,6.005])
X0=np.array([1,1,1,1])

#Jacobi method
M=np.array([[17.444,0,0,0],[0,15.629,0,0],[0,0,21.275,0],[0,0,0,13.058]])
Minv=np.linalg.inv(M)
B=np.identity(4)-np.dot(Minv,A)
#print(M)
#print(Minv)
#print(B)

#Gauss-Seidel
M2=np.array([[17.444,0,0,0],[2.021,15.629,0,0],[0.453,4.071,21.275,0],[0.031,-1.181,-0.447,13.058]])
Minv2=np.linalg.inv(M2)
B2=np.identity(4)-np.dot(Minv2,A)
#print(B2)

#B) Since the Matrix A is diagonally dominant as 
#|17.444| > |2.021| + |0.453| + |0.031|
#|15.629| > |2.021| + |4.071| + |-1.181|
#|21.275| > |0.453| + |4.071| + |-0.447|
#|13.058| > |0.031| + |-1.181| + |-0.447|
#Both the Jacobi and Gauss-Seidel Methods will converge

###Ignore
#I tried programming my own jacobi method calculator but it's not quite working, so I used the provided one below

#Method for fixed point equation Jacobi
def JacobiGaussMethod(B,Minverse,X0,b,eps=1e-8):
    #Set base point
    Xnprev=X0
    Xn=np.array([0,0,0,0])
    #Calculate fixed point method
    Minvb=np.dot(Minverse,b)
    
    #Iterate until stopping rule
    while(True):
        Xn=np.dot(B,Xnprev)+Minvb
        print("norm:")
        print(norm(Xn-Xnprev))
        print("eps:")
        print(eps)
        if(norm(Xn-Xnprev)<eps): 
            break
        else:
            Xnprev=Xn
    return(Xn)

#value=JacobiGaussMethod(B,Minv,X0,b)
#print("The final value for X is:")
#print(value)

####Ignore

#Test my method against provided one
def linsysiter(B,M,b,x0,eps):
    # Input
    #   B, M: Matrices needed for fixed point iteration
    #   b: right hand side of system of equations (vector)
    #   x0: initial guess (vector)
    #   eps: tolerance for the stopping rule
    #    
    # Output
    #   xstar: the solution of the linear system.
    #   ncv: number of iterations at convergence.
    #   err: error at convergence (L_2 norm)
    niter=200
    # This is not efficient!
    Minv=linalg.inv(M)
    Mib=dot(Minv,b)
    for i in range(niter):
        xstar=dot(B,x0)+Mib    
        err=linalg.norm(xstar-x0)
        if (err <= eps):
            ncv=i
            break    
        # update the initial guess and iterate
        x0=xstar
        if (i==niter):
            print("Iterative method failed to converge!")
            break
    return xstar, ncv, err

#C)
#Jacobi
value2=linsysiter(B,Minv,b,X0,1e-8)[0]
#D)
#Gauss-Seidel
value3=linsysiter(B2,Minv2,b,X0,1e-8)[0]

print("Jacobi Method:",value2)
print("Gauss-Seidel Method:",value3)

#E)
#In this case, the Gauss-Seidel method converges faster and more efficiently, so it is preffered.