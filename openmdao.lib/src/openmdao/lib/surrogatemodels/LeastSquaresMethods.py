# Least Squares Methods

from numpy import matrix, linalg, eye, diag,  zeros, hstack, vstack, sign, random, finfo, nonzero, ix_, array


def housels(A, b):

    A = A.copy()
    A0 = A.copy()

    m = A.shape[0]
    n = A.shape[1]
    if n > m:
        print "Error:  A is " + str(A.shape[0]) + "x" + str(A.shape[1]) + ", must have m>=n \n\n"

    Q = matrix(eye(m))

    for k in range(0,n):
        v, beta = house(A[k:,k])
        A[k:,k:] = A[k:,k:]-beta*v*(v.T*A[k:,k:])
        Q[:,k:] = Q[:,k:]-beta*(Q[:,k:]*v)*v.T
    c = Q.T*b
    x = linalg.solve(A[:n,:n],c[:n,0]) 
    rs = linalg.norm(A0*x-b)
    
    return x, rs, Q 

    
def givensls(A, b):
    
    A = A.copy()
    A0 = A.copy()
    b = b.copy()
    b0 = b.copy()

    #Determine size and check shape requirements of A matrix
    m = A.shape[0]
    n = A.shape[1]
    if n > m:
        print "Error:  A is " + str(A.shape[0]) + "x" + str(A.shape[1]) + ", must have m>=n \n\n"

    for j in range(0,n):
        for i in range(m-1,j,-1):
            if A[i,j] == 0:
                co = 1
                si = 0
            else:
                if abs(A[i,j]) > abs(A[i-1,j]):
                    t = -A[i-1,j]/A[i,j]
                    si = 1/(1+t**2)**0.5
                    co = si*t
                else:
                    t = -A[i,j]/A[i-1,j]
                    co = 1/(1+t**2)**0.5
                    si = co*t
            
            A[i-1:i+1,j:n] = matrix([[co,-si],[si,co]])*A[i-1:i+1,j:n]
            b[i-1:i+1,0] = matrix([[co,-si],[si,co]])*b[i-1:i+1,0]
    
    c = b[0:n,0]
    x = linalg.solve(A[:n,:n],c) 
    rs = linalg.norm(A0*x-b0)
    Q = 0
    
    return x, rs, Q 


def mgsls(A, b):
    
    A = A.copy()
    A0 = A.copy()
    #Determine size and check shape requirements of A matrix
    m = A.shape[0]
    n = A.shape[1]
    if n > m:
        print "Error:  A is " + str(A.shape[0]) + "x" + str(A.shape[1]) + ", must have m>=n \n\n"

    #Initial setup
    R = matrix(zeros((m,n)))
    Q = matrix(zeros((m,n)))

    #MGS algorithm
    for k in range(0,n):
        R[k,k] = linalg.norm(A[:,k])
        Q[:,k] = A[:,k]/R[k,k]
        for j in range(k+1,n):
            R[k,j] = Q[:,k].T*A[:,j]
            A[:,j] = A[:,j]-Q[:,k]*R[k,j]
    
    #Solve system of linear equations and compute residual
    c = Q.T*b
    x = linalg.solve(R[:n,:n],c) 
    rs = linalg.norm(A0*x-b)
    
    return x, rs, Q 


def normalls(A, b):

    #Determine size and check shape requirements of A matrix
    m = A.shape[0]
    n = A.shape[1]
    if n > m:
        print "Error:  A is " + str(A.shape[0]) + "x" + str(A.shape[1]) + ", must have m>=n \n\n"

    #Normal equations algorithm
    G = linalg.cholesky(A.T*A)
    H = linalg.inv(G)
    x = H.T*(H*(A.T*b))

    #Compute Q matrix and residual
    Q = A*H.T
    rs = linalg.norm(A*x-b)  

    return x, rs, Q 
    

def svdls(A, b):

    #Determine size and check shape requirements of A matrix
    m = A.shape[0]
    n = A.shape[1]
    if n > m:
        print "Error:  A is " + str(A.shape[0]) + "x" + str(A.shape[1]) + ", must have m>=n \n\n"

    B, bm, Vt, U = housebidiag(A,b)
    # print "A? = ", U*B*Vt
    
    q = 0
    eps = 1*10**-7
    while q != n:
        for i in range(0,n-1):
            if abs(B[i,i+1]) <= eps*(abs(B[i,i])+abs(B[i,i+1])):
                B[i,i+1] = 0.
        
        p = 0
        q = 0
        for i in range(0,n-1):
            if B[i,i+1] != 0:
                p = i
                break
            else:
                p = n
        for i in range(n-2,-1,-1):
            if B[i,i+1] != 0:
                q = n-i-2
                break
            else:
                q = n
        B22 = B[p:n-q,p:n-q]
        # print "B22 = ", B22
        # print "p = ", p
        # print "q = ", q
        if q < n:
            D = matrix(nonzero(abs(diag(B22))<1*10**-7))
            # print "D = ", D
            if D.any():
                d = D[0,0]
                if d == n-1:
                    # print B
                    for k in range(d-1,-1,-1):
                        y = B[k,k]
                        z = B[k,d]
                        # print "y = ", y
                        # print "z = ", z
                        if z == 0:
                            co = 1
                            si = 0
                        else:
                            if abs(z) > abs(y):
                                t = -y/z
                                si = 1/(1+t**2)**0.5
                                co = si*t
                            else:
                                t = -z/y
                                co = 1/(1+t**2)**0.5
                                si = co*t
                        # print "k = ", k
                        # print "d = ", d
                        # print "test = ", B[:,[k,d]]
                        B[:,[k,d]] = B[:,[k,d]]*matrix([[co,si],[-si,co]])
                        Vt[k:k+2,:] = (matrix([[co,si],[-si,co]])).T*Vt[k:k+2,:]
                        # print "B = ", B
                else:
                    for k in range(d+1,n):
                        y = B[k,k]
                        z = B[d,k]

                        if z == 0:
                            co = 1
                            si = 0
                        else:
                            if abs(z) > abs(y):
                                t = -y/z
                                si = 1/(1+t**2)**0.5
                                co = si*t
                            else:
                                t = -z/y
                                co = 1/(1+t**2)**0.5
                                si = co*t
                        B22[ix_([k,d]),:] = matrix([[co,-si],[si,co]])*B22[ix_([k,d]),:]
                        # print "B22 = ", B22
            else:
                # print "B22 = ", B22
                B[p:n-q,p:n-q], Ustep, Vstep = SVDstep(B22)
                # print "B = ", B
                # print n-q
                # print "Ustep = ", Ustep
                U[:,p:n-q] = U[:,p:n-q]*Ustep
                Vt[p:n-q,:] = Vstep*Vt[p:n-q,:]
    S = diag(B)
    neg = array(nonzero(S<0))
    if neg.any():
        for elem in neg:
            S[elem] = -1*S[elem]
            U[:,elem] = -1*U[:,elem]
        
    Us, s, Vs = linalg.svd(A)
    print "Us = ", Us
    print "U = ", U
    print
    print "s = ", s
    print "S = ", S
    print
    print "Vs = ", Vs
    print "V = ", Vt
    
    print "A  = ", Us[:,:n]*diag(s)*Vs
    print "A? = ", U[:,:n]*diag(S)*Vt
    
    s[s<1*10**-8] = 0
    s = s[:,s.T>1*10**-8]
    r = s.shape[0]
    c = U.T*b
    y = linalg.solve(diag(S),c[0:r])
    x = Vt[:r,:].T*y
    #x, rs, r, s = linalg.lstsq(B*Vt.T,bm)
    rs = linalg.norm(A*x-b)  

    return x, rs, 0

def SVDstep(B):
    T = B.T*B
    n = T.shape[1]
    U = matrix(eye(n))
    V = matrix(eye(n))

    D,R = linalg.eig(T[n-2:,n-2:])
    mu =  max(D-T[-1,-1])
    y = T[0,0]-mu
    z = T[0,1]
    # print "y = ", y
    # print "z = ", z
        
    for k in range(0,n-1):
        if z == 0:
            co = 1
            si = 0
        else:
            if abs(z) > abs(y):
                t = -y/z
                si = 1/(1+t**2)**0.5
                co = si*t
            else:
                t = -z/y
                co = 1/(1+t**2)**0.5
                si = co*t
        B[:,k:k+2] = B[:,k:k+2]*matrix([[co,si],[-si,co]])
        V[k:k+2,:] = (matrix([[co,si],[-si,co]])).T*V[k:k+2,:]
        y = B[k,k]
        z = B[k+1,k]
        if z == 0:
            co = 1
            si = 0
        else:
            if abs(z) > abs(y):
                t = -y/z
                si = 1/(1+t**2)**0.5
                co = si*t
            else:
                t = -z/y
                co = 1/(1+t**2)**0.5
                si = co*t
        B[k:k+2,:] = matrix([[co,-si],[si,co]])*B[k:k+2,:]    
        U[:,k:k+2] = U[:,k:k+2]*(matrix([[co,-si],[si,co]])).T
        # print "Btest = ", B
        if k < n-2:
            y = B[k,k+1]
            z = B[k,k+2]
    return B, U, V
        
        
def housebidiag(A,b):
    A = A.copy()
    b = b.copy()
    m = A.shape[0]
    n = A.shape[1]
    U = matrix(eye(m))
    I = matrix(eye(m))
    V = matrix(eye(n))
    
    for j in range(0,n):
        v, beta = house(A[j:m,j])
        A[j:,j:] = (matrix(eye(m-j))-beta*v*v.T)*A[j:,j:]
        U = U*(vstack((hstack((I[:j,:j],zeros((j,m-j)))),hstack((zeros((m-j,j)),(matrix(eye(m-j))-beta*v*v.T)))))).T
        b[j:m] = (matrix(eye(m-j))-beta*v*v.T).T*b[j:m]
        if j <= n-2:
            v, beta = house(A[j,j+1:].T)
            A[j:,j+1:] = A[j:,j+1:]*(matrix(eye(n-j-1))-beta*v*v.T)
            V = (vstack((hstack((I[:j+1,:j+1],zeros((j+1,n-j-1)))),hstack((zeros((n-j-1,j+1)),(matrix(eye(n-j-1))-beta*v*v.T)))))).T*V
    return A, b, V, U
            
def house(x):
    n = x.shape[0]
    sigma = x[1:n].T*x[1:n]
    v = vstack((matrix([1]),x[1:n,0]))
    if sigma == 0:
        beta = 0
    else:
        mu = (x[0,0]**2+sigma[0,0])**0.5
        if x[0,0] <= 0:
            v[0,0] = x[0,0] - mu
        else:
            v[0,0] = -sigma/(x[0,0]+mu)
        beta = 2*v[0,0]**2/(sigma[0,0]+v[0,0]**2)
        v = v/v[0,0]
    return v, beta


        
def svdls2(A, b):

    #Determine size and check shape requirements of A matrix
    m = A.shape[0]
    n = A.shape[1]
    if n > m:
        print "Error:  A is " + str(A.shape[0]) + "x" + str(A.shape[1]) + ", must have m>=n \n\n"

    U, s, V = linalg.svd(A)
    s[s<1*10**-8] = 0
    s = s[:,s.T>1*10**-8]
    r = s.shape[0]
    c = U.T*b
    y = linalg.solve(diag(s),c[0:r])
    x = V[:r,:].T*y
    rs = linalg.norm(A*x-b)  

    return x, rs, 0