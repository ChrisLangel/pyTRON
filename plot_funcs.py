#!/usr/bin/env python



def stretch_norm(X,Y,stretch):
    lj = len(X[:,0])
    lk = len(X[0,:])
    Xstr = [[0.5 for i in range(lk)] for j in range(lj) ]
    Ystr = [[0.5 for i in range(lk)] for j in range(lj) ]
    for k in range(0,lk):
        for j in range(0,lj):
            xa = X[j][0]
            ya = Y[j][0]
            xb = X[j][k]
            yb = Y[j][k] 
            vax = xb - xa
            vay = yb - ya
            Xstr[j][k] = xb + stretch*vax
            Ystr[j][k] = yb + stretch*vay
    return Xstr,Ystr
