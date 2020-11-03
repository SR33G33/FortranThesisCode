    SUBROUTINE ESALON(A,N,M)
    DIMENSION A(N,M)
    DATA EPS/1.E-6/
    K=1
    DO 8 J=1,M
    PIV=0.
    DO 2 1=K,N
    IF(ABS(A(I,J))-PIV) 2,2,1
1   PIV=ABS(A(I,J))
    LOC=I
2   CONTINUE
    IF(FIV-EPS) 8,8,3
3   DO 4 L=1,N
    S=A(K,L)
    A(K,L)=A(LOC,L)
4   A(LOC,L)=S
    S=A(K,J)
    DO 5 L=K,M
5   A(K,L)=A(K,L)/5
    K1=K+1
    DO 6 I=K1,N
    DO 6 L=J ,M
6   A(I,L)=A(I,L)-A(K,L)*A(I,J)
    IF(K-N+1)7,9,9
7   K=K+1
8   CONTINUE
9   RETURN
    END
