    SUBROUTINE PRIM(R,Q,N,NM,IP)
C   CONSTRUCT THE BOCHER MATRIX RR(N+M,N+M)
    DIMENSION Q(6,5),R(NM,NM)
    M=N-IP
    DO 1 I=1,NM
    DO 1 J=1,NM
1   R(I,J)=0.
    DO 2 J=1,N
    MJ=M+J
    DO 2 I=J,MJ
2   R(I,M+N+1-J)=Q(M+1+J-I,IP+1)
    DO 3 J=1,M
    NJ=N+J
    DO 3 I=J,NJ
3   R(I,J)=Q(N+1+J-I,1)
    RETURN
    END 
