    SUBROUTINE CONOBS(IV,V,W,AA,N)
    DIMENSION W(5,15),V(N,IV),AA(N,N)
    DO 1 I=1,N
    DO 1 J=1,IV
1   W(I,J)=V(I,J)
    N2=N-1
    DO 2 IQ=1,N2
    DO 2 I=1,N
    DO 2 J=1,IV
    S=O.
    DO 3 K=1,N
3   S=S+AA(I,K)*W(K,J+(IQ-1)*IV)
2   W(I,J+IQ*IV)=S
    RETURN
    END
