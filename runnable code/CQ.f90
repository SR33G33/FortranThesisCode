    SUBROUTINE CQ(Q,N,AL)
    DIMENSION Q(6, 5), AL (N)
    Q(N+1,1)=1.
    DO 1 I=1,N
1	  Q(I,1)=AL(I)
    DO 2 K=2,N
    I1=N+2-K
    DO 2 I=1,I1
2	  Q(I,K)=I*Q(I+1, K-1)
    RETURN
    END
