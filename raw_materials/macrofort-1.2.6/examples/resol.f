      SUBROUTINE RESOL (A,B,N)
      REAL A(N,*),B(*)
      DO 1, I=2,N
        IF(ABS(A(I-1,I-1)).LT.1.E-30) THEN
          WRITE(6,100)
          STOP
        ENDIF
        DO 1, II=I,N
          C=A(II,I-1)/A(I-1,I-1)
          B(II)=B(II)-B(I-1)*C
          DO 1, J=I-1,N
            A(II,J)=A(II,J)-A(I-1,J)*C
1     CONTINUE
      IF(ABS(A(N,N)).LT.1.E-30) THEN
        WRITE(6,100)
        STOP
      ENDIF
      B(N)=B(N)/A(N,N)
      DO 2, I=2,N
        II=N-I+1
        S=0.
        DO 3, J=II+1,N
          S=S+A(II,J)*B(J)
3       CONTINUE
        B(II)=(B(II)-S)/A(II,II)
2     CONTINUE
100   FORMAT (2X,"ZERO DETERMINANT IN GAUSS METHOD")
      END

